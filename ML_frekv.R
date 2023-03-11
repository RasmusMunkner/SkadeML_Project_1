library(mlr3verse)
library(mlr3mbo)
library(mlr3extralearners)
library(tidyverse)
library(rgl)
source("Rasmus_Funktioner.R") #For the ReadData-function

#Read in the data for the frequency model, filter out ObsFreq and convert to a MLR3 task.

#Nu gør jeg præcist det, som jeg har rettet Albert for... Men det er smart, fordi jeg skal bruge det samme data flere gange...
BaseData <- ReadData("freq_df") %>% 
  dplyr::select(-ObsFreq) %>% 
  slice(sample(1:n())) #Shuffling the data set just to be sure CV folds are somewhat equally distributed

task_mod1 <- BaseData %>% 
  as_task_classif(target = "ClaimInd", measure = "classif.bbrier")

train_index <- 1:floor((nrow(BaseData)*2/3))
test_index <- (floor((nrow(BaseData)*2/3))+1):nrow(BaseData)

task_mod1_train <- BaseData %>% 
  slice(train_index) %>%
  as_task_classif(target = "ClaimInd")

data_mod1_test <- BaseData %>% 
  slice(test_index)


#Learners
my_ranger_learner = lrn("classif.ranger",
                        mtry = to_tune(1,8), #Seems to matter, but not super clearly
                        min.node.size = 150,
                        num.trees = to_tune(50, 300), #Seems to be completely irrelevant
                        max.depth = to_tune(2,24) #Something magical happens around 9 or 10, which may be where overfitting begins
                        ,predict_type= "prob"
                        )

#May not be best anymore
my_current_best_ranger <- lrn("classif.ranger",
                              mtry = 5, #Seems to matter, but not super clearly
                              min.node.size = 150,
                              num.trees = 158, #Seems to be completely irrelevant
                              max.depth = 12 #Something magical happens around 9 or 10, which may be where overfitting begins
                              ,predict_type= "prob"
                              )

My_long_ranger <- lrn("classif.ranger",
                      mtry = 8, #Seems to matter, but not super clearly
                      min.node.size = 150,
                      num.trees = 71, #Seems to be completely irrelevant
                      max.depth = 23 #Something magical happens around 9 or 10, which may be where overfitting begins
                      ,predict_type= "prob"
)

my_ranger_learner_autotuner <- auto_tuner(
  method = tnr("random_search"),
  learner = my_ranger_learner,
  resampling = rsmp("cv", folds = 8),
  measure = msr("classif.bbrier"),
  terminator = trm("evals", n_evals = 60) #At least 20 evals should be needed - Probably way more for the final evaluation. Note that it is theoretically possible with too many (i think)
)

outer_resampling = rsmp("cv", folds = 5)

#nestedcvResults <- resample(task_mod1, my_ranger_learner_autotuner, outer_resampling, store_models = T)

#Results from each fold from the outer resampling
nestedcvResults %>% 
  extract_inner_tuning_results() %>% 
  `[`(,list(iteration, max.depth, num.trees)) %>% 
  as_tibble() %>% 
  inner_join(nestedcvResults$score(msr("classif.bbrier"))[,list(iteration, classif.bbrier)] %>% as_tibble(), by = "iteration")

test <- nestedcvResults %>% extract_inner_tuning_archives()

test %>% `[`(,list(iteration, max.depth, num.trees, classif.bbrier)) %>% as_tibble() %>% 
  ggplot(aes(x = num.trees, y = classif.bbrier, color = max.depth)) +
  geom_point()

test %>% `[`(,list(iteration, max.depth, num.trees, classif.bbrier)) %>% as_tibble() %>% 
  ggplot(aes(x = max.depth, y = classif.bbrier, color = iteration %>% as.factor())) +
  geom_point()

pmap_dbl(.l = list(mtry = test$mtry, num.trees = test$num.trees, max.depth=test$max.depth),
     .f = function(mtry, num.trees, max.depth){
       forest <- lrn("classif.ranger",
                     mtry = mtry, #Seems to matter, but not super clearly
                     min.node.size = 150,
                     num.trees = num.trees, #Seems to be completely irrelevant
                     max.depth = max.depth #Something magical happens around 9 or 10, which may be where overfitting begins
                     ,predict_type= "prob"
       )
       forest$train(task_mod1_train)
       forest$predict_newdata(data_mod1_test)$score(msr("classif.bbrier"))
     })

ContenderModels <- .Last.value

ggplot(mapping = aes(x = test$max.depth, y = ContenderModels, color = test$mtry %>% as.factor())) +
  geom_point() +
  labs(x = "Max Depth", y = "Out of sample error (Brier Score)", color = "Mtry")

test %>%
  as_tibble() %>%
  filter(max.depth == 2) %>% 
  filter(classif.bbrier == min(classif.bbrier))

#CV estimate for generalization error
nestedcvResults$aggregate(msr("classif.bbrier"))

#Train and test in holdout scenario
my_ranger_learner_autotuner$train(task_mod1_train)
my_ranger_learner_autotuner$predict_newdata(data_mod1_test)$score(msr("classif.bbrier"))
my_ranger_learner_autotuner$learner$param_set

#Compare to the low depth best learner
my_current_best_ranger$train(task_mod1_train)
my_current_best_ranger$predict_newdata(data_mod1_test)$score(msr("classif.bbrier"))

#Compare to a featureless learner
lrn_featureless <- lrn("classif.featureless", predict_type = "prob")
lrn_featureless$train(task_mod1)
lrn_featureless$predict(task_mod1)$score(msr("classif.bbrier"))

#Visualisation

pred <- task_mod1 %>% 
  {cbind(`$`(., data)() %>% as_tibble(),
         auto_pred = my_ranger_learner_autotuner$predict(.)$prob[,2])}

plot3d(x = pred$Exposure, y = pred$LicAge, z = pred$auto_pred, col = pred$BonusMalus)
plot3d(x = pred$LicAge, y = pred$DrivAge, z = pred$auto_pred)

#Summarise results

newdata <- ReadData("freq_df")
newdata <- data_mod1_test

results <- newdata %>%
  add_column(Pred_freq_at = my_ranger_learner_autotuner$predict_newdata(.)$prob[,2],
             Pred_freq_best = my_current_best_ranger$predict_newdata(.)$prob[,2])

results %>% 
  ggplot(aes(x = Exposure, y = Pred_freq_at)) +
  geom_point(aes(color = ClaimInd)) +
  geom_smooth(method = "gam") +
  geom_smooth(method = "lm", color = "red")

results %>% 
  ggplot(aes(x = Exposure, y = Pred_freq_best)) +
  geom_point(aes(color = ClaimInd)) +
  geom_smooth(method = "gam") +
  geom_smooth(method = "lm", color = "red")

results %>% 
  group_by(ClaimInd) %>% 
  summarise(observedFreq = mean(ClaimInd %>% as.character() %>% as.numeric()), estFreq = mean(Pred_freq_at))

#Permutation tests
task_mod1_permutation <- 
  ReadData("freq_df") %>%
  select(-ObsFreq) %>% 
  {cbind(select(.,-ClaimInd), select(.,ClaimInd) %>% slice(sample(1:n())))} %>% #This looks weird because magrittr converts . %>%  ... to a function.
  as_task_classif(target = "ClaimInd")

PermutationTest <- function(learner, data, target, permutations = 3, seed = 0){
  set.seed(seed)
    rnorm(permutations) %>% 
    map(.f = function(seed){
      set.seed(seed)
      data %>% 
        {cbind(select(.,-target), select(.,target) %>% slice(sample(1:n())))} %>% 
        as_task_regr(target = target)
    }) %>% 
      map_dbl(.f = function(task){
        set.seed(NULL)
        learner$train(task)
        learner$predict(task)$score()
      }) %>% 
      return()
}


rflearner <- lrn("regr.ranger",
                 mtry = 6, #Seems to matter, but not super clearly
                 min.node.size = 1,
                 num.trees = 85, #Seems to be completely irrelevant
                 max.depth = 16 #Something magical happens around 9 or 10, which may be where overfitting begins
                 #,predict_type= "prob"
)

y <- PermutationTest(rflearner,
                     ReadData("freq_df") %>% select(-ObsFreq) %>% mutate(ClaimInd = ClaimInd %>% as.character() %>% as.numeric()),
                     "ClaimInd",
                     permutations = 40)

rflearner$train(task_mod1)
rflearner$predict(task_mod1)$score()


