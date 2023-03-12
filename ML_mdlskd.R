library(mlr3verse)
library(mlr3benchmark)
library(mlr3mbo)
library(mlr3extralearners)
library(mlr3tuningspaces)
library(tidyverse)
library(rgl)
library(ranger)
library(glmnet)
source("Rasmus_Funktioner.R") #For the ReadData-function

#Read in the data for the frequency model, filter out ObsFreq and convert to a MLR3 task.

#Nu gør jeg præcist det, som jeg har rettet Albert for... Men det er smart, fordi jeg skal bruge det samme data flere gange...
BaseData <- ReadData("claimsize_df") %>% 
  dplyr::slice(sample(1:n())) #Shuffling the data set just to be sure CV folds are somewhat equally distributed

task_mod1 <- BaseData %>% 
  as_task_regr(target = "ClaimAmount")

train_index <- 1:floor((nrow(BaseData)*2/3))
test_index <- (floor((nrow(BaseData)*2/3))+1):nrow(BaseData)

task_mod1_train <- BaseData %>% 
  dplyr::slice(train_index) %>%
  as_task_regr(target = "ClaimAmount")

data_mod1_test <- BaseData %>% 
  dplyr::slice(test_index)

#####################################################################
#Learners
#####################################################################

inner_tuner <- tnr("random_search")
inner_resampling <- rsmp("cv", folds = 5)
inner_terminator <- trm("evals", n_evals = 30)
inner_measure <- msr("regr.mse")

lrn_ranger_tmp = lrn("regr.ranger",
                        mtry.ratio = to_tune(0,0.5), #Seems to matter, but not super clearly
                        min.node.size = 50,
                        num.trees = to_tune(50, 200), #Seems to be completely irrelevant
                        max.depth = to_tune(2,36))

lrn_ranger_auto <- auto_tuner(
  method = inner_tuner,
  learner = lrn_ranger_tmp,
  resampling = inner_resampling,
  measure = inner_measure,
  terminator = inner_terminator
)

lrn_glmnet_tmp <- lts(lrn("regr.glmnet"),
                      s= to_tune(0, 1),
                      alpha=to_tune(0, 1))

lrn_glmnet_auto <- auto_tuner(
  method = inner_tuner,
  learner = lrn_glmnet_tmp,
  resampling = inner_resampling,
  measure = inner_measure,
  terminator = inner_terminator
)

lrn_baseline <- lrn("regr.featureless")

lrn_lm_tmp <- lts(lrn("regr.lm"))

lrn_lm_auto <- auto_tuner(
  method = inner_tuner,
  learner = lrn_lm_tmp,
  resampling = inner_resampling,
  measure = inner_measure,
  terminator = inner_terminator
)

lrn_knn_tmp <- lts(lrn("regr.kknn"))

lrn_knn_auto <- auto_tuner(
  method = inner_tuner,
  learner = lrn_knn_tmp,
  resampling = inner_resampling,
  measure = inner_measure,
  terminator = inner_terminator
)


#May not be best anymore
# my_current_best_ranger <- lrn("classif.ranger",
#                               mtry = 5, #Seems to matter, but not super clearly
#                               min.node.size = 150,
#                               num.trees = 158, #Seems to be completely irrelevant
#                               max.depth = 12 #Something magical happens around 9 or 10, which may be where overfitting begins
#                               ,predict_type= "prob"
#                               )
# 
# My_long_ranger <- lrn("classif.ranger",
#                       mtry = 8, #Seems to matter, but not super clearly
#                       min.node.size = 150,
#                       num.trees = 71, #Seems to be completely irrelevant
#                       max.depth = 23 #Something magical happens around 9 or 10, which may be where overfitting begins
#                       ,predict_type= "prob"
# )

#####################################################################
#Benchmark
#####################################################################

parallel::detectCores() #Check the number of cores available on your machine, consider adjusting the number of outer folds to be a multiple of this number

benchmark_design <- benchmark_grid(task_mod1,
                           list(rf = lrn_ranger_auto, glmnet = lrn_glmnet_auto, logreg = lrn_logreg, baseline = lrn_baseline),
                           rsmp("cv", folds = 8))

future::plan("multisession") #Enables parallel computation
benchmark_result <- benchmark_design %>% benchmark(store_models = T)
future::plan("sequential") #Disables parallel computation

#Ordinary nested-CV error
benchmark_result$aggregate(msr("classif.bbrier"))

#Quantile-based error - Instead of performing an outer average, we consider quantiles.
#This is more representative for the model performance in the bad cases
benchmark_result$score(msr("classif.bbrier")) %>% 
  {tibble(learner = .$learner_id, OOSE = .$classif.bbrier)} %>% 
  group_by(learner) %>% 
  summarise(OOSE_quant = quantile(OOSE, 0.75))

#####################################################################
#Graphs
#####################################################################

#Train and test in holdout scenario to have something to plot
lrn_ranger_auto$train(task_mod1_train)
lrn_ranger_auto$predict(task_mod1)$score(msr("classif.bbrier"))
lrn_ranger_auto$predict_newdata(data_mod1_test)$score(msr("classif.bbrier"))
lrn_ranger_auto$learner$param_set

#3d - Only for heuristics
pred <- task_mod1 %>% 
  {cbind(`$`(., data)() %>% as_tibble(),
         auto_pred = lrn_ranger_auto$predict(.)$prob[,2])}

plot3d(x = pred$Exposure, y = pred$LicAge, z = pred$auto_pred, col = pred$BonusMalus)
plot3d(x = pred$LicAge, y = pred$DrivAge, z = pred$auto_pred)

#2d
results <- BaseData %>%
  slice(train_index) %>% 
  add_column(Pred_freq_at = lrn_ranger_auto$predict_newdata(.)$prob[,2]) #DONT USE THIS - It is for inspecting the performance on the training set
results <- data_mod1_test %>%
  add_column(Pred_freq_at = lrn_ranger_auto$predict_newdata(.)$prob[,2]) #Results on the test set

#Inspect how the estimated frequencies compare to the observed frequencies by factor level
results %>%
  pivot_longer(cols = c(VehUsage, VehEngine, VehEnergy, VehMaxSpeed, VehClass, VehAge, VehBody, VehPrice),
               names_to = "factor",
               values_to = "level") %>% 
  group_by(factor, level) %>% 
  mutate(ClaimInd = ClaimInd %>% as.character() %>% as.numeric()) %>% 
  summarise(freq = mean(ClaimInd), err = sd(ClaimInd) / sqrt(n()) * 1.96, pred_freq = mean(Pred_freq_at)) %>% 
  ggplot(aes(x = level)) +
  geom_point(aes(y = freq)) +
  geom_point(aes(y = pred_freq), color = "red") +
  geom_errorbar(aes(ymin = freq - err, ymax = freq + err)) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(~factor, scales ="free_x")

#The classical exposure plot. Gives a decent indication of whether or not the model has managed to separate the data in an overfitting manner
results %>% 
  ggplot(aes(x = Exposure, y = Pred_freq_at)) +
  geom_point(aes(color = ClaimInd)) +
  geom_smooth(method = "gam") +
  geom_smooth(method = "lm", color = "red")

#Compare fitted frequencies for observations that have claims versus fitted frequencies for observations that do not have claims
results %>% 
  group_by(ClaimInd) %>% 
  summarise(observedFreq = mean(ClaimInd %>% as.character() %>% as.numeric()), estFreq = mean(Pred_freq_at))

#####################################################################
#Permutation tests - Somewhat unclear what we need this for
#####################################################################
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


