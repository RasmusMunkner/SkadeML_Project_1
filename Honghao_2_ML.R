##### 1. Load packages and data ######
library(dplyr)

#Installing packages
#install.packages(c("xts", "sp", "zoo"))
#install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
#install.packages('gmodels')
library(gmodels)
library(beepr)
#install.packages(c("rsample","randomForest", "ranger", "caret", "h2o"))
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform
#Load the package containing the dataset
library(CASdatasets)
?CASdatasets

library(tidyr)     # contains tools to tidy data
library(ggplot2) # for plotting
library(broom)
library(h2o)
library(bit64)


#Load the data into memory
data("freMPL1")

##### 2. Data manipulation #####
#So our ML model has the same information as the GLM models from Part I.
freMPL1$LicAgeInd <- (freMPL1$LicAge<60)
freMPL1$SocioCategInd <- (freMPL1$SocioCateg %in% c("CSP26", "CSP66"))
freMPL1$VehUsageInd <- (freMPL1$VehUsage %in% c("Professional", "Professional run"))
freMPL1$VehBodyInd <- (freMPL1$VehBody %in% c("cabriolet", "microvan", "van"))
freMPL1$BonusMalusInd <- (freMPL1$BonusMalus < 100)
freMPL1$VehEnergyInd <- (freMPL1$VehEnergy == "diesel")
freMPL1$BonusMalusG <- cut(freMPL1$BonusMalus, breaks=c(49,60,80,272))
freMPL1$VehBodyInd2 <- (freMPL1$VehBody == "station wagon")
freMPL1$VehClassG <- (freMPL1$VehClass %in% c("H"))
freMPL1$Claim <- freMPL1$ClaimAmount * freMPL1$ClaimInd

tree <- freMPL1[c("Exposure", "LicAgeInd", "SocioCategInd", "VehUsageInd", 
                  "VehBodyInd", "HasKmLimit", "BonusMalusInd", "VehEnergyInd",
                  "BonusMalusG", "VehBodyInd2", "VehClassG", "Claim")]

set.seed(1)
frempl1_split <- initial_split(tree, prop = .7)
frempl1_train <- training(frempl1_split)
frempl1_test  <- testing(frempl1_split)

##### 3. First ML model using randomForest()
m1 <- randomForest(
  formula = Claim ~ .,
  data    = frempl1_train
)
plot(m1)

which.min(m1$mse)
# RMSE of this optimal random forest
sqrt(m1$mse[which.min(m1$mse)])

pred <- predict(m1, frempl1_test)
summary(pred)
sqrt(mean((pred-frempl1_test$Claim)^2))

#ML trees first model: 
# sqrt(MSE)=2292.951



##### 3. hyperparameter search (tuner) #####
# Man kan bruge tuner til at finde endnu bedre modeller. 
# Jeg har ikke haft tid til at rydde op, fordi kode tager for lang tid at køre.
features <- setdiff(names(frempl1_train), "Claim")

# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry       = seq(2, 6, by = 2),
  node_size  = seq(2, 6, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)
nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula         = Claim ~ ., 
    data            = frempl1_train, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}
beep(3)

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)


OOB_RMSE <- vector(mode = "numeric", length = 100)

for(i in seq_along(OOB_RMSE)) {
  
  optimal_ranger <- ranger(
    formula         = Claim ~ ., 
    data            = frempl1_train, 
    num.trees       = 500,
    mtry            = 6,
    min.node.size   = 2,
    sample.fraction = .8,
    importance      = 'impurity'
  )
  
  OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}

hist(OOB_RMSE, breaks = 20)

optimal_ranger$variable.importance %>% 
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(10) %>%
  ggplot(aes(reorder(names, x), x)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 10 important variables")



##### 4. h2o #####
#install.packages("h2o")
h2o.shutdown()
h2o.no_progress()
h2o.init(max_mem_size = "5g")

# create feature names
y <- "Claim"
x <- setdiff(names(frempl1_train), "Claim")

# turn training set into h2o object
train.h2o <- as.h2o(frempl1_train)

# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = seq(300, 500, by = 100),
  mtries      = seq(2, 6, by = 2),
  sample_rate = c(.55, .632, .70, .80)
)


# build grid search 
grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid2",
  x = x, 
  y = y, 
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = list(strategy = "Cartesian")
)

# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "rf_grid2", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf)

# Grab the model_id for the top model, chosen by validation error
best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

# Now let’s evaluate the model performance on a test set
frempl1_test.h2o <- as.h2o(frempl1_test)
best_model_perf <- h2o.performance(model = best_model, newdata = frempl1_test.h2o)

# RMSE of best model
h2o.mse(best_model_perf) %>% sqrt()
## [1] 2544.667


# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = seq(300, 500, by = 200),
  mtries      = seq(4, 8, by = 2),
  max_depth   = seq(20, 40, by = 10),
  min_rows    = seq(2, 4, by = 2),
  nbins       = seq(15, 25, by = 10),
  sample_rate = c(.7, .8)
)
# random grid search criteria
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.005,
  stopping_rounds = 10,
  max_runtime_secs = 15*60
)
# build grid search 
random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid3",
  x = x, 
  y = y, 
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = search_criteria
)
#beep(3)
# collect the results and sort by our model performance metric of choice
grid_perf2 <- h2o.getGrid(
  grid_id = "rf_grid3", 
  sort_by = "mse", 
  decreasing = FALSE
)
print(grid_perf2)


# Grab the model_id for the top model, chosen by validation error
best_model_id <- grid_perf2@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

# Now let’s evaluate the model performance on a test set
frempl1_test.h2o <- as.h2o(frempl1_test)
best_model_perf <- h2o.performance(model = best_model, newdata = frempl1_test.h2o)

# RMSE of best model
h2o.mse(best_model_perf) %>% sqrt()
## [1] 2754.289
