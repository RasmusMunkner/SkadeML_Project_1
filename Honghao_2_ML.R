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

#Load the data into memory
data("freMPL1")

freMPL1$LicAgeInd <- (freMPL1$LicAge<60)
freMPL1$SocioCategInd <- (freMPL1$SocioCateg %in% c("CSP26", "CSP66"))
freMPL1$VehUsageInd <- (freMPL1$VehUsage %in% c("Professional", "Professional run"))
freMPL1$VehBodyInd <- (freMPL1$VehBody %in% c("cabriolet", "microvan", "van"))
#HasKmLimit
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
tree$X <- rbinom(30595,1,8/18)

treetrain <- subset(tree,X=1)
treetest <- subset(tree,X=0)

m1 <- randomForest(
  formula = Claim ~ .,
  data    = treetrain
)
beep(3)

plot(m1)

which.min(m1$mse)
# RMSE of this optimal random forest
sqrt(m1$mse[which.min(m1$mse)])


pred <- predict(m1, treetest)
sqrt(mean((pred-treetest$Claim)^2))




