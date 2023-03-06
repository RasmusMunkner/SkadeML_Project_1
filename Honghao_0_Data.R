##### 1. Load packages and data ######
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(mlr3verse)
source("Rasmus_Funktioner.R")
#install.packages(c("xts", "sp", "zoo"))
#install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
#install.packages('gmodels')
library(gmodels)
library(CASdatasets)
library(rsample)
?CASdatasets

#Load the data into memory
data("freMPL1")
freMPL1Claim <- subset(freMPL1, ClaimInd==1)
summary(freMPL1Claim$ClaimAmount) #No zero claims

#Recode SocioCateg such that the factor levels are monotonically ordered
freMPL1 <- freMPL1 %>% 
  mutate(SocioCateg = SocioCateg %>%
           as.character() %>% 
           map_chr(.f = substr, start = 4, stop = 999) %>% 
           map_dbl(.f = as.numeric) %>% 
           factor(levels = 1:100))

##### Exploratory Analysis - Freq model ######
#Change "SocioCateg" to other variables.
#For continuous vairbales, write floor(Var/100) or floor(Var/20) for grouping
freMPL1 %>%
  group_by(SocioCateg) %>%
  summarise(AntSka = sum(ClaimInd), AntFaar=sum(Exposure), Frek = sum(ClaimInd)/sum(Exposure)) %>%
  #arrange(-AntFaar) %>% 
  print(n=100)

freMPL1 %>% 
  TreeModelGrouping(.feature = "SocioCateg", .target = "ClaimInd") %>% 
  ggplot(aes(x = SocioCateg)) +
  geom_point(aes(y = Pred))

freMPL1 %>% 
  TreeModelGrouping(.feature = "SocioCateg", .target = "ClaimInd") %>% 
  group_by(SocioCateg) %>% 
  summarise(n = n())

freMPL1 %>% 
  TreeModelGrouping(.feature = "SocioCateg", .target = "ClaimInd", featureName = "SocioCategG") %>%
  ggplot(aes(x = SocioCateg, y = Pred)) +
  geom_point()

##The following vairbles have the biggest impact on the empirical frequency.
freMPL1$LicAgeInd <- (freMPL1$LicAge<60)
freMPL1$SocioCategInd <- (freMPL1$SocioCateg %in% c("CSP26", "CSP66"))
freMPL1$VehUsageInd <- (freMPL1$VehUsage %in% c("Professional", "Professional run"))
freMPL1$VehBodyInd <- (freMPL1$VehBody %in% c("cabriolet", "microvan", "van"))
freMPL1$DrivAgeInd <- (freMPL1$DrivAge<25)
#HasKmLimit
freMPL1$BonusMalusInd <- (freMPL1$BonusMalus < 100)
freMPL1$VehEngineInd <- (freMPL1$VehEngine == "direct injection overpowered")
freMPL1$VehEnergyInd <- (freMPL1$VehEnergy == "diesel")


##### Exploratory Analysis - Claim model ######
freMPL1Claim %>%
  group_by(VehClass) %>%
  summarise(AntSka = sum(ClaimInd), AntFaar=sum(Exposure), Median = median(ClaimAmount), Middel = mean(ClaimAmount)) %>%
  print(n=100)

#The following vairbles have the biggest impact on the emprirical claim size.
freMPL1$LicAgeInd <- (freMPL1$LicAge<60)
#MariStat
freMPL1$BonusMalusG <- cut(freMPL1$BonusMalus, breaks=c(49,60,80,272))
freMPL1$VehBodyInd2 <- (freMPL1$VehBody == "station wagon")
freMPL1$VehClassG <- (freMPL1$VehClass %in% c("H"))
freMPL1$VehPriceG <- (freMPL1$VehPrice %in% c("Z", "Y", "X", "W", "V", "U", "T", "S", "R", "Q"))



freMPL1_final <- freMPL1[c("LicAgeInd", "SocioCategInd", "VehUsageInd", "VehBodyInd",
                          "DrivAgeInd", "HasKmLimit", "BonusMalusInd", "VehEngineInd", 
                          "VehEnergyInd", "MariStat", "BonusMalusG", "VehBodyInd2", 
                          "VehClassG", "VehPriceG", "ClaimInd", "ClaimAmount")]

##### Split #####
set.seed(1)
frempl1_split <- initial_split(freMPL1_final, prop = .7)
frempl1_train <- training(frempl1_split)
frempl1_test  <- testing(frempl1_split)
