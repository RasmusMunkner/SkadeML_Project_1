##### 1. Load packages and data ######
library(dplyr)
#install.packages(c("xts", "sp", "zoo"))
#install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
#install.packages('gmodels')
library(gmodels)
library(CASdatasets)
?CASdatasets

#Load the data into memory
data("freMPL1")
freMPL1Claim <- subset(freMPL1, ClaimInd==1)
summary(freMPL1Claim$ClaimAmount) #No zero claims


##### Exploratory Analysis - Freq model ######
#Change "SocioCateg" to other variables.
#For continuous vairbales, write floor(Var/100) or floor(Var/20) for grouping
freMPL1 %>%
  group_by(SocioCateg) %>%
  summarise(AntSka = sum(ClaimInd), AntFaar=sum(Exposure), Frek = sum(ClaimInd)/sum(Exposure)) %>%
  print(n=100)

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
  group_by(floor(BonusMalus/10)) %>%
  summarise(AntSka = sum(ClaimInd), AntFaar=sum(Exposure), Median = median(ClaimAmount), Middel = mean(ClaimAmount)) %>%
  print(n=100)

#The following vairbles have the biggest impact on the emprirical claim size.
freMPL1$LicAgeInd <- (freMPL1$LicAge<60)
#MariStat
freMPL1$BonusMalusG <- cut(freMPL1$BonusMalus, breaks=c(49,60,80,272))
freMPL1$VehBodyInd2 <- (freMPL1$VehBody == "station wagon")
freMPL1$VehClassG <- (freMPL1$VehClass %in% c(0, "H"))
freMPL1$VehClassG <- (freMPL1$VehClass %in% c("H"))


##### Split #####
set.seed(1)
frempl1_split <- initial_split(freMPL1, prop = .7)
frempl1_train <- training(frempl1_split)
frempl1_test  <- testing(frempl1_split)
