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


#
freMPL1 %>%
  group_by(VehUsage) %>%
  summarise(AntSka = sum(ClaimInd), AntFaar=sum(Exposure), Frek = sum(ClaimInd)/sum(Exposure)) %>%
  print(n=100)

#
freMPL1Claim %>%
  group_by(MariStat) %>%
  summarise(AntSka = sum(ClaimInd), AntFaar=sum(Exposure), Median = median(ClaimAmount), Middel = mean(ClaimAmount)) %>%
  print(n=100)


#
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

#tree <- freMPL1[c("Exposure", "LicAgeInd", "SocioCategInd", "VehUsageInd", 
#                  "VehBodyInd", "HasKmLimit", "BonusMalusInd", "VehEnergyInd",
#                  "BonusMalusG", "VehBodyInd2", "VehClassG", "Claim")]



