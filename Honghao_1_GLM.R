##### 1. Load packages and data ######
library(dplyr)

#Installing packages
#install.packages(c("xts", "sp", "zoo"))
#install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
#install.packages('gmodels')
library(gmodels)

#Load the package containing the dataset
library(CASdatasets)
?CASdatasets

#Load the data into memory
data("freMPL1")



##### 2. Exploratory Analysis - Freq model ######
freMPL1 %>%
  group_by(LicAge<60) %>%
  summarise(AntSka = sum(ClaimInd), AntFaar=sum(Exposure), Freq = sum(ClaimInd)/sum(Exposure)) %>%
  print(n=100)

freMPL1 %>%
  group_by((freMPL1$SocioCateg %in% c("CSP26", "CSP66"))) %>%
  summarise(AntSka = sum(ClaimInd), AntFaar=sum(Exposure), Freq = sum(ClaimInd)/sum(Exposure)) %>%
  print(n=100)

freMPL1 %>%
  group_by(VehUsage %in% c("Professional", "Professional run")) %>%
  summarise(AntSka = sum(ClaimInd), AntFaar=sum(Exposure), Freq = sum(ClaimInd)/sum(Exposure)) %>%
  print(n=100)


##### 3. Exploratory Analysis - Claim size model #####
freMPL1Claim <- subset(freMPL1, ClaimInd==1)
freMPL1Claim %>%
  group_by(VehPrice %in% c("Z", "Y", "X", "W", "V", "U", "T", "S", "R", "Q")) %>%
  summarise(AntSka = sum(ClaimInd), AntFaar=sum(Exposure), Median = median(ClaimAmount), Middel = mean(ClaimAmount)) %>%
  print(n=100)

freMPL1Claim %>%
  group_by(MariStat) %>%
  summarise(AntSka = sum(ClaimInd), AntFaar=sum(Exposure), Median = median(ClaimAmount), Middel = mean(ClaimAmount)) %>%
  print(n=100)

freMPL1Claim %>%
  group_by(VehClass) %>%
  summarise(AntSka = sum(ClaimInd), AntFaar=sum(Exposure), Median = median(ClaimAmount), Middel = mean(ClaimAmount)) %>%
  print(n=100)



##### 4. Fit GLM models #####
freMPL1$SocioCategInd <- (freMPL1$SocioCateg %in% c("CSP26", "CSP66"))
freMPL1$VehUsageInd <- (freMPL1$VehUsage %in% c("Professional", "Professional run"))
freMPL1$VehBodyInd <- (freMPL1$VehBody %in% c("cabriolet", "microvan", "van"))

Freq1 <- glm(ClaimInd ~ (LicAge<60) + 
               SocioCategInd +
               VehUsageInd +
               VehBodyInd +
               #(DrivAge<25)+
               HasKmLimit + 
               (BonusMalus<100) + 
               #(VehEngine == "direct injection overpowered")+
               (VehEnergy == "diesel")+
               offset(log(Exposure)), data=freMPL1, family="binomial")
summary(Freq1)


#Lognormal distribution fits the data well
hist(log(freMPL1Claim$ClaimAmount))
qqnorm(log(freMPL1Claim$ClaimAmount))


freMPL1$BonusMalusG <- cut(freMPL1$BonusMalus, breaks=c(49,60,80,272))
freMPL1$VehPriceG <- (freMPL1$VehPrice %in% c("Z", "Y", "X", "W", "V", "U", "T", "S", "R", "Q"))
freMPL1$VehClassG <- (freMPL1$VehClass %in% c(0, "H"))
freMPL1Claim <- subset(freMPL1, ClaimInd==1)
#nrow(freMPL1Claim)
Claim1 <- glm(ClaimAmount ~ 
               #(LicAge<50) + 
               MariStat + 
               #(DrivAge<25) + 
               BonusMalusG + 
               (VehBody == "station wagon") +
               #VehPriceG + 
               VehClassG 
               #(RiskVar>17)
               , data=freMPL1Claim, family=gaussian(link = "log"))
summary(Claim1)

#Check our predictions
summary(predict(Claim1, freMPL1, type = "response"))
summary(predict(Freq1, freMPL1, type = "response")/freMPL1$Exposure) 
summary(predict(Claim1, freMPL1, type = "response") *predict(Freq1, freMPL1, type = "response")/freMPL1$Exposure)


