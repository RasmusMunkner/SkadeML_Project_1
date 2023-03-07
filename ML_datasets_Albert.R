##### 1. Load packages and data ######
library(dplyr)
library(tidyverse)
library(mlr3)
library(lubridate)
#source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
#install.packages(c("xts", "sp", "zoo"))
#install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
#install.packages('gmodels')
library(gmodels)
library(sp)
library(xts)
library(zoo)
library(forcats)
library(CASdatasets)

#Load the data into memory
data("freMPL1")


#We make a series of crosstables between variables to get a feel
#for the data
crosstab(freMPL1, row.vars = "VehEngine", col.vars = "VehEnergy")  
crosstab(freMPL1, row.vars = "VehEngine", col.vars = "ClaimInd")  
crosstab(freMPL1, row.vars = "VehEngine", col.vars = "ClaimInd", type = "r")  
crosstab(freMPL1, row.vars = "SocioCateg", col.vars = "ClaimInd", type = "r")  
crosstab(freMPL1, row.vars = "SocioCateg", col.vars = "ClaimInd", type = "f")  
crosstab(freMPL1, row.vars = "Garage", col.vars = "ClaimInd",type = "r")  
levels(freMPL1$VehAge)
levels(freMPL1$VehBody)
crosstab(freMPL1, row.vars = "VehBody", col.vars = "ClaimInd", type="r")  
crosstab(freMPL1, row.vars = "VehBody", col.vars = "ClaimInd", type="f")  
crosstab(freMPL1, row.vars = "HasKmLimit", col.vars = "ClaimInd", type="r") 
crosstab(freMPL1, row.vars = "VehMaxSpeed", col.vars = "ClaimInd", type="r")  
crosstab(freMPL1, row.vars = "VehMaxSpeed", col.vars = "ClaimInd", type="f")  

freMPL1 %>%
  mutate(ClaimInd=ClaimInd/Exposure)%>%
  select(ClaimInd,SocioCateg)%>%
  group_by(SocioCateg)%>%
  summarise(avg = mean(ClaimInd))%>%
  print(n=37)

#Creating Frequency data

freq_df <- freMPL1 %>%
            mutate(ObsFreq=ClaimInd/Exposure)%>%
                  mutate(Cheap=as.factor(as.numeric(VehPrice)<13))%>%
                    mutate(Old=as.factor(VehAge=="10+"))%>%
                    filter(VehEnergy %in% c("regular","diesel"))%>%
                        droplevels()%>%
                    filter(!VehEngine %in% c("electric","GPL"))%>%
                        droplevels()%>%
                    mutate(LicAge = as.numeric(LicAge))%>%
                    mutate(HasKmLimit=as.factor(HasKmLimit))%>%
                    mutate(Sedan=as.factor(VehBody == "sedan"))%>%
                    select(-c(RecordEnd, ClaimAmount,
                              Garage, Gender, MariStat,
                              SocioCateg, VehAge, VehPrice,
                              RiskVar, VehClass, VehBody,
                              RecordBeg))

freq_df$VehMaxSpeed<-fct_collapse(freq_df$VehMaxSpeed, "1-150 km/h" = c("1-130 km/h", "130-140 km/h", "140-150 km/h"),
             "150-200 km/h" = c("150-160 km/h","160-170 km/h","170-180 km/h",
                                "180-190 km/h","190-200 km/h"), 
             "200+ km/h" = c("200-220 km/h","220+ km/h"))

levels(freq_df$VehMaxSpeed)


#We create the claim size data set

claimsize_df <- freMPL1 %>%
                filter(ClaimInd==1)%>%
                mutate(LicAge = as.numeric(LicAge))%>%
                mutate(HasKmLimit=as.factor(HasKmLimit))%>%
                mutate(Cheap=as.factor(as.numeric(VehPrice)<13))%>%
                mutate(DrivAge=as.numeric(DrivAge))%>%
                mutate(Old=as.factor(VehAge=="10+"))%>%
                filter(VehEnergy %in% c("regular","diesel"))%>%
                    droplevels()%>%
                filter(!VehEngine %in% c("electric","GPL"))%>%
                    droplevels()%>%
                mutate(Sedan=as.factor(VehBody == "sedan"))%>%
                select(-c(ClaimInd,RecordEnd,
                        Gender, MariStat, SocioCateg,
                        VehPrice,RiskVar,RecordBeg,
                        VehClass, VehAge, VehPrice,
                        BonusMalus, Exposure, VehBody))



  
claimsize_df$VehMaxSpeed<-fct_collapse(claimsize_df$VehMaxSpeed, "1-150 km/h" = c("1-130 km/h", "130-140 km/h", "140-150 km/h"),
                                  "150-200 km/h" = c("150-160 km/h","160-170 km/h","170-180 km/h",
                                                     "180-190 km/h","190-200 km/h"), 
                                  "200+ km/h" = c("200-220 km/h","220+ km/h"))



  
  

