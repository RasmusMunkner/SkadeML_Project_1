##### 1. Load packages and data ######
library(dplyr)
library(tidyverse)
library(mlr3)
library(lubridate)
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
#install.packages(c("xts", "sp", "zoo"))
#install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
#install.packages('gmodels')
library(gmodels)
library(sp)
library(xts)
library(zoo)
library(forcats)
library(CASdatasets)
library(fastDummies)
source("Rasmus_Funktioner.R")

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

###########################################################################################
#Creating Frequency data
###########################################################################################

#Old implementation
freq_df <- freMPL1 %>%
  distinct() %>% 
  mutate(ObsFreq=ClaimInd/Exposure) %>%
  mutate(Cheap=as.factor(as.numeric(VehPrice)<13)) %>%
    mutate(Old=as.factor(VehAge=="10+")) %>%
    mutate(VehEnergy = fct_collapse(VehEnergy,
                                    regular=c("regular", "eletric", "GPL"))) %>% 
    mutate(VehEngine = fct_collapse(VehEngine,
                                    injection=c("injection", "electric", "GPL")),
           VehMaxSpeed = fct_collapse(VehMaxSpeed, "1-150_km/h" = c("1-130 km/h", "130-140 km/h", "140-150 km/h"),
                                      "150-200_km/h" = c("150-160 km/h","160-170 km/h","170-180 km/h",
                                                         "180-190 km/h","190-200 km/h"), 
                                      "200plus_km/h" = c("200-220 km/h","220+ km/h"))) %>%
    mutate(LicAge = as.numeric(LicAge)) %>%
    mutate(HasKmLimit=as.factor(HasKmLimit)) %>%
    mutate(Sedan=as.factor(VehBody == "sedan")) %>%
    mutate(BonusMalus = as.numeric(BonusMalus)) %>%
    mutate(DrivAge = as.numeric(DrivAge)) %>%
    select(-c(RecordEnd, ClaimAmount,
              Garage, Gender, MariStat,
              SocioCateg, VehAge, VehPrice,
              RiskVar, VehClass, VehBody,
              RecordBeg))
freq_df<-dummy_cols(freq_df, remove_selected_columns = TRUE, remove_first_dummy = T)%>% 
  dplyr::rename_all(list(~make.names(.))) %>% 
  mutate(ClaimInd = factor(ClaimInd))


#New implementation
lvl_pre <- "Group_"
freq_df <- freMPL1 %>% 
  mutate(VehEnergy = fct_collapse(VehEnergy,
                                  regular=c("regular", "eletric", "GPL")),
         VehEngine = fct_collapse(VehEngine,
                                  injection=c("injection", "electric", "GPL")),
         VehMaxSpeed = fct_collapse(VehMaxSpeed,
                                    "1-150_km/h" = c("1-130 km/h", "130-140 km/h", "140-150 km/h"),
                                    "150-200_km/h" = c("150-160 km/h","160-170 km/h","170-180 km/h","180-190 km/h","190-200 km/h"), 
                                    "200plus_km/h" = c("200-220 km/h","220+ km/h")),
         HasKmLimit = factor(HasKmLimit)) %>% 
  select(-c(RecordEnd, ClaimAmount,
            Garage, Gender, MariStat,
            SocioCateg,
            RiskVar,
            RecordBeg)) %>% 
  TreeModelGrouping("VehAge", "ClaimInd", level_prefix = lvl_pre) %>% 
  TreeModelGrouping("VehBody", "ClaimInd", level_prefix = lvl_pre) %>% 
  TreeModelGrouping("VehPrice", "ClaimInd", level_prefix = lvl_pre) %>% 
  mutate(ClaimInd = factor(ClaimInd))

#A nice plot
freq_df %>%
  pivot_longer(cols = c(VehUsage, VehEngine, VehEnergy, VehMaxSpeed, VehClass, VehAge, VehBody, VehPrice),
               names_to = "factor",
               values_to = "level") %>% 
  group_by(factor, level) %>% 
  mutate(ClaimInd = ClaimInd %>% as.character() %>% as.numeric()) %>% 
  summarise(freq = mean(ClaimInd), err = sd(ClaimInd) / sqrt(n()) * 1.96) %>% 
  ggplot(aes(x = level, y = freq)) +
  geom_point() +
  geom_errorbar(aes(ymin = freq - err, ymax = freq + err)) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(~factor, scales ="free_x")

###########################################################################################
#Claim size
###########################################################################################

claimsize_df <- freMPL1 %>%
                distinct() %>% 
                filter(ClaimInd==1)%>%
                mutate(Cheap=as.factor(as.numeric(VehPrice)<13))%>%
                mutate(Old=as.factor(VehAge=="10+"))%>%
                filter(VehEnergy %in% c("regular","diesel"))%>%
                droplevels()%>%
                filter(!VehEngine %in% c("electric","GPL"))%>%
                droplevels()%>%
                mutate(LicAge = as.numeric(LicAge))%>%
                mutate(HasKmLimit=as.factor(HasKmLimit))%>%
                mutate(Sedan=as.factor(VehBody == "sedan"))%>%
                mutate(BonusMalus = as.numeric(BonusMalus))%>%
                mutate(DrivAge = as.numeric(DrivAge))%>%
                select(-c(RecordEnd,
                          Garage, Gender, MariStat,
                          SocioCateg, VehAge, VehPrice,
                          RiskVar, VehClass, VehBody,
                          RecordBeg, Exposure, ClaimInd))
              


  
claimsize_df$VehMaxSpeed<-fct_collapse(claimsize_df$VehMaxSpeed, "1-150_km/h" = c("1-130 km/h", "130-140 km/h", "140-150 km/h"),
                                  "150-200_km/h" = c("150-160 km/h","160-170 km/h","170-180 km/h",
                                                     "180-190 km/h","190-200 km/h"), 
                                  "200plus_km/h" = c("200-220 km/h","220+ km/h"))



claimsize_df<-dummy_cols(claimsize_df, remove_selected_columns = TRUE, remove_first_dummy = T)%>% 
  dplyr::rename_all(list(~make.names(.)))
  

dummy_ClaimAmount <- freMPL1 %>%
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
  mutate(BonusMalus = as.numeric(BonusMalus))%>%
  mutate(DrivAge = as.numeric(DrivAge))%>%
  select(ClaimAmount)

#Writing the datasets to .csv files
source("Rasmus_Funktioner.R")
WriteData(freq_df, "freq_df")
WriteData(claimsize_df, "claimsize_df")

