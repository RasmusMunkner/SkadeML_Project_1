
#Installing packages
install.packages(c("xts", "sp", "zoo"))
install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
install.packages('gmodels')
library(gmodels)

#Load the package containing the dataset
library(CASdatasets)
?CASdatasets

#Load the data into memory
data("freMPL1")

Freq1 <- glm(ClaimInd ~ DrivAge + offset(Exposure), data=freMPL1, family="binomial")
summary(Freq1)

Freq1 <- glm(ClaimInd ~ Gender, data=freMPL1, family="binomial")

print('Final Test 2')
