
#Installing packages
install.packages(c("xts", "sp", "zoo"))
install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")

#Load the package containing the dataset
library(CASdatasets)
?CASdatasets

#Load the data into memory
data("freMPL1")
