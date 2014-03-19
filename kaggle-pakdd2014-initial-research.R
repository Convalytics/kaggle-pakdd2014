######################################################
#  Kaggle PAKDD 2014 ASUS Challenge
#  Jason Green
#  March 18th, 2014
#  https://github.com/Convalytics/kaggle-pakdd2014
#  Last Updated: 3/18/2014
######################################################


# Load Packages
library(plyr)
library(ggplot2)
library(gridExtra)


# Set Working Directory
setwd("~/GitHub/kaggle-pakdd2014")

# Import Data
sampleSubmission <- read.csv("~/GitHub/kaggle-pakdd2014/SampleSubmission.csv")
RepairTrain <- read.csv("~/GitHub/kaggle-pakdd2014/RepairTrain.csv")
TargetMap <- read.csv("~/GitHub/kaggle-pakdd2014/Output_TargetID_Mapping.csv")
SaleTrain <- read.csv("~/GitHub/kaggle-pakdd2014/SaleTrain.csv")

head(sampleSubmission, n=5)

head(RepairTrain, n=5)
summary(RepairTrain)
hist(subset(RepairTrain$number_repair, RepairTrain$number_repair > 3))
table(RepairTrain$number_repair)
# Very few number_repair > 2

head(TargetMap, n=5)
summary(TargetMap)

head(SaleTrain, n=5)
summary(SaleTrain)
#plot(SaleTrain$number_sale ~ SaleTrain$module_category)

moduleRepairs <- qplot(module_category,data=RepairTrain, binwidth = 1, geom="histogram", na.rm=T, ylab="Repair Count")
compRepairs <- qplot(component_category,data=RepairTrain, binwidth = 1, geom="histogram", na.rm=T, ylab="RepairCount")
moduleSales <- qplot(module_category,data=SaleTrain, binwidth = 1, geom="histogram", na.rm=T, ylab="Sales Count")
compSales <- qplot(component_category,data=SaleTrain, binwidth = 1, geom="histogram", na.rm=T, ylab="Sales Count")
# ??? Every component was sold the exact same number of times???

grid.arrange(moduleSales, compSales, moduleRepairs, compRepairs,ncol=2)

## Need to figure out the average time from sale to repair