######################################################
#  Kaggle PAKDD 2014 ASUS Challenge
#  Jason Green
#  March 18th, 2014
#  https://github.com/Convalytics/kaggle-pakdd2014
#  Last Updated: 3/18/2014
######################################################

#install.packages("Hmisc")
# Load Packages
library(plyr)
library(ggplot2)
library(gridExtra)
library(Hmisc)

# Set Working Directory
setwd("~/GitHub/kaggle-pakdd2014")

# Import Data
sampleSubmission <- read.csv("~/GitHub/kaggle-pakdd2014/SampleSubmission.csv")
RepairTrain <- read.csv("~/GitHub/kaggle-pakdd2014/RepairTrain.csv")
SaleTrain <- read.csv("~/GitHub/kaggle-pakdd2014/SaleTrain.csv")
TargetMap <- read.csv("~/GitHub/kaggle-pakdd2014/Output_TargetID_Mapping.csv")

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



SaleSummary <- ddply(SaleTrain, c("module_category","component_category","year.month"), summarize, 
                      #TotalCount = length(module_category), 
                      SaleCount = sum(number_sale)
)
RepairSummary <- ddply(RepairTrain, c("module_category","component_category","year.month.repair."), summarize, 
                        #TotalCount = length(module_category), 
                        RepairCount = sum(number_repair)
)

TotalRepairs <- ddply(RepairTrain, c("module_category","component_category"), summarize, 
                       #TotalCount = length(module_category), 
                       RepairCount = sum(number_repair)
)

# write.csv(SaleSummary, file="SaleSummary.csv")
# write.csv(RepairSummary, file="RepairSummary.csv")

AllSummary <- merge(x=SaleSummary, y=RepairSummary, 
                    by.x=c("module_category","component_category","year.month"), 
                    by.y=c("module_category","component_category","year.month.repair."), 
                    all=TRUE
                    )

head(AllSummary)
AllSummary$year <- substr(AllSummary$year.month,1,4)
AllSummary$month <- substr(AllSummary$year.month,6,7)
# 
# M1_P02_Sales <- subset(SaleTrain, module_category=="M1" & component_category == "P02")
# M1_P02_Sales <- ddply(M1_P02_Sales, c("module_category","component_category","year.month"), summarize, 
#                         TotalCount = length(module_category), 
#                         SaleCount = sum(number_sale)
# )
# 
# 
# M1_P02_Repairs <- subset(RepairTrain, module_category=="M1" & component_category == "P02")
# M1_P02_Repairs <- ddply(M1_P02_Repairs, c("module_category","component_category","year.month.repair."), summarize, 
#                         TotalCount = length(module_category), 
#                         RepairCount = sum(number_repair)
#                         )
# 
# 
####
####
##########################
# Map to target
TargetMap$Target <- NA
head(TargetMap)

# Put repairs into ranks
TotalRepairs$Group <- as.numeric(cut2(TotalRepairs$RepairCount, g=5))

TargetMap <- merge(x=TargetMap, y=TotalRepairs, 
      by.x=c("module_category","component_category"), 
      by.y=c("module_category","component_category"), 
      all.x=TRUE
)

TargetMap$Group <- TargetMap$Group - 1
TargetMap$Group <- ifelse(TargetMap$year == 2011, TargetMap$Group - 1, TargetMap$Group)
TargetMap$Group <- ifelse(TargetMap$Group < 0, 0, TargetMap$Group)

write.csv(TargetMap[,"Group"],file="convalytics_pakdd_xx.csv")

