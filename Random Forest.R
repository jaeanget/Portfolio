#Random Forest#
library(dplyr)
library(caTools)
library(randomForest)
library(caret)

Data_3 <- read_excel("Paper/Data and command/Data 3.xlsx")
Data_3 <- as.data.frame(Data_3)
split <- sample.split(Data_3, SplitRatio = 0.8) 
split 

data_train <- subset(Data_3, split == "TRUE") 
data_test <- subset(Data_3, split == "FALSE") 

str(Data_3)

Data_3$SalePriceNum <- as.factor(Data_3$SalePriceNum)    
data_train$SalePriceNum <- as.factor(data_train$SalePriceNum)

bestmtry <- tuneRF(data_train,data_train$SalePriceNum,stepFactor = 1.2, improve = 0.01, trace=T, plot= T) 

rndmfrstmodel <- randomForest(SalePriceNum~LotArea+TotalSFFlr+GarageArea,data= data_train)

rndmfrstmodel

