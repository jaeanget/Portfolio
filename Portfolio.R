#Lampiran Rscript#

#Load Package#
library(factoextra)
library(cluster)
library(ggplot2)
library(dplyr)
library(broom)
library(ggdendro)
library(readxl)
library(fmsb)
library(NbClust)
library(RColorBrewer)
library(gridExtra)
library(readxl)
library(tidyr)
library(corrplot)

#Import Data with Authors Modificated data by Entering Certain Used Variables
Data_1 <- read_excel("Data 1.xlsx")
View(Data_1)

#Checking Structure of The Data
str(Data_1)

#Transforming Lot Frontage from Character to Numeric
Data_1$LotFrontage<- as.numeric (Data_1$LotFrontage)

#There is Still Missing value in Lot Frontage Variable
#Replace missing values in each numeric column with median value of column
Data_1 <- Data_1 %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm=TRUE))))

#Exploratory Data Analysis#

##Making Descriptive Statstics of The Data##
summary(Data_1)

##Correlation of The Data#

library(ggcorrplot)
corr <- round(cor(Data_1), 1)

# Plot
ggcorrplot(corr,
           type = "lower",
           lab = TRUE, 
           lab_size = 5,  
           colors = c("red", "white", "blue"),
           title="Correlogram of Housing Dataset", 
           ggtheme=theme_bw)


##Boxplot of Every Variables#
boxplot(Data_1$LotFrontage)
boxplot(Data_1$LotArea)
boxplot(Data_1$TotalSFFlr)
boxplot(Data_1$GarageArea)
boxplot(Data_1$SalePrice)

boxplot(Data_1)

## Histogram Every Variables#
hist(Data_1$LotFrontage, lwd=3, main="Lot Frontage", xlab = "ft^2", col = "yellow")
hist(Data_1$LotArea, lwd=3, main="Lot Area", xlab = "ft^2", col = "pink")
hist(Data_1$TotalSFFlr, lwd=3, main="Total SFFlr", xlab = "ft^2", col = "lightgreen")
hist(Data_1$GarageArea, lwd=3, main="Garage Area", xlab = "ft^2", col = "lightblue")
hist(Data_1$SalePrice, lwd=3, main="Sale Price", xlab = "US Dollar", col = "purple")


##Data Distribution each variables##
pairs(Data_1)

#Selecting Model by Seeing R Square for each Model#
attach(Data_1)
lm.fit<-lm(SalePrice ~ LotFrontage + LotArea + GarageArea, data = Data_1)
summary(lm.fit)
lm.fit2<-lm(SalePrice ~LotFrontage + LotArea + TotalSFFlr, data = Data_1)
summary(lm.fit2)
lm.fit3<-lm(SalePrice~ LotFrontage +  TotalSFFlr + GarageArea, data = Data_1)
summary(lm.fit3)
lm.fit4 <- lm(SalePrice ~ LotArea + GarageArea +  TotalSFFlr, data = Data_1)
summary(lm.fit4)

#Clustering#

library(factoextra)  #Mengaplikasikan algoritma clustering kmeans
library(cluster) #Mengaplikasikan algoritma clustering
library(ggplot2) #Memvisualisasikan data yang dimiliki
library(dplyr) #Manipulasi data
library(broom) #Manipulasi data, statistical objects into tibbles
library(ggdendro) #Memvisualisasikan dendogram
library(readxl) #Membaca file excel
library(fmsb) #Memvisualisasikan radar chart
library(NbClust) #Optimum number of Cluster
library(RColorBrewer) #Visualisasi Warna di R
library(gridExtra)

##Import Data Excluding Sale Price as our Dependent Variable##
Data_2 <- read_excel("Paper/Data and command/Data 2.xlsx")
View(Data_2)

Data_2 <- as.data.frame(Data_2)


## Determining Best Cluster Using Elbow Method##

set.seed(123)
wss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(Data_2, centers = i)
  wss[i] <- kmeans_model$tot.withinss
}
plot(1:10, wss, type = "b", main = "Elbow Method", xlab = "Number of Clusters (k)", ylab = "Total Within Sum of Squares")

##K-Means Clustering##
nc <- NbClust(Data_2, distance="euclidean", min.nc=2, max.nc=10, method="kmeans")

set.seed(123)
km_out = kmeans(Data_2, 3)

fviz_cluster(km_out, data = Data_2)

res <- cbind(Data_2, km_out$cluster)

res

new <- aggregate(res[,-ncol(res)], list(res[,ncol(res)]), mean)
new

##Set graphic colors##
library(RColorBrewer)
coul <- brewer.pal(4, "RdBu")
colors_border <- coul
library(scales)
colors_in <- alpha(coul, 0.3)

## Remove the 2 first lines, the function compute the max and min of each variable with the available data:
radarchart( new[,-1], axistype=0 , maxmin=F,
            #custom polygon
            pcol=colors_border, pfcol=colors_in, plwd=4, plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            #custom labels
            vlcex=0.8 
)

## Add a legend
legend(x=0.7, y=1, legend = new$Group.1, bty = "n", pch=20, col=colors_in, text.col = "grey", cex=1.2, pt.cex=3)

# Linear Regression #

lm.fit4 <- lm(SalePrice ~ LotArea + GarageArea +  TotalSFFlr, data = Data_1)
summary(lm.fit4)

# Multinomial Logistic Regression #

## Import data with dependent variable has changed into categorical data ##
Data_3 <- read_excel("Paper/Data and command/Data 3.xlsx")
View(Data_3)

##Multionmial Logistic Regression Result##

library(nnet)
Data_3$SalePriceNum <- as.factor(Data_3$SalePriceNum)
mlg <- multinom(SalePriceNum~LotArea+TotalSFFlr+GarageArea, data = Data_3)
summary(mlg)

coefficients <- coef(mlg)

# Calculate odds ratios
odds_ratios <- exp(coefficients)

cat("Odds Ratios:\n")
print(odds_ratios)

# Print odds ratios
cat("Odds Ratios:\n")
print(odds_ratios)

##Confusion Matrix Multinomial Logistic Regression Model##


predicted_scores <- predict (mlg, Data_3, "probs") # predict on new data
predicted_class <- predict (mlg, Data_3)
table(predicted_class, Data_3$SalePriceNum)
confusionMatrix <- confusionMatrix(predicted_class, Data_3$SalePriceNum)
confusionMatrix


# Decision Tree #

attach(Data_3)

Data_3 <- data.frame(Data_3, 3)
tree <- tree(SalePriceNum ~ ., Data_3)

## Plot of Decision Tree ##
plot(tree)
text(tree, pretty = 0)
tree

## Determining Optimal Point to Prune the Tree ##
cv_result <- cv.tree(tree)
plot(cv_result$size, cv_result$dev, type = "b", xlab = "Size", ylab = "CV Error")

optimal_tree_size <- which.min(cv_result$dev)
pruned_tree <- prune.tree(tree, best = optimal_tree_size)

## Plot of Decision Tree After Pruned ##
plot(pruned_tree)
text(pruned_tree, pretty = 0)

## Confusion Matrix of Decision Tree After Pruned ##
pruned_predictions <- predict(pruned_tree, Data_3, type = "class")
conf_matrix_pruned <- table(pruned_predictions, SalePriceNum)
conf_matrix_pruned

accuracy_pruned <- sum(diag(conf_matrix_pruned)) / sum(conf_matrix_pruned)
precision_pruned <- diag(conf_matrix_pruned) / rowSums(conf_matrix_pruned)
recall_pruned <- diag(conf_matrix_pruned) / colSums(conf_matrix_pruned)
f1_score_pruned <- 2 * (precision_pruned * recall_pruned) / (precision_pruned + recall_pruned)

cat("Accuracy:", accuracy_pruned, "\n")
cat("Precision:", precision_pruned, "\n")
cat("Recall:", recall_pruned, "\n")
cat("F1 Score:", f1_score_pruned, "\n")


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

Data_3$SalePriceNum <- as.factor(Data_3$SalePriceNum)    
data_train$SalePriceNum <- as.factor(data_train$SalePriceNum)

## Maximizing Random Forest ##

bestmtry <- tuneRF(data_train,data_train$SalePriceNum,stepFactor = 1.2, improve = 0.01, trace=T, plot= T) 

## Result of the Random Forest ##
rndmfrstmodel <- randomForest(SalePriceNum~LotArea+TotalSFFlr+GarageArea,data= data_train)
rndmfrstmodel

#K-Fold Cross Validation#

library(caret)
library(nnet)

#specify the cross-validation method, trying 5 fold
ctrl <- trainControl(method = "cv", number = 5)

model <- train(SalePriceNum~ ., data = Data_3, method = "multinom", trControl = ctrl)

print(model)

#view final model
model$finalModel

#view predictions for each fold
model$resample

#10 fold
ctrl <- trainControl(method = "cv", number = 10)

model <- train(SalePriceNum~ ., data = Data_3, method = "multinom", trControl = ctrl)

print(model)

#view final model
model$finalModel

#view predictions for each fold
model$resample
