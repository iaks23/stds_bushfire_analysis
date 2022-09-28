rm(list=ls())
#load packages
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(corrplot)
library(GGally)
library(DT)
library(caret)
library(dlookr)
library(dplyr)
library(class)
library(naivebayes)
library(pROC)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(e1071)
library(gmodels)
library(psych)
library(pROC)
library(caret)
library(datasets)
library(skimr)
library(ggpubr)
library(moments)

#load csv file
viirs_data <- read.csv("~/Desktop/UNI/Sem_2_2022/36103/AT2/stds_bushfire_analysis/Datasets/viirs_data.csv")

#Remove unnecessary columns
viirs_model_data <- select(viirs_data, -acq_date, -satellite, -instrument, -version)

#Encode confidence and daynight columns
viirs_model_data$confidence <- as.numeric(factor(viirs_model_data$confidence))
viirs_model_data$daynight <- as.numeric(factor(viirs_model_data$daynight))

as.factor(viirs_model_data$confidence)

b <- ggplot(data = viirs_model_data, aes(x=confidence, fill= "red")) + geom_bar() 
b

c<- ggplot(data = viirs_model_data, aes(x=daynight, fill= "grey")) + geom_bar() 
c


#plot frp
a <- ggplot(data=viirs_model_data, aes(x=frp)) + geom_histogram()
a

#Check distribution of frp
ggdensity(viirs_model_data, x= "frp", fill="blue", title= "frp distribution") +
  stat_overlay_normal_density(color="red", linetype= "dashed")

#Compute Skewness
skewness(viirs_model_data$frp)

#log transformation on frp data due to skewness.
viirs_model_data$frp <- log10(viirs_model_data$frp +1)

log10(142)
#viirs_model_data$frp <- ifelse(viirs_model_data$frp >= log10(142), 1, 0)
View(viirs_model_data)

#View transformed data
ggplot(viirs_model_data, aes(x=frp, fill="blue")) + geom_density() +
  stat_overlay_normal_density(color="blue", linetype="dashed")

skewness(viirs_model_data$frp)

#Stratified sampling
set.seed(12)
train.index <- createDataPartition(viirs_model_data$frp, p=0.8, list= FALSE)
train <- viirs_model_data[train.index,]
train_frp <- select(train,-frp)
View(train_frp)
test <- viirs_model_data[-train.index,]
test_frp <- select(test, -frp)

x=train[,-9]
y=train$frp

##Add all predictors 
glm_model <- glm(y ~ ., data = x)

summary(glm_model)
plot(glm_model)



##Calculate the prob of model
#Do this on train data, then test data

prob_train <- predict(glm_model, train_frp, type = "response")
prob_test <- predict(glm_model, test_frp, type = "response")

pred_train <- ifelse(prob_train > 0.5, 1, 0)
pred_test <- ifelse(prob_test > 0.5, 1, 0)

pred_test <- as.factor(pred_test)
is.factor(pred_test)
test$frp <- as.factor(test$frp)
is.factor(test$frp)

frp_comp <- data.frame()
frp_comp <- data.frame(pred_test, test$frp)

View(frp_comp)



#Confusion Matrix, F1 and ROC AUC for test set
confusionMatrix(pred_test, test$frp, mode= "everything")
ROC_test <- roc(test$frp, prob_test)
plot(ROC_test, col="red")
auc(ROC_test)
