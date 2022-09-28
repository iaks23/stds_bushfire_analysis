#load packages
library(pROC)
library(readr)
library(corrplot)
library(GGally)
library(dlookr)
library(ggplot2)
library(dplyr)
library(caTools)

#load csv file
viirs_data <- read.csv("~/Desktop/UNI/Sem_2_2022/36103/AT2/stds_bushfire_analysis/Datasets/viirs_data.csv")

summary(viirs_data$confidence)
table(viirs_data$confidence)
table(viirs_data$daynight)

#Remove unnecessary columns
viirs_model_data <- select(viirs_data, -acq_date, -satellite, -instrument, -version)

viirs_model_data$frp <- log10(viirs_model_data$frp +1)

#Encode confidence and daynight columns
viirs_model_data$confidence <- as.numeric(factor(viirs_model_data$confidence))
viirs_model_data$daynight <- as.numeric(factor(viirs_model_data$daynight))

#Split into train and test sets
set.seed(12)
viirs_split <- viirs_model_data
sample <- sample.split(viirs_split, SplitRatio = 0.8)
train <- subset(viirs_split, sample == TRUE)
test <- subset(viirs_split, sample == FALSE)

#Check dimensions of train and test
dim(train)
dim(test)

head(train)

#prepare for modelling
x= train[,-9]
y= train$frp

x_test<- test[,-9]
y_test <- test$frp

#define intercept only model
intercept_only <- lm(y ~ 1, data= x)


#define model with all predictors
all <- lm(y ~ ., data= x)

#perform forward stepwise regression
forward <- step(intercept_only, direction= "forward", scope = formula(all), trace=0)

#View results
forward$anova

#View final model
forward$coefficients
summary(forward)

step_prob_train <- predict(forward, x_test, type = "response")

#Plot ROC
ROC<- roc(y_test, step_prob_train)
plot(ROC, col="blue")
auc(ROC)




