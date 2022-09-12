# Importing Library

library(tidyverse)
library(magrittr)
library(GGally)
library(ggpubr)
library(caret)

# Loading Data
frms <- read_csv("FRMS/modis_data.csv")
# Creating datafram for the model
frms_model_data <- frms %>%
  dplyr::select(latitude:track,confidence,bright_t31,frp,daynight)

# Basic EDA
summary(frms_model_data)
str(frms_model_data)
dim(frms_model_data)
no_na_value <- sum(apply(frms_model_data,2, function(x) is.na(x)))

# Fire class creation
frms_model_data$fire_class <- ifelse(frms_model_data$frp >=142, 1, 0)

# Data split for train and test

train_index <- createDataPartition(frms_model_data$fire_class, p=0.80, list=FALSE)
train_data <- frms_model_data[train_index,]
test_data <- frms_model_data[-train_index,]


glm = glm(fire_class ~ longitude * latitude + brightness, family = binomial(logit), data = train_data)

probability <- predict(glm, newdata =test_data, type="response")

test_data$prediction <- probability
test_data$prediction <- ifelse(test_data$prediction > 0.5, 1, 0)

table(test_data$fire_class)
table(test_data$prediction)

# Confusion matrix
cfm <- table(predicted=test_data$prediction,true=test_data$fire_class)

# Accuracy
accuracy <- (cfm[1,1]+cfm[2,2])/sum(cfm)
accuracy

# Presision
precision <- cfm[1,1]/(cfm[1,1]+cfm[1,2])
precision

# Recall

recall <- cfm[1,1]/(cfm[1,1]+cfm[2,1])
recall

# F1 score

f1 <- 2*(precision*recall/(precision+recall))
f1


#ROC-curve using pROC library

library(pROC)

pROC_obj <- roc(test_data$fire_class,test_data$prediction,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)

sens.ci <- ci.se(pROC_obj)
plot(sens.ci, type="shape", col="lightblue", main ="ROC curve -- Logistic Regression ")


