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
viirs_data <- read.csv("viirs_data.csv")
