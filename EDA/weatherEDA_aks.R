################ Clearing everything ################

rm(list = ls())

################ Loading Packages ################
library(here)
library(tidyverse) 
library(ggplot2)
library(janitor)
library(dplyr)
library(jsonlite)
library(tidyr)
library(data.table)
library(Amelia)


#Loading the dataset
getwd()
setwd("/Users/akshayaparthasarathy/Desktop/UNI/Semester 2/STDS/Assessments")
dset <- read_csv("fire_modis.csv")
summary(dset)
nrow(dset)


dir <-  "/Users/akshayaparthasarathy/Desktop/UNI/Semester 2/STDS/Assessments/AT2/dataset"
files <-  list.files(path = dir, pattern = "*.csv", full.names = TRUE)
wdset <- do.call(rbind, lapply(files, fread))
head(wdset)
nrow(wdset)

dset <- clean_names(dset)
wdset <- clean_names(wdset)

#Adding Latitude and Longitude columns to Weather Data
wdset <- wdset %>% add_column(latitude = NA)
wdset <- wdset %>% add_column(longitude = NA)

table(wdset$name)


wdset$name[wdset$name == "-35.7752, 137.2142"] <- "Kangaroo Island, Australia"
wdset$name[wdset$name == "-35.6307, 148.1893"] <- "Snowy Valleys, Australia"
wdset$name[wdset$name == "-12.4662, 142.1488"] <- "Napranum, Australia"



#Adding latitude & longitude values

#Aurukun
wdset$latitude[wdset$name == "Aurukun, Australia"] <- -13.354
wdset$longitude[wdset$name == "Aurukun, Australia"] <- 141.7


#Kangaroo Island
wdset$latitude[wdset$name == "Kangaroo Island, Australia"] <- -35.775
wdset$longitude[wdset$name == "Kangaroo Island, Australia"] <- 137.2


#Snowy Valleys
wdset$latitude[wdset$name == "Snowy Valleys, Australia"] <- -35.630
wdset$longitude[wdset$name == "Snowy Valleys, Australia"] <- 148.1

#Napranum
wdset$latitude[wdset$name == "Napranum, Australia"] <- -12.466
wdset$longitude[wdset$name == "Napranum, Australia"] <- 142.1


#Bega Valley
wdset$latitude[wdset$name == "Bega Valley, Australia"] <- -36.680
wdset$longitude[wdset$name == "Bega Valley, Australia"] <- 149.6


#Belyuen
wdset$latitude[wdset$name == "Belyuen"] <- -12.536
wdset$longitude[wdset$name == "Belyuen"] <- 130.6

#Blue Mountains
wdset$latitude[wdset$name == "Blue Mountains, Australia"] <- -33.410
wdset$longitude[wdset$name == "Blue Mountains, Australia"] <- 150.3

#Clarence Valley, Australia
wdset$latitude[wdset$name == "Clarence Valley, Australia"] <- -29.878
wdset$longitude[wdset$name == "Clarence Valley, Australia"] <- 152.6


#East Gippsland, Australia 
wdset$latitude[wdset$name == "East Gippsland, Australia"] <- -37.497
wdset$longitude[wdset$name == "East Gippsland, Australia"] <- 148.189

#Eurobodalla, NSW 
wdset$latitude[wdset$name == "Eurobodalla, NSW"] <- -35.889
wdset$longitude[wdset$name == "Eurobodalla, NSW"] <- 149.933

#Hawkesbury, Australia
wdset$latitude[wdset$name == "Hawkesbury, Australia"] <- -33.226
wdset$longitude[wdset$name == "Hawkesbury, Australia"] <- 150.860

#Kempsey, Australia
wdset$latitude[wdset$name == "Kempsey, Australia"] <- -31.080
wdset$longitude[wdset$name == "Kempsey, Australia"] <- 152.8


#Kowanyama, Australia
wdset$latitude[wdset$name == "Kowanyama, Australia"] <- -15.474
wdset$longitude[wdset$name == "Kowanyama, Australia"] <- 151.7


#Lithgow, Australia
wdset$latitude[wdset$name == "Lithgow, Australia"] <- -33.482
wdset$longitude[wdset$name == "Lithgow, Australia"] <- 150.1

#Pormpuraaw, Australia
wdset$latitude[wdset$name == "Pormpuraaw, Australia"] <- -14.899
wdset$longitude[wdset$name == "Pormpuraaw, Australia"] <- 151.6

#Port Macquarie-Hastings
wdset$latitude[wdset$name == "Port Macquarie-Hastings"] <- -31.470
wdset$longitude[wdset$name == "Port Macquarie-Hastings"] <- 152.5

#Richmond Valley, Australia
wdset$latitude[wdset$name == "Richmond Valley, Australia"] <- -29.038
wdset$longitude[wdset$name == "Richmond Valley, Australia"] <- 153.1

#Shoalhaven, Australia
wdset$latitude[wdset$name == "Shoalhaven, Australia, Australia"] <- -35.081
wdset$longitude[wdset$name == "Shoalhaven, Australia, Australia"] <- 150.4


#Wollondilly, Australia
wdset$latitude[wdset$name == "Wollondilly, Australia"] <- -34.110
wdset$longitude[wdset$name == "Wollondilly, Australia"] <- 150.4


#Areas which were not affected by fire
#Brewarrina, Australia
wdset$latitude[wdset$name == "Brewarrina, Australia"] <- -29.962
wdset$longitude[wdset$name == "Brewarrina, Australia"] <- 146.8


#Cowra, Australia
wdset$latitude[wdset$name == "Cowra, Australia"] <- -33.834
wdset$longitude[wdset$name == "Cowra, Australia"] <- 148.7


#Sydney
wdset$latitude[wdset$name == "Sydney"] <- -33.868
wdset$longitude[wdset$name == "Sydney"] <- 151.2



#Broken Hill, Australia
wdset$latitude[wdset$name == "Broken Hill, Australia"] <- -31.959
wdset$longitude[wdset$name == "Broken Hill, Australia"] <- 141.4

#Melbourne
wdset$latitude[wdset$name == "Melbourne"] <- -37.813
wdset$longitude[wdset$name == "Melbourne"] <- 144.9

#Data is ready for analysis

#Data Cleaning 


#Plotting out missing values

missmap(wdset, main = "Missing V/S Observed")

#Removing columns based on their irrelevancy to the bushfires

data_sample <- dplyr::select(wdset, -c('stations','moonphase','sunrise','sunset','snow','snowdepth','severerisk'))

df = merge(x=dset,y=data_sample,by="latitude",all.x=TRUE)

df2 = merge(x=dset,y=data_sample,by="latitude",all.y=TRUE)

table(dset$latitude)

table(data_sample$latitude)

summary(df2)

nrow(df2)

write.csv(df2,"/Users/akshayaparthasarathy/Desktop/UNI/Semester 2/STDS/Assessments/AT2/dataset/compiled_weather/weather_final.csv", row.names = FALSE)


write.csv(df,"/Users/akshayaparthasarathy/Desktop/UNI/Semester 2/STDS/Assessments/AT2/dataset/compiled_weather/new_merge.csv", row.names = FALSE)



head(df2)










