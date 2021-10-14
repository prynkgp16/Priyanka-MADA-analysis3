###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(readxl) #for loading Excel files
library(dplyr) #for data processing
library(tidyverse)
library(here) #to set paths

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","raw_data","SympAct_Any_Pos.Rda")

#load data. 
#note that for functions that come from specific packages (instead of base R)
# I often specify both package and function like so
#package::function() that's not required one could just call the function
#specifying the package makes it clearer where the function "lives",
#but it adds typing. You can do it either way.
rawdata <- readRDS(data_location)

#take a look at the data
dplyr::glimpse(rawdata)

#dataset is so small, we can print it to the screen.
#that is often not possible.
print(rawdata)

# Removing variables

cleaneddata <- rawdata[, -c(1:8, 41:63)]

#take a look at the data

cleaneddata

#Removing NA oberservations

cleaneddata <- na.omit(cleaneddata)


#take a look at the data

cleaneddata


# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")

saveRDS(cleaneddata, file = save_data_location)


