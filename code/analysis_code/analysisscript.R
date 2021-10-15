###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidyverse)
#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
cleaneddata <- readRDS(data_location)


######################################
#Data fitting/statistical analysis
######################################

##Linear Models

library(tidymodels)  

library(readr)


#setting engine
lmfit <- 
  linear_reg() %>% 
  set_engine("lm")


# fit linear model

##  linear model 1 .Body temperature on runny nose 

lmfit1
<- lm(BodyTemp ~ RunnyNose, cleaneddata)  

# place results from fit into a data frame with the tidy function
lmtable1 <- broom::tidy(lmfit1)

#look at fit results
print(lmtable1)


##  linear model 2 .Body temperature on on all predictors

lmfit2 <- lm(BodyTemp ~ ., cleaneddata)


# place results from fit into a data frame with the tidy function
lmtable2 <- broom::tidy(lmfit2)

#look at fit results
print(lmtable2)


#####Comparing model results 

comparing_model <- anova(lmfit$fit, lmfit1$fit)

### Logistic regression Model

### Logistic regression Model 1 : Nausea on runny nose

log_fit <- 
  logistic_reg() %>% 
  set_mode("classification") %>% 
  set_engine("glm")

glm_1 <- 
  log_fit %>% 
  fit(Nausea ~ RunnyNose, data = cleaneddata)

glm_1

#summarize the results
tidy(glm_1)


#Logistic regression Model 2: nausea on all predictors

glm_2 <- log_fit %>% 
  fit(Nausea ~ ., data = cleaneddata)

glm_2

#summarize the results
tidy(glm_2)


#####Comparing model results

Comp_model <- anova(glm_1$fit, glm_2$fit)



#####  model with all predictors had a better fit than single predcitor model

# save fit results table  
saveRDS(Comp_model, file = here::here("results", "glmtable.Rds"))
saveRDS(comparing_model, file = here::here("results", "lineartable.Rds"))
  