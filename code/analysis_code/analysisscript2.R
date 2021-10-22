---
title: "analysisscript2"
author: "priyanka"
date: "10/20/2021"
output: html_document
---

```{r setup, include=FALSE}
library(here)
library(tidymodels)
library(dplyr)
library(tidyverse) #Added by Carter Coleman to run autoplot().
```


#path to data

```{r

data_location <- here::here("data","processed_data","processeddata.rds")

```

#load data. 

```{r
cleaneddata <- readRDS(data_location)
```

#SPliting data :a training set and a testing set

# Put 3/4 of the data into the training set 


data_split <- initial_split(cleaneddata, prop = 3/4)


# Create data frames for the two sets:


train_data <- training(data_split)
test_data  <- testing(data_split)


# Creating Recipe for all predcitors


nausea_recipe <- recipes::recipe(Nausea ~ ., data = train_data)

nausea_recipe


# Model 1. Fitting a logistic model to Nausea using all predictors of interest

log_fit <- 
   logistic_reg() %>% 
   set_mode("classification") %>% 
   set_engine("glm")


# Using workflow to pair a model and recipe together.


nausea_wflow <- 
    workflow() %>% add_model(log_fit) %>% add_recipe(nausea_recipe)
nausea_wflow


#Preparing  the recipe and train the model



nausea_fit <- 
  nausea_wflow %>% 
  fit(data = train_data)



# Extracting model


nausea_fit %>% 
 extract_fit_parsnip() %>% 
  tidy()


# Predicting with unseen test data

predict(nausea_fit, test_data)


# Probabilities 

Nausea_aug = augment(nausea_fit, test_data)

# The data look like:

Nausea_aug %>% 
  select(Nausea, .pred_class, .pred_Yes)

# ROC curve

Nausea_aug %>% 
       roc_curve(truth = Nausea, .pred_No) %>% 
      autoplot()



## Alternative model with one predictor(Runnynose)

## Creating a recipe: Nausea x RunnyNose

nausea_runnynose_recipe <- 
      recipe(Nausea ~ RunnyNose, data = train_data) 

#  Fitting a logistic model to Nausea using all predictors of interest

log_fit <- 
  logistic_reg() %>% 
  set_mode("classification") %>% 
  set_engine("glm")

# Using workflow to pair a model and recipe together.


nausea_runnynose_wflow <- 
  workflow() %>% add_model(log_fit) %>% add_recipe(nausea_runnynose_recipe)


nausea_runnynose_wflow


#Preparing  the recipe and train the model



nausea_runnynose_fit <- 
  nausea_runnynose_wflow %>% 
  fit(data = train_data)


# Extracting model


nausea_runnynose_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()


# Predicting with unseen test data

predict(nausea_runnynose_fit, test_data)


# Probabilities 

Nausea_runnynose_aug = augment(nausea_runnynose_fit, test_data)

# The data look like:

Nausea_runnynose_aug %>% 
  select(Nausea, .pred_class, .pred_Yes)

# ROC curve

Nausea_runnynose_aug %>% 
  roc_curve(truth = Nausea, .pred_No) %>% 
  autoplot()


#########CARTER COLEMAN MODULE 9 ANALYSIS###########

## Assessing fit model for Body Temperature and all other outcomes.

  
#### Creating a Recipe and Workflow
  
  
# At this stage, our data is split into training and test data sets. 
# Now, we need to create a recipe and workflow to help process the train data 
# for model building. The output is a function that will run the entire linear 
# regression model for any data set. Therefore, we can run the same model for 
# the train and test data frames to create the exact same analysis workflow to 
# ensure comparison.

#Creates recipe
Recipe_BT <- recipe(BodyTemp ~ ., data = train_data)

#Define logistical regression model pipe
linear_mod <- linear_reg() %>% 
  set_engine("lm") %>%
  set_mode("regression")

#Create workflow that adds our recipe and model
BT_lm_wflow <- 
  workflow() %>% 
  add_model(linear_mod) %>% 
  add_recipe(Recipe_BT)
  
  #### Modeling Using the Workflow
  
# Using the workflow above, lets not fit the model to the train data set (Recipe_BT)._

# Defining a command that runs model fitting to Recipe_BT
BT_fit <- 
  BT_lm_wflow %>% 
  fit(data = train_data)

#Pull linear regression fit model using parsnip()
BT_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()
  
### Using Workflows to Make Predictions
  
#Now that we have our modeling workflow, we are going to use the fitted model 
#to predict values in the test data set.

#predict values in test data
predict(BT_fit, test_data)
  
#Additionally, we can use the augment() function to predict outcomes in the 
#test data. But unlike the predict() function, augment() includes prediction residuals._

#Predict outcomes using augment in test data
BT_aug_test <- 
  augment(BT_fit, test_data)
  
### Using RMSE to Assess Model Fit
  
#At this point, we have a fit model, a workflow for that model, and have some 
#predictions made from a test subset of our data. 
#Now, we need to use test predictions to assess if the model can predict 
#values that reflect our actual data. In particular, we will be using
#RMSE as our metric.

#### Assessing fit for test data predictions

##Test Data##

#Calculate RMSE for test data
BT_aug_test %>%
  rmse(truth = BodyTemp, .pred)

# RSME = 1.03

##Train Data##

#Predict outcomes using augment in training data
BT_aug_train <- 
  augment(BT_fit, train_data)

#Calculate RSME for training data
BT_aug_train %>%
  rmse(truth = BodyTemp, .pred)

#RSME = 1.15
  
##The resulting RSME values were 1.03 and 1.15 for the test data model and 
#the training data model, respectively. This suggests that there is a 
#low amount of variance between the model and both data sets. Therefore, 
#the models are good fits to our data and can predict outcomes for Body Temperature.
  
#######Assessing fit model for Body Temperature and Runny Nose.########
  
#Creating a Recipe and Workflow
  
#Create a recipe and workflow to help process the train data for model building.

#Creates recipe
Recipe_BT2 <- recipe(BodyTemp ~ RunnyNose, data = train_data)

#Define linear regression model pipe
linear_mod <- linear_reg() %>% 
  set_engine("lm") %>%
  set_mode("regression")

#Create workflow that adds our recipe and model
BT2_wflow <- 
  workflow() %>% 
  add_model(linear_mod) %>% 
  add_recipe(Recipe_BT2)
  
#### Modeling Using the Workflow
  
  
#Use workflow to fit the model to the train data set (Recipe_BT2).

#Defining a command that runs model fitting to Recipe_BT2.
BT2_fit <- 
  BT2_wflow %>% 
  fit(data = train_data)

#Pull linear reg fit model using parsnip()
BT2_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()
  
### Using Workflows to Make Predictions
  
  
#Use the fitted model to predict values in the test data set.

#predict values in test data
predict(BT2_fit, test_data)

  
#Use the augment() function to predict outcomes in the test data.

#Predict outcomes using augment in test data
BT2_aug_test <- 
  augment(BT2_fit, test_data)
  
### Using RMSE to Assess Model Fit
  
  
#### Assessing fit for test data predictions
  
#Make RMSE for test data predictions.
BT2_aug_test %>%
  rmse(truth = BodyTemp, .pred)
  
#RMSE = 1.07

####RMSE for Body Temperature as a function of Runny Nose in Train Data.

#Predict outcomes using augment in training data.
BT2_aug_train <- 
  augment(BT2_fit, train_data)

#Calculate RMSE for training data.
BT2_aug_train %>%
  rmse(truth = BodyTemp, .pred)

#RMSE = 1.22
  
#The resulting RMSE values were 1.07 and 1.22 for the test data model and the 
#training data model, respectively. This suggests that there is a 
#low amount of variance between the model and both data sets. Therefore, 
#the models are good fits to our data and can predict outcomes for Body Temperature.
