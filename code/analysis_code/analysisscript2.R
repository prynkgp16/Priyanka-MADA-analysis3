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
```


#path to data

```{r

data_location1 <- here::here("data","processed_data","processeddata.rds")

```

#load data. 

```{r
cleaneddata2 <- readRDS(data_location1)
```

#SPliting data :a training set and a testing set

# Put 3/4 of the data into the training set 


data_split1 <- initial_split(cleaneddata2, prop = 3/4)


# Create data frames for the two sets:


train_data <- training(data_split1)
test_data  <- testing(data_split1)


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