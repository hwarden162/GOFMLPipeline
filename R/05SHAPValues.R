library(doParallel)
library(fastshap)
library(shapviz)
library(tidymodels)
library(tidyverse)
library(stacks)

registerDoParallel()

full_data_recipe <- readRDS("./models/full_data_recipe.rds")
area_data_recipe <- readRDS("./models/area_data_recipe.rds")
spatial_data_recipe <- readRDS("./models/spatial_data_recipe.rds")

full_data_train <- read_csv("./data/full_data_train.csv")
area_data_train <- read_csv("./data/area_data_train.csv")
spatial_data_train <- read_csv("./data/spatial_data_train.csv")

full_data_test <- read_csv("./data/full_data_test.csv")
area_data_test <- read_csv("./data/area_data_test.csv")
spatial_data_test <- read_csv("./data/spatial_data_test.csv")

full_data_model <- readRDS("./models/full_data_model.rds")
area_data_model <- readRDS("./models/area_data_model.rds")
spatial_data_model <- readRDS("./models/spatial_data_model.rds")

get_shap_vals <- function(data_recipe, data_train, data_test, data_model) {
  
  X <- data_test |> 
    select(-GOF) |> 
    as.matrix()
  
  predict_fn <- function(object, newdata) {
    preds <- predict(object, new_data = newdata, type = "prob")
    dplyr::pull(preds, ".pred_Kras")
  }
  
  vals <- fastshap::explain(
    object = data_model,
    X = X,
    pred_wrapper = predict_fn,
    newdata = X,
    nsim = 1000
  )
  
  baseline <- mean(predict_fn(data_model, newdata = data_train))
  
  shapviz(vals, X=X, baseline=baseline)
}

full_data_vals <- get_shap_vals(full_data_recipe, full_data_train, full_data_test, full_data_model)
area_data_vals <- get_shap_vals(area_data_recipe, area_data_train, area_data_test, area_data_model)
spatial_data_vals <- get_shap_vals(spatial_data_recipe, spatial_data_train, spatial_data_test, spatial_data_model)

full_data_vals |> 
  saveRDS("./data/full_shap_vals.rds")

area_data_vals |> 
  saveRDS("./data/area_shap_vals.rds")

spatial_data_vals |> 
  saveRDS("./data/spatial_shap_vals.rds")
