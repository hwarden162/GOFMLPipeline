library(tidymodels)
library(tidyverse)

full_data_train <- read_csv("./data/full_data_train.csv")
area_data_train <- read_csv("./data/area_data_train.csv")
spatial_data_train <- read_csv("./data/spatial_data_train.csv")

full_data_recipe <- readRDS("./models/full_data_recipe.rds")
area_data_recipe <- readRDS("./models/area_data_recipe.rds")
spatial_data_recipe <- readRDS("./models/spatial_data_recipe.rds")

train_model <- function(data_train, data_recipe) {
  data_folds <- vfold_cv(data_train, v=5, strata=GOF)
  
  rfrst_model <- rand_forest(
    mtry = tune(),
    trees = tune(),
    min_n = tune()
  ) |> 
    set_mode("classification") |> 
    set_engine("ranger", importance = "impurity")
  
  data_wf <- workflow() |> 
    add_recipe(data_recipe) |> 
    add_model(rfrst_model)
  
  tuning_params <- rfrst_model |> 
    extract_parameter_set_dials() |> 
    finalize(data_train)
  
  tuning_grid <- grid_regular(
    tuning_params,
    levels = 5
  )
  
  data_model_tuning <- data_wf |> 
    tune_grid(
      data_folds, 
      tuning_grid
    )
  
  best_params <- data_model_tuning |> 
    select_best("roc_auc")
  
  data_wf_fitted <- data_wf |> 
    finalize_workflow(best_params) |> 
    fit(data_train)
  
  data_wf_fitted
}

full_data_model <- train_model(full_data_train, full_data_recipe)
area_data_model <- train_model(area_data_train, area_data_recipe)
spatial_data_model <- train_model(spatial_data_train, spatial_data_recipe)

full_data_model |> 
  saveRDS("./models/full_data_model.rds")

area_data_model |> 
  saveRDS("./models/area_data_model.rds")

spatial_data_model |> 
  saveRDS("./models/spatial_data_model.rds")
