library(tidymodels)
library(tidyverse)
library(stacks)

GRID_LEVELS <- 25

get_elastic_nets <- function(data_folds, data_recipe, ctrl_grid) {
  
  elast_model <- logistic_reg(
    penalty = tune(),
    mixture = tune()
  ) |> 
    set_engine("glmnet")
  
  elast_wf <- workflow() |> 
    add_recipe(data_recipe) |> 
    add_model(elast_model)
  
  elast_res <- tune_grid(
    elast_wf,
    resamples = data_folds,
    grid = GRID_LEVELS,
    control = ctrl_grid
  )
  
  elast_res
}

get_knns <- function(data_folds, data_recipe, ctrl_grid) {
  
  knn_model <- nearest_neighbor(
    neighbors = tune()
  ) |> 
    set_mode("classification") |> 
    set_engine("kknn")
  
  knn_wf <- workflow() |> 
    add_recipe(data_recipe) |> 
    add_model(knn_model)
  
  knn_res <- tune_grid(
    knn_wf,
    resamples = data_folds,
    grid = GRID_LEVELS,
    control = ctrl_grid
  )
  
  knn_res
}

get_xgbs <- function(data_folds, data_train, data_recipe, ctrl_grid) {
  xgb_model <- boost_tree(
    tree_depth = tune(),      # This is your max depth
    learn_rate = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    mtry = tune()
  ) |> 
    set_engine("xgboost") |> 
    set_mode("classification")
  
  tuning_params <- xgb_model |> 
    extract_parameter_set_dials() |> 
    finalize(data_train)
  
  xgb_wf <- workflow() |> 
    add_recipe(data_recipe) |> 
    add_model(xgb_model)
  
  tuning_grid <- grid_regular(
    tuning_params,
    levels = GRID_LEVELS
  )
  
  xgb_res <- xgb_wf |> 
    tune_grid(
      data_folds, 
      tuning_grid,
      control = ctrl_grid
    )
  
  xgb_res
}

train_model <- function(data_train, data_recipe) {
  data_folds <- vfold_cv(data_train, v=5, repeats = 1, strata = GOF)
  
  ctrl_grid <- control_stack_grid()
  
  elastic_res <- get_elastic_nets(data_folds, data_recipe, ctrl_grid)
  knn_res <- get_knns(data_folds, data_recipe, ctrl_grid)
  xgb_res <- get_xgbs(data_folds, data_train, data_recipe, ctrl_grid)
  
  model_stack <- stacks() |> 
    add_candidates(elastic_res) |> 
    add_candidates(knn_res) |> 
    add_candidates(xgb_res) |> 
    blend_predictions() |> 
    fit_members()
  
  model_stack
}
