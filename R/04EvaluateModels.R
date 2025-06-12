suppressMessages({
  library(caret)
  library(tidymodels)
  library(tidyverse)
  library(stacks)
})

full_data_test <- suppressMessages(read_csv("./data/full_data_test.csv"))
area_data_test <- suppressMessages(read_csv("./data/area_data_test.csv"))
spatial_data_test <- suppressMessages(read_csv("./data/spatial_data_test.csv"))

full_data_model <- readRDS("./models/full_data_model.rds")
area_data_model <- readRDS("./models/area_data_model.rds")
spatial_data_model <- readRDS("./models/spatial_data_model.rds")

make_confusion_mat <- function(data_test, data_model) {
  model_preds <- data_model |> 
    predict(data_test) |> 
    pull(.pred_class) |> 
    factor()
  
  confusionMatrix(model_preds, data_test |> pull(GOF) |> factor())
}

make_confusion_mat(full_data_test, full_data_model)
make_confusion_mat(area_data_test, area_data_model)
make_confusion_mat(spatial_data_test, spatial_data_model)
