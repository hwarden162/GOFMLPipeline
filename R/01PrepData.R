suppressMessages({
  library(tidymodels)
  library(tidyverse)
  library(themis)
})

ninewk_files <- list.dirs("./imagedata")[-1]

read_folder <- function(path, threshold = 3) {
  morph_data <- suppressMessages(read_csv(paste0(path, "/morphology.csv")))
  spat_data <- suppressMessages(read_csv(paste0(path, "/spatial.csv")))
  morph_data |> 
    left_join(spat_data, by = "Meta_Global_Mask_Label") |> 
    rename_with(~ str_replace_all(., "-", "")) |> 
    filter(
      QC_Global_Mask_SegVal != 0,
      Spatial_Nuclei_Spatial_LocalCounts200 > 5,
      AreaShape_Nuclei_Mask_Area > 20,
      Intensity_Cytoplasm_Secondary_MedianIntensity / Spatial_Object_Spatial_LocalMeansIntensityCytoplasmSecondaryMedianIntensity200 > threshold
    ) |> 
    select(
      Meta_Global_Mask_Label,
      QC_Global_Mask_SegVal,
      starts_with("AreaShape_Nuclei"),
      starts_with("Spatial_Nuclei_Spatial_LocalCounts"),
      starts_with("Spatial_Object_Spatial_LocalMeansAreaShapeNucleiMaskArea"),
      starts_with("Spatial_Object_Spatial_LocalMeansAreaShapeNucleiMaskEccentricity"),
      starts_with("Spatial_Object_Spatial_LocalMeansAreaShapeNucleiMaskAxisMinorLength"),
      starts_with("Spatial_Object_Spatial_LocalMeansAreaShapeNucleiMaskEccentricity")
    ) |> 
    select(
      -AreaShape_Nuclei_Mask_AreaFilled,
      -AreaShape_Nuclei_Mask_EulerNumber,
      -starts_with("AreaShape_Nuclei_Mask_InertiaTensor"),
      -starts_with("AreaShape_Nuclei_Mask_InertiaTensorEigvals")
    ) |> 
    mutate(
      Meta_ImagePath = path,
      GOF = str_match(path, "^[^_]*_([^_]*)_")[,2]
    ) |> 
    mutate(
      Training = str_detect(Meta_ImagePath, "658_Bcat_P53_9") | str_detect(Meta_ImagePath, "1024456_Kras_P53_21d_IHC")
    ) |> 
    select(
      Meta_Global_Mask_Label,
      Meta_ImagePath,
      GOF,
      Training,
      starts_with("Area"),
      starts_with("Spatial")
    )
}

full_data <- bind_rows(
  map(ninewk_files, read_folder)
)

full_data_train <- full_data |> 
  filter(Training) |> 
  select(-Training)

full_data_train <- recipe(GOF ~ ., data = full_data_train) |> 
  step_downsample(GOF) |> 
  prep() |> 
  juice()

full_data_test <- full_data |> 
  filter(!Training) |> 
  select(-Training)

full_data_test <- recipe(GOF ~ ., data = full_data_test) |> 
  step_downsample(GOF) |> 
  prep() |> 
  juice() |> 
  initial_split(prop = 0.5, strata = GOF)

full_data |> 
  write_csv("./data/full_data.csv")

full_data_train |> 
  write_csv("./data/full_data_train.csv")

full_data_test |> 
  training() |> 
  write_csv("./data/full_data_calibration.csv")

full_data_test |> 
  testing() |> 
  write_csv("./data/full_data_test.csv")

full_data_train |> 
  select(-starts_with("Spatial")) |> 
  write_csv("./data/area_data_train.csv")

full_data_test |> 
  training() |> 
  select(-starts_with("Spatial")) |> 
  write_csv("./data/area_data_calibration.csv")

full_data_test |> 
  testing() |> 
  select(-starts_with("Spatial")) |> 
  write_csv("./data/area_data_test.csv")

full_data_train |> 
  select(-starts_with("Area")) |> 
  write_csv("./data/spatial_data_train.csv")

full_data_test |> 
  training() |>
  select(-starts_with("Area")) |> 
  write_csv("./data/spatial_data_calibration.csv")

full_data_test |> 
  testing() |> 
  select(-starts_with("Area")) |> 
  write_csv("./data/spatial_data_test.csv")
