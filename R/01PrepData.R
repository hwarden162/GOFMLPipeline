suppressMessages({
  library(tidymodels)
  library(tidyverse)
})

ninewk_files <- c(
  "./imagedata/572_Kras_P53_9w_IHC_20210520_160048.ndpi",
  "./imagedata/572_Kras_P53_9w_IHC_20210520_160216.ndpi",
  "./imagedata/572_Kras_P53_9w_IHC_20210520_160411.ndpi",
  "./imagedata/657_Bcat_P53_9w_IHC_20211117_155258.ndpi",
  "./imagedata/658_Bcat_P53_9w_IHC_20211117_160348.ndpi",
  "./imagedata/659_Bcat_P53_9w_IHC_20211117_163154.ndpi"
)

read_folder <- function(path, threshold) {
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
      ImagePath = path,
      GOF = str_match(path, "^[^_]*_([^_]*)_")[,2]
    ) |> 
    mutate(
      Training = !(ImagePath %in% c("./imagedata/572_Kras_P53_9w_IHC_20210520_160411.ndpi", "./imagedata/657_Bcat_P53_9w_IHC_20211117_155258.ndpi"))
    ) |> 
    select(
      ImagePath,
      GOF,
      Training,
      starts_with("Area"),
      starts_with("Spatial")
    )
}

full_data <- bind_rows(
  read_folder(ninewk_files[1], 3),
  read_folder(ninewk_files[2], 3),
  read_folder(ninewk_files[3], 3),
  read_folder(ninewk_files[4], 2.25),
  read_folder(ninewk_files[5], 2.25),
  read_folder(ninewk_files[6], 2.25)
)

full_data_train <- full_data |> 
  filter(Training) |> 
  select(-Training, -ImagePath)

full_data_test <- full_data |> 
  filter(!Training) |> 
  select(-Training, -ImagePath) |> 
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
