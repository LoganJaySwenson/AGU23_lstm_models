# 04. Post-process GEE covariates
library(lubridate)
library(tidyverse)

# Land cover percentages- NLCD 2019

landcover <- read_csv("data/spatial/GEE/landcover.csv", show_col_types=F) %>%
  select(-"system:index") %>%
  rename_all(tolower) 

# https://developers.google.com/earth-engine/datasets/catalog/USGS_NLCD_RELEASES_2019_REL_NLCD

landcover_types <- c(
  site_no = "site_no",
  class_sum = "sum",
  class_11 = "open_water", class_12 = "snow", 
  class_21 = "developed_open_space", class_22 = "developed_low_intensity", class_23 = "developed_medium_intensity", class_24 = "developed_high_intensity",
  class_31 = "barren_land", 
  class_41 = "deciduous_forest", class_42 = "evergreen_forest", class_43 = "mixed_forest", 
  class_51 = "dwarf_scrub", class_52 = "shrub",
  class_71 = "grassland", class_72 = "sedge", class_73 = "lichens", class_74 = "moss", 
  class_81 = "pasture", class_82 = "crops",
  class_90 = "woody_wetlands", class_95 = "emergent_wetlands")

colnames(landcover) <- sapply(colnames(landcover), function(x) landcover_types[x])

landcover <- landcover %>%
  rowwise() %>%
  mutate(
    urban = c_across(contains("developed")) %>% sum(),
    forest = c_across(contains("forest")) %>% sum(),
    other = c_across(c("snow", "barren_land", "shrub", "woody_wetlands", "emergent_wetlands")) %>% sum()) %>%
  select(site_no, crops, grassland, pasture, open_water, urban, forest, other)

dir_save <- "data_dir/attributes/"

for (i in colnames(landcover)[-1]){
  
  temp <- select(landcover, site_no, i)
  
  file_name <- paste0(dir_save, i, ".csv", sep = "")
  
  write_csv(temp, file = file_name)
}




# Geology percentages- US lithology
geology <- read_csv("data/spatial/GEE/geology.csv", show_col_types=F) %>%
  select(-"system:index") %>%
  rename_all(tolower) 
  
# https://developers.google.com/earth-engine/datasets/catalog/CSP_ERGo_1_0_US_lithology

geol_types <- c(
  site_no = "site_no",
  class_sum = "sum",
  class_0 = "water",
  class_1 = "carbonate",
  class_3 = "non_carbonate", 
  class_4 = "alkaline_intrusive", 
  class_5 = "silicic_residual",
  class_7 = "extrusive_volcanic",
  class_8 = "colluvial_sediment", 
  class_9 = "glacial_till_clay", 
  class_10 = "glacial_till_loam",
  class_11 = "glacial_till_coarse",
  class_13 = "glacial_lake_sediment_fine",
  class_14 = "glacial_outwash_coarse",
  class_15 = "hydric", 
  class_16 = "eolian_sediment_coarse", 
  class_17 = "eolian_sediment_fine",
  class_18 = "saline_lake_sediment",
  class_19 = "coastal_sediment_fine", 
  class_20 = "coastal_sediment_coarse")

colnames(geology) <- sapply(colnames(geology), function(x) geol_types[x])

geology <- geology %>%
  select(site_no, carbonate, non_carbonate, eolian_sediment_coarse, eolian_sediment_fine,
         water, glacial_till_loam, glacial_till_coarse, colluvial_sediment, 
         extrusive_volcanic, saline_lake_sediment, coastal_sediment_fine, silicic_residual)

dir_save <- "data_dir/attributes/"

for (i in colnames(geology)[-1]){
  
  temp <- select(geology, site_no, i)
  
  file_name <- paste0(dir_save, i, ".csv", sep = "")
  
  write_csv(temp, file = file_name)
}