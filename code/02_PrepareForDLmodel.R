# 02. Prepare inputs for neuralhydrology models
library(dataRetrieval)
library(lubridate)
library(tidyverse)


# Generate all folders
dir.create("data_dir")
dir.create("data_dir/data")
dir.create("data_dir/time_series")
dir.create("data_dir/attributes")

# Set path to directories 
dir_save <- "data_dir/data/"

# Set dates
date_start <- as.Date("1979-10-01")
date_end <- as.Date("2023-09-30")
years <- year(seq(date_start, date_end, by="year"))




# Read: Gauges (n = 96)
Gauges <- read_csv("data/Gages.csv") 

# Gauges to model
site_no <- Gauges$site_no

write.table(site_no, file = "data_dir/Gauges.txt", row.names = F, col.names = F, quote = F)

# Select Gauges for fine-tuning
#Gauges_Kansas <- filter(Gauges, state == "KS") # (n = 28)
#site_no <- Gauges_Kansas$site_no
#write.table(site_no, file = "data_dir/Gauges_Kansas.txt", row.names = F, col.names = F, quote = F)




# Watershed attributes
drainage_area <- select(Gauges, "site_no", "drainage_area")
write_csv(drainage_area, file = "data_dir/attributes/drainage_area.csv", col_names = T)




# Flow at Gauges
site_no <- Gauges$site_no
flow <- tibble()

for (i in seq_along(site_no)) {
  
  temp <- as_tibble(readNWISdv(siteNumber = site_no[i], parameterCd = "00060", startDate = "1979-10-01", endDate = "2023-09-30")) %>%
    renameNWISColumns() %>%
    rename_with(.cols = everything(), tolower) %>%
    select(site_no, date, flow) 
  
  flow <- bind_rows(flow, temp)
}

flow <- flow %>%
  filter(date >= date_start & date <= date_end)

# flow from cfs --> m3/s and mm/d
norm_flow <- tibble()

for (i in seq_along(site_no)){
  
  temp <- flow[flow$site_no == site_no[i], ]
  
  area <- as.numeric(drainage_area[drainage_area$site_no == site_no[i], "drainage_area"])
  
  temp$flow <- temp$flow * 0.028316847  # cfs to m3/s
  temp$norm_flow <- ((temp$flow * 1e+9) / (area * 1e+10)) * 86400  # m3/s to mm/d
  
  norm_flow <- bind_rows(norm_flow, temp)
}
write_csv(norm_flow, file = "data/Gages_flow.csv", col_names = T)




# Meteorological variables 
meteorological_vars <- read_csv("data/Gages_meteorological_vars.csv") %>%
  rename(date = date_ymd) %>%
  mutate(date = ymd(date)) %>%
  filter(date >= date_start & date <= date_end)

inputs <- left_join(meteorological_vars, norm_flow, by = c("site_no", "date"))

site_no <- unique(inputs$site_no)

# Save inputs (forcings & target) for stream gauges across the High Plains Aquifer
for (i in seq_along(site_no)) {
  
  temp <- filter(inputs, site_no == site_no[i])
  
  file_path <- paste0(dir_save, site_no[i], ".csv")
  
  write_csv(temp, file_path)
}




# Climate attributes (i.e., average precip, ref ET, & aridity)
climate <- meteorological_vars %>%
  mutate(year = year(date)) %>%
  group_by(year, site_no) %>%
  summarize(annual_precip = sum(pr),
            annual_refET = sum(etr)) %>%
  group_by(site_no) %>%
  summarize(average_precip = mean(annual_precip),
            average_refET = mean(annual_refET),
            aridity = average_precip / average_refET)

write_csv(select(climate, site_no, average_precip),
          file = "data_dir/attributes/average_annual_precip.csv")

write_csv(select(climate, site_no, average_refET),
          file = "data_dir/attributes/average_annual_refET.csv")

write_csv(select(climate, site_no, aridity),
          file = "data_dir/attributes/aridity.csv")