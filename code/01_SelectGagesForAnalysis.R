# 01. Get a subset of USGS stream gauges in the High Plains Aquifer (n = 96) for modeling studies
library(sf)
library(lubridate)
library(dataRetrieval)
library(tidyverse)


# 1. Get a list of stream gauges in the High Plains Aquifer
states <- c("SD", "WY", "NE", "CO", "KS", "OK", "NM", "TX")
aquifer <- st_read("data/spatial/high_plains_aquifer/high_plains_aquifer.shp", quiet=T) %>% st_transform(crs = 4326)

Gauges <- tibble()

for (i in seq_along(states)) {
  temp <- as_tibble(whatNWISsites(stateCd = states[i],
                                  parameterCd = "00060",
                                  outputDataTypeCd = "dv")) %>% 
    rename_with(.cols = everything(), tolower) %>%
    rename(lat = dec_lat_va, 
           lon = dec_long_va) %>%
    select(site_no, station_nm, lat, lon)
  
  Gauges <- bind_rows(Gauges, temp)
}

Gauges <- st_as_sf(Gauges, coords = c("lon", "lat"), crs = 4326) 

Gauges <- st_filter(Gauges, aquifer)




# 2. Get watershed boundaries & area for stream gauges in the High Plains Aquifer

site_no <- Gauges$site_no
dir_save <- "data/spatial/high_plains_aquifer_watersheds/"

# Get watershed for stream gauges
for(i in 1:length(site_no)){
  nldiURL <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-",site_no[i],"/basin")
  
  try(nldi_data <- sf::read_sf(nldiURL))
  
  no_holes <- nngeo::st_remove_holes(nldi_data) # remove internal closed basins 
  
  try(st_write(nldi_data, paste0(dir_save, site_no[i], ".shp"), append = F))
}

site_files <- list.files(dir_save, pattern = "\\.shp", full.names = T)

areas <- tibble(site_no = site_no, drainage_area = rep(NA, length(site_files)))

# Get drainage area for stream gauges
for (i in seq_along(site_files)) {
  
  file <- site_files[i]
  
  if (st_is_valid(st_read(file))) {
    
    areas$drainage_area[i] <- st_area(st_read(file))
    
  } else {
    
    areas$drainage_area[i] <- NA
  }
}

Gauges <- Gauges %>%
  left_join(areas, by = c("site_no" = "site_no")) %>%
  filter(!is.na(drainage_area)) %>%
  arrange(drainage_area)




# 3. Get start & end of flow records for stream gauges in the High Plains Aquifer

site_no <- Gauges$site_no
flow <- tibble()

for (i in seq_along(site_no)) {
  temp <- as_tibble(readNWISdv(siteNumber = site_no[i], parameterCd = "00060", startDate = "1979-10-01")) %>%
    renameNWISColumns() %>%
    rename_with(.cols = everything(), tolower) 
  
  flow <- rbind(flow, temp)
}

flow <- select(flow, site_no, date, flow)

flow_wider <- pivot_wider(flow, names_from = site_no, values_from = flow)

# Get start and end dates for stream gauges
start_dates <- flow_wider %>% 
  summarise(across(starts_with("0"), ~ min(as.Date(date[!is.na(.)]), na.rm = T))) %>% 
  pivot_longer(cols = starts_with("0"), names_to = "site_no", values_to = "start") 

end_dates <- flow_wider %>% 
  summarise(across(starts_with("0"), ~ max(as.Date(date[!is.na(.)]), na.rm = T))) %>% 
  pivot_longer(cols = starts_with("0"), names_to = "site_no", values_to = "end") 

Gauges <- inner_join(Gauges, start_dates, by = "site_no")
Gauges <- inner_join(Gauges, end_dates, by = "site_no")

Gauges <- Gauges %>%
  mutate(lon = unlist(map(Gauges$geometry,1)),
         lat = unlist(map(Gauges$geometry,2))) %>%
  select(site_no, station_nm, lon, lat, drainage_area, start, end)




# 4. Select a subset of stream gauges for modeling studies & prepare inputs for Google Earth Engine

Gauges_sf <- read_csv("data/Gages.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(record = as.numeric(difftime(end, start, units = "days")/365.25)) %>%
  filter(record > 20 & year(end) == 2023)

states_sf <- st_read("data/spatial/states/states.shp", quiet=T) %>% 
  st_transform(crs = 4326) %>%
  rename_with(.cols = everything(), tolower) 

Gauges_sf <- st_join(Gauges_sf, states_sf %>% select(state_abbr), join = st_within) %>%
  mutate(drainage_area = drainage_area/1e+6) 

Gauges <- Gauges_sf %>%
  mutate(lon = unlist(purrr::map(Gauges_sf$geometry,1)),
         lat = unlist(purrr::map(Gauges_sf$geometry,2))) %>%
  tibble() %>%
  select(-geometry) 

write_csv(Gauges, "data/Gages.csv")




# 5. Combine watersheds for each stream gauge (Google Earth Engine)
site_no <- Gauges_sf$site_no
dir_watersheds <- "data/spatial/high_plains_aquifer_watersheds/"

watersheds <- list()

for (i in site_no) {
  
  watershed_path <- file.path(dir_watersheds, paste0(i, ".shp"))
  
  watershed <- st_read(watershed_path)
  
  watersheds[[i]] <- watershed
}

# export multi-watershed shapefile
watersheds <- do.call(rbind, watersheds)

watersheds$site_no <- rownames(watersheds) # Set site no as FID

# Remove the FID column
watersheds$FID <- NULL

# Remove the row names
rownames(watersheds) <- NULL

watersheds %>% st_transform(crs=4326) %>%
  select(site_no, geometry)

st_write(watersheds, "data/spatial/GEE/watersheds.shp", append=F)