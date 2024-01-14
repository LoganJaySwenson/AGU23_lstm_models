# Fig 1. USGS stream gauges (n = 96) overlying the High Plains Aquifer with at least 20 years of data used in our study.
library(sf)
library(terra)
library(lubridate)
library(tidyverse)

# Pub theme
source("code/Theme+Settings.R")

# Global coordinate ref
crs_proj <- '+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

states_sf <- st_read("data/spatial/states/states.shp", quiet=T) %>% st_transform(crs = crs_proj)

kansas_sf <- st_read("data/spatial/Kansas/Kansas_outline.shp", quiet=T) %>% st_transform(crs = crs_proj)

aquifer_sf <- st_read("data/spatial/high_plains_aquifer/high_plains_aquifer.shp", quiet=T) %>%
  st_transform(crs = crs_proj)

GMDs <- st_read("data/spatial/GMDs/gmds.shp", quiet=T) %>% 
  st_transform(crs = crs_proj) %>%
  filter(NAME == "Equus Beds GMD #2" | NAME == "Northwest Kansas GMD #4")

GMD2 <- filter(GMDs, NAME == "Equus Beds GMD #2")
GMD4 <- filter(GMDs, NAME == "Northwest Kansas GMD #4")

Gauges_sf <- read_csv("data/Gages.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs_proj) 

window <- aquifer_sf %>% 
  st_buffer(1e4) %>% 
  st_bbox()

irrigation <- rast("data/spatial/irrigated_area/high_plains_aquifer_irrigated_area_2015-2017.tif")
irrigation <- project(irrigation, crs_proj, res = 450)
irrigation <- crop(irrigation, aquifer_sf)
irrigation <- mask(irrigation, aquifer_sf)

irrigation <- terra::as.data.frame(irrigation, xy=T, na.rm=F)

colnames(irrigation) <- c("lon", "lat", "irrigation")

irrigation <- irrigation %>%
  mutate(irrigation = ifelse(irrigation > 0, 1, NA))

# Plot!
ggplot()+
  geom_raster(data = irrigation, aes(x = lon, y = lat, fill = factor(irrigation)))+
  guides(fill = "none")+
  scale_fill_manual(values = c("darkgoldenrod1"), na.value = "transparent") +
  ggnewscale::new_scale_fill()+
  geom_sf(data = states_sf, fill = NA, color = "#000000", linewidth = 0.5)+
  geom_sf(data = aquifer_sf, fill = "#377EB8", alpha = 0.5)+
  geom_sf(data = GMD2, fill = NA, color = "#000000", linewidth = 0.5)+
  geom_sf(data = GMD4, fill = NA, color = "#000000", linewidth = 0.5)+
  geom_sf(data = kansas_sf, fill = NA, color = "#000000", linewidth = 0.5)+
  geom_sf(data = Gauges_sf, aes(fill = "USGS Stream Gauges (n = 96)"), color = "#000000", pch = 21, size = 6)+
  labs(x = "", y = "")+
  scale_fill_manual(name = "", values = c("USGS Stream Gauges (n = 96)" = "purple"))+
  scale_x_continuous(expand = c(0,0), labels = function(x) paste0(x, '\u00B0', "W"))+
  scale_y_continuous(expand = c(0,0), labels = function(x) paste0(x, '\u00B0', "N"))+
  coord_sf(xlim = c(window$xmin, window$xmax),
           ylim = c(window$ymin, window$ymax)) +
  theme(panel.border = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(size = 18), legend.text = element_text(size = 36))
ggsave("figures/Fig1.png", dpi = 300, width = 14, height = 14, units = "in")