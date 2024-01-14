# Figs 2-6. Evaluate model performance
library(sf)
library(scales)
library(viridis)
library(patchwork)
library(lubridate)
library(tidyverse)

# Pub theme
source("code/Theme+Settings.R")


# Fig 3. Nash-Sutcliffe efficiency (NSE) for modeled sites.

# Global coordinate ref
crs_proj <- '+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

states_sf <- st_read("data/spatial/states/states.shp", quiet=T) %>% st_transform(crs = crs_proj)

kansas_sf <- st_read("data/spatial/Kansas/Kansas_outline.shp", quiet=T) %>% st_transform(crs = crs_proj)

aquifer_sf <- st_read("data/spatial/high_plains_aquifer/high_plains_aquifer.shp", quiet=T) %>%
  st_transform(crs = crs_proj)

window <- aquifer_sf %>% 
  st_buffer(1e4) %>% 
  st_bbox()

GMDs <- st_read("data/spatial/GMDs/gmds.shp", quiet=T) %>% 
  st_transform(crs = crs_proj) %>%
  filter(NAME == "Equus Beds GMD #2" | NAME == "Northwest Kansas GMD #4")

GMD2 <- filter(GMDs, NAME == "Equus Beds GMD #2")
GMD4 <- filter(GMDs, NAME == "Northwest Kansas GMD #4")

metrics <- read_csv("data/test_metrics.csv")

Gauges_sf <- read_csv("data/Gages.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs_proj) %>%
  left_join(metrics, by = c("site_no" = "basin")) 

Gauges_sf$NSE_binned <- cut(Gauges_sf$NSE, breaks = c(-Inf, seq(0, 1, 0.25)))
ggplot()+
  geom_sf(data = states_sf, fill = NA, color = "#000000", linewidth = 0.5)+
  geom_sf(data = aquifer_sf, fill = "#377EB8", alpha = 0.5)+
  geom_sf(data = GMD2, fill = NA, color = "#000000", linewidth = 0.5)+
  geom_sf(data = GMD4, fill = NA, color = "#000000", linewidth = 0.5)+
  geom_sf(data = kansas_sf, fill = NA, color = "#000000", linewidth = 0.5)+
  geom_sf(data = Gauges_sf, aes(fill = NSE_binned), color = "#000000", pch = 21, size = 10)+
  labs(x = "", y = "")+
  labs(fill = "NSE")+
  guides(color = "none")+
  scale_fill_manual(values = c("#969696", "#fde725", "#35b779", "#31688e", "#440154"), 
                    labels = c("-Inf to 0", "0 to 0.25", "0.25 to 0.5", "0.5 to 0.75", "0.75 to 1"))+
  scale_x_continuous(expand = c(0,0), labels = function(x) paste0(x, '\u00B0', "W"))+
  scale_y_continuous(expand = c(0,0), labels = function(x) paste0(x, '\u00B0', "N"))+
  coord_sf(xlim = c(window$xmin, window$xmax),
           ylim = c(window$ymin, window$ymax)) +
  theme(panel.border = element_blank(),
        legend.position = "bottom",
        #legend.box = "vertical",
        axis.text = element_text(size = 18), legend.text = element_text(size = 36))
ggsave("figures/Fig3.png", dpi = 300, width = 14, height = 14, units = "in")




# Fig 4. Simulated streamflow at a site closest to the median NSE & at the best performing site.

flow <- read_csv("data/Gages_flow_sim.csv")

# Get modeled stream gauge closest to median performance
median_value <- median(Gauges_sf$NSE)

idx <- which.min(abs(Gauges_sf$NSE - median_value))

median_sample <- Gauges_sf[idx, ]

x <- median_sample$site_no
station_nm <- median_sample$station_nm
NSE <- round(median_sample$NSE, 2)
KGE <- round(median_sample$KGE, 2)

median_flow_sim <- filter(flow, site_no == x)

min_q <- 0.001
median_flow_sim$flow_sim_forlog <- median_flow_sim$flow_sim
median_flow_sim$flow_sim_forlog[median_flow_sim$flow_sim_forlog < min_q] <- min_q

ggplot()+
  geom_line(data = median_flow_sim, aes(date, flow_obs, color = "Observed"))+
  geom_line(data = median_flow_sim, aes(date, flow_sim_forlog, color = "Simulated"))+
  annotate("text", x = as.Date("2015-11-01"), y = Inf, hjust = 0, vjust = 1, 
           label = paste("Little White River near Martin, SD\nUSGS 06447500\nNSE:", NSE), size = 18/.pt)+
  labs(x = "", y = "Discharge (mm/d)")+
  labs(color = "")+
  scale_color_manual(name = "", values = c("Observed" = "#000000", "Simulated" = "#377EB8"))+
  guides(color = "none")+
  scale_x_date(expand = c(0,0), limit=c(as.Date("2015-10-01"),as.Date("2023-10-01")),
               breaks=date_breaks("12 months"), labels=date_format("%b %y"))+
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(0.01, 100),
    expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        plot.margin=unit(c(5,5,5,5), "mm"))
ggsave("figures/Fig4a.png", dpi = 300, width = 10.4, height = 6.5, units = "in")


# Get modeled stream gauge with best performance
max_value <- max(Gauges_sf$NSE)

idx <- which(Gauges_sf$NSE == max_value)

max_sample <- Gauges_sf[idx, ]

x <- max_sample$site_no
station_nm <- max_sample$station_nm
NSE <- round(max_sample$NSE, 2)
KGE <- round(max_sample$KGE, 2)

max_flow_sim <- filter(flow, site_no == x)

min_q <- 0.001
max_flow_sim$flow_sim_forlog <- max_flow_sim$flow_sim
max_flow_sim$flow_sim_forlog[max_flow_sim$flow_sim_forlog < min_q] <- min_q

ggplot()+
  geom_line(data = max_flow_sim, aes(date, flow_obs, color = "Observed"))+
  geom_line(data = max_flow_sim, aes(date, flow_sim_forlog, color = "Simulated"))+
  annotate("text", x = as.Date("2015-11-01"), y = Inf, hjust = 0, vjust = 1, 
           label = paste("Little Arkansas River near Sedgwick, KS\nUSGS 07144100\nNSE:", NSE), size = 18/.pt)+
  labs(x = "", y = "Discharge (mm/d)")+
  labs(color = "")+
  scale_color_manual(name = "", values = c("Observed" = "#000000", "Simulated" = "#377EB8"))+
  guides(color = "none")+
  scale_x_date(expand = c(0,0), limit=c(as.Date("2015-10-01"),as.Date("2023-10-01")),
               breaks=date_breaks("12 months"), labels=date_format("%b %y"))+
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x)),
    limits = c(0.01, 10000),
    expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        plot.margin=unit(c(5,5,5,5), "mm"))
ggsave("figures/Fig4b.png", dpi = 300, width = 10.4, height = 6.5, units = "in")




# Fig 5. Pearson correlation (ρ) between observed daily flow at each gauge. 
flow <- read_csv("data/Gages_flow.csv") %>%
  select(site_no, date, norm_flow)

flow <- flow %>%
  mutate(site_no = as.numeric(factor(site_no, levels = unique(site_no))))

flow_wide <- flow %>%
  pivot_wider(names_from = site_no, values_from = norm_flow)

corr_cols <- select(flow_wide, -date)

# compute correlation matrix
flow_corr <- round(cor(corr_cols, use = "pairwise.complete.obs"), digits=2)

flow_corr_long <- as_tibble(flow_corr) %>% rownames_to_column() %>% pivot_longer(-1) %>%
  mutate(name = as.numeric(name), rowname = as.numeric(rowname))

ggplot()+
  geom_tile(data = flow_corr_long, aes(name, rowname, fill=value))+
  coord_equal()+
  scale_fill_viridis(name = "\u03C1", option="magma")+
  scale_x_continuous(expand = c(0,0), breaks = seq(0, 96, 10), labels = seq(0, 96, 10))+
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 96, 10), labels = seq(0, 96, 10))+
  labs(x = "Stream Gauge", y = "Stream Gauge")+
  theme(axis.text = element_text(size = 18),
        legend.title = element_text(size = 28),
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        legend.text = element_text(size = 18))
ggsave("figures/Fig5.png", dpi = 300, width = 8, height = 8, units = "in")




# Fig 6. Pearson correlation (ρ) between NSE and catchment climatic and physiographic attributes.

dir_save <- "data_dir/attributes/"

covariates <- c("crops", "grassland", "pasture", "open_water", "urban", "forest", "other", 
                "carbonate", "non_carbonate", 
                "eolian_sediment_coarse", "eolian_sediment_fine",
                "glacial_till_coarse", "glacial_till_loam",
                "saline_lake_sediment", "extrusive_volcanic",
                "colluvial_sediment", 
                "coastal_sediment_fine", 
                "silicic_residual",
                "depletion_time",
                "elevation",
                "average_annual_precip",
                "average_annual_refET",
                "aridity")

for (i in seq_along(covariates)){
  
  file_path <- paste0(dir_save, covariates[i], ".csv")
  
  temp <- read_csv(file_path)
  
  Gauges_sf <- left_join(Gauges_sf, temp, by = "site_no")
}

# Focus comparisons on stream gauges with NSE > 0
Gauges_sf <- filter(Gauges_sf, NSE > 0)

Gauges_sf <- mutate(Gauges_sf, drainage_area = drainage_area/1e+6)

features <- c("drainage_area", "elevation", "average_precip", "aridity", "crops", "grassland")

labels <- c("Drainage area", "Elevation", "Precip", "Aridity", "Crops", "Grassland")

p1 <-
  ggplot() +
  geom_smooth(data = Gauges_sf, aes(drainage_area, NSE), method = "lm", se = T, color = "blue") +
  geom_point(data = Gauges_sf, aes(drainage_area, NSE), color = "#000000", fill = "#636363", pch = 21, size = 5) +
  annotate("text", x = 0, y = Inf, hjust = 0, vjust = 1, fontface = "bold", size = 18/.pt, 
           label = paste("\u03C1: ", round(cor(Gauges_sf$drainage_area, Gauges_sf$NSE, method = "pearson"), 2)), parse = T) +
  labs(x = "Drainage area", y = "NSE") +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_y_continuous(limits = c(0, 0.82), breaks = c(seq(0, 0.8, 0.2)))+
  theme(axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        plot.margin=unit(c(5,5,5,5), "mm"))

p2 <-
  ggplot() +
  geom_smooth(data = Gauges_sf, aes(elevation, NSE), method = "lm", se = T, color = "blue") +
  geom_point(data = Gauges_sf, aes(elevation, NSE), color = "#000000", fill = "#636363", pch = 21, size = 5) +
  annotate("text", x = -Inf, y = Inf, hjust = 0, vjust = 1, fontface = "bold", size = 18/.pt,
           label = paste("\u03C1: ", round(cor(Gauges_sf$elevation, Gauges_sf$NSE, method = "pearson"), 2)), parse = T) +
  labs(x = "Elevation", y = "NSE") +
  scale_x_continuous(limits = c(300, 1500), breaks = c(seq(300, 1500, 300)))+
  scale_y_continuous(limits = c(0, 0.82), breaks = c(seq(0, 0.8, 0.2)))+
  theme(axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        plot.margin=unit(c(5,5,5,5), "mm"))


p3 <-
  ggplot() +
  geom_smooth(data = Gauges_sf, aes(average_precip, NSE), method = "lm", se = T, color = "blue") +
  geom_point(data = Gauges_sf, aes(average_precip, NSE), color = "#000000", fill = "#636363", pch = 21, size = 5) +
  annotate("text", x = -Inf, y = Inf, hjust = 0, vjust = 1, fontface = "bold", size = 18/.pt,
           label = paste("\u03C1: ", round(cor(Gauges_sf$average_precip, Gauges_sf$NSE, method = "pearson"), 2)), parse = T)+
  labs(x = "Precipitation", y = "NSE") +
  scale_y_continuous(limits = c(0, 0.82), breaks = c(seq(0, 0.8, 0.2)))+
  theme(axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        plot.margin=unit(c(5,5,5,5), "mm"))

p4 <-
  ggplot() +
  geom_smooth(data = Gauges_sf, aes(aridity, NSE), method = "lm", se = T, color = "blue") +
  geom_point(data = Gauges_sf, aes(aridity, NSE), color = "#000000", fill = "#636363", pch = 21, size = 5) +
  annotate("text", x = -Inf, y = Inf, hjust = 0, vjust = 1, fontface = "bold", size = 18/.pt, 
           label = paste("\u03C1: ", round(cor(Gauges_sf$aridity, Gauges_sf$NSE, method = "pearson"), 2)), parse = T)+
  labs(x = "Aridity", y = "NSE") +
  scale_x_continuous(limits = c(0.2, 0.5), breaks = c(seq(0.2, 0.5, 0.1)))+
  scale_y_continuous(limits = c(0, 0.82), breaks = c(seq(0, 0.8, 0.2)))+
  theme(axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        plot.margin=unit(c(5,5,5,5), "mm"))

p5 <-
  ggplot() +
  geom_smooth(data = Gauges_sf, aes(crops, NSE), method = "lm", se = T, color = "blue") +
  geom_point(data = Gauges_sf, aes(crops, NSE), color = "#000000", fill = "#636363", pch = 21, size = 5) +
  annotate("text", x = -Inf, y = Inf, hjust = 0, vjust = 1, fontface = "bold", size = 18/.pt, 
           label = paste("\u03C1: ", round(cor(Gauges_sf$crops, Gauges_sf$NSE, method = "pearson"), 2)), parse = T)+
  labs(x = "Crops", y = "NSE") +
  scale_x_continuous(limits = c(0, 1.0), breaks = c(seq(0, 1.0, 0.25)))+
  scale_y_continuous(limits = c(0, 0.82), breaks = c(seq(0, 0.8, 0.2)))+
  theme(axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        plot.margin=unit(c(5,5,5,5), "mm"))

p6 <-
  ggplot() +
  geom_smooth(data = Gauges_sf, aes(grassland, NSE), method = "lm", se = T, color = "blue") +
  geom_point(data = Gauges_sf, aes(grassland, NSE), color = "#000000", fill = "#636363", pch = 21, size = 5) +
  annotate("text", x = -Inf, y = Inf, hjust = 0, vjust = 1, fontface = "bold", size = 18/.pt, 
           label = paste("\u03C1: ", round(cor(Gauges_sf$grassland, Gauges_sf$NSE, method = "pearson"), 2)), parse = T)+
  labs(x = "Grassland", y = "NSE") +
  scale_x_continuous(limits = c(0, 1.0), breaks = c(seq(0, 1.0, 0.25)))+
  scale_y_continuous(limits = c(0, 0.82), breaks = c(seq(0, 0.8, 0.2)))+
  theme(axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 28),
        axis.title.y = element_text(size = 28),
        plot.margin=unit(c(5,5,5,5), "mm"))

pp <- list(p1, p2, p3, p4, p5, p6)
cowplot::plot_grid(plotlist = pp, ncol = 3)
ggsave("figures/Fig6.png", dpi = 300, width = 12, height = 12, units = "in")