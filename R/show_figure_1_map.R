#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Garchinger Heide
# Show figure 1 ####
# Map
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2024-02-18



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation #########################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)

### Start ###
rm(list = ls())



## 1 Load data ################################################################


europe <- rnaturalearth::ne_countries(scale = 50, continent = "Europe") %>%
  summarise()
rivers <- rnaturalearth::ne_download(
  scale = 50, type = 'rivers_lake_centerlines', category = 'physical'
)
elevation <- europe %>%
  st_coordinates() %>%
  data.frame() %>%
  select(X, Y) %>%
  st_as_sf(coords = c(1,2)) %>%
  st_set_crs(4326) %>%
  elevatr::get_elev_raster(z = 5, clip = "locations") %>%
  raster::as.data.frame(xy = TRUE) %>%
  rename(elevation = 3)



## 2 Load theme ################################################################


theme_mb <- function() {
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid = element_line(colour = NA),
    text = element_text(size = 10, color = "black"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(.5, 0, 0, 0, "cm")
  )
}



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



ggplot() +
  geom_tile(data = elevation, aes(x = x, y = y, fill = elevation)) +
  geom_sf(data = europe, color = "black", fill = "transparent", linewidth = .2) +
  geom_sf(data = rivers, color = "#38afcd", linewidth = 0.2) +
  # annotate(
  #   geom = "text", label = "Rhine", x = 8, y = 51.3,
  #   angle = 300, color = "#38afcd"
  #   ) +
  # annotate(
  #   geom = "text", label = "Danube", x = 14.2, y = 49.2,
  #   angle = 345, color = "#38afcd"
  # ) +
  coord_sf(xlim = c(0, 19), ylim = c(44, 55)) +
  geom_point(aes(x = 11.65286, y = 48.29032), size = .3, color = "red") +
  geom_label(aes(x = 13.365, y = 52.523), label = "B", shape = 0, size = 1.2) +
  geom_label(aes(x = 2.3442, y = 48.8543), label = "P", shape = 0, size = 1.2) +
  geom_label(aes(x = 16.3882, y = 48.21), label = "V", shape = 0, size = 1.2) +
  marmap::scale_fill_etopo() +
  theme_mb() +
  theme(
    plot.background = element_blank(),
    legend.position = "none"
  )

ggsave(
  "figure_1a_map_ggplot_800dpi_2x2cm.tiff",
  dpi = 800, width = 2, height = 2, units = "cm",
  path = here("outputs", "figures")
)
