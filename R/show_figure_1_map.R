#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Garchinger Heide
# Map ####
# Show figure 1
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2024-03-03



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
  summarise() %>%
  st_crop(xmin = 0, xmax = 19, ymin = 40, ymax = 55)
rivers <- rnaturalearth::ne_download(
  scale = 50, type = 'rivers_lake_centerlines', category = 'physical'
  ) %>%
  st_crop(xmin = 0, xmax = 19, ymin = 44, ymax = 55)
ecoregions <- st_read(here("data", "raw", "spatial", "ecoregions2017.shp")) %>%
  st_transform(crs = 4326) %>%
  filter(
    ECO_NAME == "Western European broadleaf forests" |
      ECO_NAME == "Central European mixed forests" |
      ECO_NAME == "European Atlantic mixed forests" |
      ECO_NAME == "Baltic mixed forests" |
      ECO_NAME == "Alps conifer and mixed forests"
  ) %>%
  st_crop(xmin = 0, xmax = 19, ymin = 44, ymax = 55)
elevation <- elevatr::get_elev_raster(
  locations = data.frame(x = c(0, 19), y = c(44, 55)),
  prj = "+proj=longlat +datum=WGS84 +no_defs",
  clip = "bbox", z = 5
) %>%
  terra::rast()



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



graph_sites <- ggplot() +
  tidyterra::geom_spatraster(data = elevation) +
  geom_sf(
    data = ecoregions, colour = "black", fill = "transparent", size = 1,
    linetype = "dashed"
  ) +
  geom_sf(data = rivers, color = "#38afcd", linewidth = 0.2) +
  geom_point(aes(x = 11.65286, y = 48.29032), size = 1, color = "red") +
  annotate(
    geom = "text", label = "Danube", x = 13.5, y = 49.2,
    angle = 335, color = "#38afcd", size = 1.2
  ) +
  coord_sf(xlim = c(0, 19), ylim = c(44, 55)) +
  ggspatial::annotation_north_arrow(
    which_north = "true",
    style = ggspatial::north_arrow_fancy_orienteering(text_size = 0),
    height = unit(.5, "cm"),
    width = unit(.5, "cm"),
    pad_y = unit(3.7, "cm"),
    pad_x = unit(.3, "cm")
  ) +
  tidyterra::scale_fill_hypso_tint_c(
    palette = "gmt_globe",
    labels = scales::label_number(),
    breaks = c(-1000, 0, 1000, 2000, 3000)
  ) +
  theme_mb() +
  theme(
    plot.background = element_blank(),
    legend.position = "none"
  )
graph_sites


## Save ####

# ggsave(
#   "figure_1a_map_300dpi_4x4cm.tiff",
#   dpi = 300, width = 4, height = 4, units = "cm",
#   path = here("outputs", "figures")
# )
