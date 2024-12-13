#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Garchinger Heide
# Show figure 1 ####
# Map
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus bauer
# 2024-12-13



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


world <- ne_countries(scale = 110) 



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

europe <- ne_countries(scale = 50, continent = "Europe") %>%
  summarise()
rivers <- ne_download(
  scale = 50, type = 'rivers_lake_centerlines', category = 'physical'
  )

graph_europe <- ggplot() +
  geom_sf(data = europe, color = "black", fill = "grey95", linewidth = .2) +
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
  geom_point(aes(x = 11.65286, y = 48.29032), size = .3) +
  geom_label(aes(x = 13.365, y = 52.523), label = "B", shape = 0, size = 1.2) +
  geom_label(aes(x = 2.3442, y = 48.8543), label = "P", shape = 0, size = 1.2) +
  geom_label(aes(x = 16.3882, y = 48.21), label = "W", shape = 0, size = 1.2) +
  theme_mb() +
  theme(
    plot.background = element_blank()
  );graph_europe

### c Save --------------------------------------------------------------

ggsave(
  "figure_1_map_ggplot_300dpi_2x2cm.tiff",
  dpi = 300, width = 2, height = 2, units = "cm",
  path = here("outputs", "figures")
)
