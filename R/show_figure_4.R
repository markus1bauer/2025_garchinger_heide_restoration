#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Garchinger Heide
# NMDS ####
# Show figure 4
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sina Appeltauer, Markus Bauer
# 2025-03-03



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(vegan)

### Start ###
rm(list = setdiff(ls(), c("graph_a", "graph_b", "graph_c", "graph_d")))

### Functions ###
theme_mb <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    text = element_text(size = 9, color = "black"),
    strip.text = element_text(size = 10),
    axis.text = element_text(angle = 0, hjust = 0.5, size = 9,
                             color = "black"),
    axis.title = element_text(angle = 0, hjust = 0.5, size = 9,
                              color = "black"),
    axis.line = element_line(),
    legend.key = element_rect(fill = "white"),
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    legend.text = element_text(size = 10),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

vegan_cov_ellipse <- function(cov, center = c(0, 0), scale = 1, npoints = 100) {
  theta <- (0:npoints) * 2 * pi / npoints
  circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(circle %*% chol(cov)))
}

#### * Load data sites ####

sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types =
    cols(
      .default = "?"
      )
) %>%
  filter(is.na(location) | location != "rollfeld") %>%
  arrange(id)

#### * Choosen model ####

base::load(here("outputs", "models", "model_nmds.Rdata"))
ordi

base::load(here("outputs", "models", "model_nmds_envfit_vector1.Rdata"))
envfit <- ef_vector1

rm(
  list = setdiff(
    ls(), c("sites", "envfit", "ordi", "theme_mb", "vegan_cov_ellipse")
  )
)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#### * Preparation ####

data_envfit <- envfit %>%
  scores(display = "vectors") %>%
  as_tibble(rownames = NA) %>%
  rownames_to_column(var = "variable") %>%
  mutate(
    variable = as_factor(variable),
    variable = fct_recode(
      variable,
      "Vegetation\nheight" = "height_vegetation",
      "Vegetation\ncover" = "cover_vegetation",
      "Grass\ncover" = "grass_cover",
    )
  )

data_nmds <-  sites %>%
  select(id, treatment, esy) %>% # modify group
  mutate(
    group = as_factor(treatment), # modify group
    NMDS1 = ordi$points[, 1],
    NMDS2 = ordi$points[, 2]
  ) %>%
  group_by(group) %>%
  mutate(mean1 = mean(NMDS1), mean2 = mean(NMDS2))


#### * Site scores ####

(graph_a <- ggplot() +
   geom_point(
     aes(y = NMDS2, x = NMDS1, shape = esy, color = group, group = group),
     data = data_nmds,
     cex = 2, alpha = .8
   ) +
   
   #### * Ellipses ####

 stat_ellipse(
   aes(y = NMDS2, x = NMDS1, color = group, group = group),
   data = data_nmds,
   geom = "path", level = 0.95, show.legend = FALSE
 ) +
   ggrepel::geom_label_repel(
     aes(x = NMDS1, y = NMDS2, label = variable),
     data = data_envfit %>% filter(NMDS2 < 0 & NMDS1 > 0),
     fill = alpha("white", .6),
     size = 3,
     nudge_y = -.17,
     nudge_x = .1,
     min.segment.length = Inf
   ) +

   #### * Envfit variables ####

 geom_segment(
   data = data_envfit,
   aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
   arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
   color = "black",
   linewidth = 1, alpha = 1
 ) +
   coord_fixed() +

   #### * Design ####

 scale_color_manual(
   labels = c(
     "control" = "Reference",
     "cut_summer" = "Mowing\nsummer",
     "cut_autumn" = "Mowing\nautumn",
     "grazing" = "Topsoil\nremoval"
   ),
   values = c(
     "control" = "#f947d1", 
     "cut_summer" = "#61a161", 
     "cut_autumn" = "#87ceeb", 
     "grazing" = "#b06e13"
   )
 ) +
   scale_shape_manual(
     labels = c(
       "R22" = "R22:\nHay meadow",
       "R1A" = "R1A:\nSemi-dry grassland",
       "R" = "R:\nGeneral grassland"
     ),
     values = c(
       "R22" = 1,
       "R1A" = 16,
       "R" = 3
     )
   ) +
   scale_linetype_manual(values = c(1, 1, 1, 1, 1, 1, 1)) +
   labs(x = "NMDS1", y = "NMDS2", shape = "", color = "") +
   theme_mb() +
   theme(legend.key.height = unit(.9, "cm")))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# ggsave(
#   here("outputs", "figures", "figure_4_nmds_800dpi_16.5x11cm.tiff"),
#   dpi = 800, width = 16.5, height = 11, units = "cm"
# )
