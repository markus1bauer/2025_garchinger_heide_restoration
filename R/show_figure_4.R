#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Garchinger Heide
# NMDS ####
# Show figure 4
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sina Appeltauer, Markus Bauer
# 2024-10-09



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
)

#### * Choosen model ####

base::load(here("outputs", "models", "model_nmds.Rdata"))
ordi

base::load(here("outputs", "models", "model_nmds_envfit_vector.Rdata"))
data_envfit <- ef_vector2

rm(
  list = setdiff(
    ls(), c("sites", "data_envfit", "ordi", "theme_mb", "vegan_cov_ellipse")
  )
)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#### * Preparation ####

data_envfit <- data_envfit %>%
  scores(display = "vectors") %>%
  as_tibble(rownames = NA) %>%
  rownames_to_column(var = "variable") %>%
  mutate(
    variable = as_factor(variable),
    variable = fct_recode(
      variable,
      "Graminoid cover" = "graminoid_cover_ratio",
      "Ruderal cover" = "ruderal_cover",
      "Specialist richness" = "ellenberg_richness",
      "Survey year" = "survey_year"
    )
  )

data_nmds <-  sites %>%
  select(id, esy, exposition) %>% # modify group
  mutate(
    group_type = as_factor(esy), # modify group
    NMDS1 = ordi$points[, 1],
    NMDS2 = ordi$points[, 2]
  ) %>%
  group_by(group_type) %>%
  mutate(mean1 = mean(NMDS1), mean2 = mean(NMDS2))


#### * Site scores ####

(graph_a <- ggplot() +
   geom_point(
     aes(y = NMDS2, x = NMDS1, color = group_type, shape = group_type),
     data = data_nmds,
     cex = 2
   ) +
   
   #### * Ellipses ####
 
 stat_ellipse(
   aes(color = group, group = group),
   geom = "path", level = 0.95, show.legend = FALSE
 ) +
   ggrepel::geom_label_repel(
     aes(x = NMDS1, y = NMDS2, label = variable),
     data = data_envfit %>% filter(NMDS2 < 0 & NMDS1 < 0),
     fill = alpha("white", .7),
     size = 3,
     nudge_y = -.1,
     nudge_x = -.1,
     min.segment.length = Inf
   ) +
   ggrepel::geom_label_repel(
     aes(x = NMDS1, y = NMDS2, label = variable),
     data = data_envfit %>% filter(NMDS2 > 0),
     fill = alpha("white", .7),
     size = 3,
     nudge_y = .1,
     min.segment.length = Inf
   ) +
   ggrepel::geom_label_repel(
     aes(x = NMDS1, y = NMDS2, label = variable),
     data = data_envfit %>% filter(NMDS2 < 0 & NMDS1 > 0 & NMDS2 > -.2),
     fill = alpha("white", .7),
     size = 3,
     nudge_y = -.02,
     nudge_x = .17,
     min.segment.length = Inf
   ) +
   ggrepel::geom_label_repel(
     aes(x = NMDS1, y = NMDS2, label = variable),
     data = data_envfit %>% filter(NMDS2 < -.2 & NMDS1 > 0),
     fill = alpha("white", .7),
     size = 3,
     nudge_y = -.17,
     nudge_x = .1,
     min.segment.length = Inf
   ) +
   
   #### * Envfit variables ####
 
 geom_segment(
   data = data_envfit %>% filter(variable != "Survey year"),
   aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
   arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
   color = "black",
   linewidth = 1
 ) +
   geom_segment(
     data = data_envfit %>% filter(variable == "Survey year"),
     aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
     arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
     color = "black",
     linewidth = .8,
     linetype = "dotted"
   ) +
   geom_label(
     aes(x = mean1, y = mean2, label = group_type, fill = group_type),
     data = data_nmds %>%
       filter(group_type != "?") %>%
       group_by(group_type) %>%
       slice(1),
     color = "white",
     size = 3,
     show.legend = FALSE
   ) +
   coord_fixed() +
   
   #### * Design ####
 
 scale_color_manual(
   labels = c(
     "R22" = "R22:\nHay meadow",
     "R1A" = "R1A:\nSemi-dry grassland",
     "R" = "R:\nGeneral grassland",
     "V38" = "V38:\nDry anthropogenic\nvegetation",
     "?" = "No classification"
   ),
   values = c(
     "R22" = "#00BFC4",
     "R1A" = "#F8766D",
     "R" = "grey30",
     "V38" = "#C77CFF",
     "?" = "grey90"
   )
 ) +
   scale_fill_manual(
     values = alpha(c(
       "R22" = "#00BFC4",
       "R1A" = "#F8766D",
       "R" = "grey30",
       "V38" = "#C77CFF",
       "?" = "grey30"
     ), alpha = 0.6)
   ) +
   scale_shape_manual(
     labels = c(
       "R22" = "R22:\nHay meadow",
       "R1A" = "R1A:\nSemi-dry grassland",
       "R" = "R:\nGeneral grassland",
       "V38" = "V38:\nDry anthropogenic\nvegetation",
       "?" = "No classification"
     ),
     values = c(
       "R22" = 16,
       "R1A" = 16,
       "R" = 3,
       "V38" = 15,
       "?" = 4
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
#   here("outputs", "figures", "figure_2_800dpi_16.5x11cm.tiff"),
#   dpi = 800, width = 16.5, height = 11, units = "cm"
# )
