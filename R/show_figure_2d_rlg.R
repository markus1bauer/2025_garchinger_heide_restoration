#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Garchinger Heide
# Species rlg ####
# Show figure 2
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-01-28



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(blme)
library(ggeffects)
library(ggbeeswarm)

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
    legend.position = "none",
    legend.margin = margin(0, 0, 0, 0, "cm"),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}

### Load data ###
sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("", "na", "NA"), col_types = cols(
    .default = "?",
    treatment = "f"
  )
) %>%
  rename(y = rlg) %>%
  mutate(
    treatment = fct_relevel(
      treatment, "control", "cut_summer", "cut_autumn", "grazing"
    ),
    treatment = fct_recode(
      treatment, "Reference" = "control", "Mowing\nsummer" = "cut_summer",
      "Mowing\nautumn" = "cut_autumn", "Topsoil\nremoval" = "grazing"
    )
  )

### * Model ####
load(file = here("outputs", "models", "model_richness_RLG_1.Rdata"))
m <- m1
m #m@call



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Plot ######################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



data_model <- ggeffect(
  m, terms = c("treatment"), back.transform = TRUE, ci_level = .95
) %>%
  mutate(
    x = fct_recode(
      x, "Reference" = "control", "Mowing\nsummer" = "cut_summer",
      "Mowing\nautumn" = "cut_autumn", "Topsoil\nremoval" = "grazing"
    )
  ) %>%
  slice(1:4)

data <- sites %>%
  rename(predicted = y, x = treatment)

(graph_d <- ggplot() +
    geom_quasirandom(
      data = data,
      aes(x = x, predicted, color = x),
      dodge.width = .6, size = 1, shape = 16
    ) +
    geom_hline(
      yintercept = c(21.162, 20.325, 21.998),
      linetype = c(1, 2, 2),
      color = "grey70"
    ) +
    geom_errorbar(
      data = data_model,
      aes(x, predicted, ymin = conf.low, ymax = conf.high),
      width = 0.0, linewidth = 0.4
    ) +
    geom_point(
      data = data_model,
      aes(x, predicted),
      size = 2
    ) +
    annotate("text", label = "a", x = 1, y = 35) +
    annotate("text", label = "b", x = 2, y = 35) +
    annotate("text", label = "b", x = 3, y = 35) +
    annotate("text", label = "c", x = 4, y = 35) +
    scale_y_continuous(limits = c(0, 35), breaks = seq(-100, 400, 5)) +
    scale_color_manual(values = c("Reference" = "#f947d1", 
                                  "Mowing\nsummer" = "#61a161", 
                                  "Mowing\nautumn" = "#87ceeb", 
                                  "Topsoil\nremoval" = "#b06e13")) +
    labs(x = "",
         y = expression(
           Red ~ List ~ species ~ (Germany) ~ "[" * 'sp./4m²' * "]")
    ) +
    theme_mb())

### Save ###
ggsave(
  here("outputs", "figures", "figure_rlg_800dpi_8x8cm.tiff"),
  dpi = 800, width = 8, height = 8, units = "cm"
)
