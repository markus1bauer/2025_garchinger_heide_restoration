#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Garchinger Heide
# NMDS ####
# Model building
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sina Appeltauer, Markus Bauer
# 2025-02-11



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(vegan)

### Start ###
rm(list = ls())

#### * Load data sites ####

sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types =
    cols(
    .default = "?",
    treatment = col_factor(
      levels = c("control", "cut_summer", "cut_autumn", "grazing")
      )
    )
  ) %>%
  filter(is.na(location) | location != "rollfeld")

#### * Load data species ####

species <- read_csv(
  here("data", "processed", "data_processed_species.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types =
    cols(
      .default = "?"
    )
) %>%
  pivot_longer(-accepted_name, names_to = "id", values_to = "value") %>%
  semi_join(sites, by = "id") %>%
  pivot_wider(names_from = "accepted_name", values_from = "value") %>%
  column_to_rownames(var = "id")

rm(list = setdiff(ls(), c("sites", "species", "theme_mb")))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics ################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### 1 NMDS ####################################################################


### Calculate ###
# Calculate only once and then load below:
# set.seed(11)
# ordi <- metaMDS(
#   species, dist = "bray", binary = TRUE,
#   try = 99, previous.best = TRUE, na.rm = TRUE
#   )
# save(ordi, file = here("outputs", "models", "model_nmds.Rdata"))
base::load(here("outputs", "models", "model_nmds.Rdata"))
ordi

### Stress ###
stressplot(ordi)
goodness_of_fit <- goodness(ordi)
plot(ordi, type = "t", main = "Goodness of fit")
points(ordi, display = "sites", cex = goodness_of_fit * 300)



### 2 Environmental factors ###################################################


#### a Vectors ----------------------------------------------------------------

(ef_vector1 <- envfit(
  ordi ~ height_vegetation + cover_vegetation,
  data = sites,
  permu = 999,
  na.rm = TRUE
))
plot(ordi, type = "n")
plot(ef_vector1, add = TRUE, p. = .99)

save(
  ef_vector1,
  file = here("outputs", "models", "model_nmds_envfit_vector1.Rdata")
)


#### b Factors ----------------------------------------------------------------

(ef_factor1 <- envfit(
  ordi ~  treatment,
  data = sites, permu = 999, na.rm = TRUE
))
plot(ordi, type = "n")
ordiellipse(ordi, sites$treatment, kind = "sd", draw = "lines", label = TRUE)

