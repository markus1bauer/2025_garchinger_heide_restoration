#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Management Garchinger Heide restoration sites
# Specific leaf area (SLA) ####
# Model building
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-03-03



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(patchwork)
library(DHARMa)

### Start ###
rm(list = ls())

### Load data ###
sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types =
    cols(
      .default = "?",
      treatment = "f"
    )
) %>%
  rename(y = CWM_SLA) %>%
  filter(
    is.na(location) | location != "rollfeld")



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics #################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Data exploration ##########################################################


### a Graphs of raw data -------------------------------------------------------

ggplot(sites, aes(y = y, x = treatment)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent")
ggplot(sites, aes(y = y, x = cover_vegetation)) +
  geom_quasirandom(color = "grey") + geom_smooth(method = "lm") +
  facet_grid(~treatment)


### b Outliers, zero-inflation, transformations? ----------------------------

sites %>% group_by(treatment) %>% count(treatment)
ggplot(sites, aes(x = treatment, y = y)) + geom_quasirandom()
ggplot(sites, aes(x = y)) + geom_histogram(binwidth = .1)
ggplot(sites, aes(x = y)) + geom_density()


### c Check collinearity ------------------------------------------------------

sites %>%
  select(height_vegetation, cover_vegetation) %>%
  GGally::ggpairs(lower = list(continuous = "smooth_loess")) +
  theme(strip.text = element_text(size = 7))
#--> exclude r > 0.7
# Dormann et al. 2013 Ecography
# https://doi.org/10.1111/j.1600-0587.2012.07348.x



## 2 Model building ###########################################################


m1 <- lm(y ~ treatment, data = sites)
simulateResiduals(m1, plot = TRUE)

m2 <- lm(y ~ treatment * cover_vegetation, data = sites)
simulateResiduals(m2, plot = TRUE)


### Save ####

save(m1, file = here("outputs", "models", "model_sla_1.Rdata"))
save(m2, file = here("outputs", "models", "model_sla_2.Rdata"))
