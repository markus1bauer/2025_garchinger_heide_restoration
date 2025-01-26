#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Garchinger Heide
# Species richness ####
# Model building
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sina Appeltauer, Markus Bauer
# 2024-10-09



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Preparation ###############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



### Packages ###
library(here)
library(tidyverse)
library(ggbeeswarm)
library(patchwork)
library(brms)
library(blme)

### Start ###
rm(list = ls())

### Load data ###
sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("na", "NA", ""), col_types =
    cols(
      .default = "?"
    )
)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Statistics #################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Data exploration ##########################################################


### a Graphs of raw data -------------------------------------------------------

# plot1 <- ggplot(sites %>% filter(survey_year == 2021),
#                 aes(y = n, x = sand_ratio)) +
#   geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent") +
#   facet_grid(~ survey_year_fct) +
#   labs(title = "Sand ratio [vol%]")

plot1 <- ggplot(sites, aes(y = richness_total, x = treatment)) +
  geom_quasirandom(color = "grey") + geom_boxplot(fill = "transparent")

### b Outliers, zero-inflation, transformations? ----------------------------

# sites %>% group_by(exposition) %>% count(site)
# boxplot(sites$n)
# ggplot(sites, aes(x = exposition, y = n)) + geom_quasirandom()
# ggplot(sites, aes(x = n)) + geom_histogram(binwidth = 0.03)
# ggplot(sites, aes(x = n)) + geom_density()

sites %>% group_by(treatment) %>% count(id)
boxplot(sites$richness_total)
ggplot(sites, aes(x = treatment, y = richness_total)) + geom_quasirandom()
ggplot(sites, aes(x = richness_total)) + geom_histogram(binwidth = 0.03)
ggplot(sites, aes(x = richness_total)) + geom_density()

### c Check collinearity ------------------------------------------------------

# sites %>%
#   select(where(is.numeric), -b, -c, -d, -y, -ends_with("scaled")) %>%
#   GGally::ggpairs(lower = list(continuous = "smooth_loess")) +
#   theme(strip.text = element_text(size = 7))
# sites <- sites %>%
#   select(-biotope_area)
#--> exclude r > 0.7
# Dormann et al. 2013 Ecography
# https://doi.org/10.1111/j.1600-0587.2012.07348.x

sites %>%
  select(where(is.numeric), -(4:12), -14) %>%
  GGally::ggpairs(lower = list(continuous = "smooth_loess")) +
  theme(strip.text = element_text(size = 7))
sites <- sites %>%
  select(-shannon)


## 2 Model building ###########################################################

### a Random structure ---------------------------------------------------------

m1a <- blmer(
  richness_total ~ 1 + (1 | treatment), data = sites, REML = TRUE
)
m1b <- blmer(
  richness_total ~ 1 + (1 | treatment / id), data = sites, REML = TRUE
)
m1c <- blmer(richness_total ~ 1 + (1 | id), data = sites, REML = TRUE)

MuMIn::AICc(m1a, m1b, m1c) %>%
  arrange(AICc)


### b Fixed effects ------------------------------------------------------------

m1 <- blmer(
  richness_total ~ treatment + (1 | id),
  REML = FALSE,
  control = lmerControl(optimizer = "Nelder_Mead"),
  cov.prior = wishart,
  data = sites
)
simulateResiduals(m1, plot = TRUE)



### d Save ---------------------------------------------------------------------


save(m1, file = here("outputs", "models", "model_species_richness_1.Rdata"))
save(m2, file = here("outputs", "models", "model_species_richness_2.Rdata"))

