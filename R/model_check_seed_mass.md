Garchinger Heide and restoration sites: <br> Seed mass
================
<b>Markus Bauer</b> <br>
<b>2025-01-29</b>

- [Preparation](#preparation)
- [Statistics](#statistics)
  - [Data exploration](#data-exploration)
    - [Means and deviations](#means-and-deviations)
    - [Graphs of raw data (Step 2, 6,
      7)](#graphs-of-raw-data-step-2-6-7)
    - [Outliers, zero-inflation, transformations? (Step 1, 3,
      4)](#outliers-zero-inflation-transformations-step-1-3-4)
    - [Check collinearity part 1 (Step
      5)](#check-collinearity-part-1-step-5)
  - [Models](#models)
  - [Model check](#model-check)
    - [DHARMa](#dharma)
    - [Check collinearity part 2 (Step
      5)](#check-collinearity-part-2-step-5)
  - [Model comparison](#model-comparison)
    - [<i>R</i><sup>2</sup> values](#r2-values)
    - [AICc](#aicc)
  - [Predicted values](#predicted-values)
    - [Summary table](#summary-table)
    - [Forest plot](#forest-plot)
    - [Effect sizes](#effect-sizes)
- [Session info](#session-info)

<br/> <br/> <b>Sina Appeltauer</b>, <b>Malte Knöppler</b>, <b>Maren
Teschauer</b> & <b>Markus Bauer</b>\*

Technichal University of Munich, TUM School of Life Sciences, Chair of
Restoration Ecology, Emil-Ramann-Straße 6, 85354 Freising, Germany

\*<markus1.bauer@tum.de>

ORCiD ID: [0000-0001-5372-4174](https://orcid.org/0000-0001-5372-4174)
<br> [Google
Scholar](https://scholar.google.de/citations?user=oHhmOkkAAAAJ&hl=de&oi=ao)
<br> GitHub: [markus1bauer](https://github.com/markus1bauer)

To compare different models, you only have to change the models in
section ‘Load models’

# Preparation

Protocol of data exploration (Steps 1-8) used from Zuur et al. (2010)
Methods Ecol Evol [DOI:
10.1111/2041-210X.12577](https://doi.org/10.1111/2041-210X.12577)

#### Packages

``` r
library(here)
library(tidyverse)
library(ggbeeswarm)
library(patchwork)
library(blme)
library(DHARMa)
library(emmeans)
```

#### Load data

``` r
sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("", "na", "NA"), col_types = 
    cols(
      .default = "?",
      treatment = col_factor(
        levels = c("control", "cut_summer", "cut_autumn", "grazing")
      )
    )
  ) %>%
  rename(y = CWM_Seed) %>%
  filter(
    is.na(location) | location != "Rollfeld" &
      !(id %in% c(
        "X2021tum03", "X2021tum27", "X2021tum43", "X2021tum48", "X2021tum51"
        )) # Plots with >=10% of Polygonatum odoratum (seed mass = 0.08 g)
    )
```

# Statistics

## Data exploration

### Means and deviations

``` r
Rmisc::CI(sites$y, ci = .95)
```

    ##       upper        mean       lower 
    ## 0.003160867 0.002994722 0.002828577

``` r
median(sites$y)
```

    ## [1] 0.00310387

``` r
sd(sites$y)
```

    ## [1] 0.001040192

``` r
quantile(sites$y, probs = c(0.05, 0.95), na.rm = TRUE)
```

    ##          5%         95% 
    ## 0.001465411 0.004540762

### Graphs of raw data (Step 2, 6, 7)

![](model_check_seed_mass_files/figure-gfm/data-exploration-1.png)<!-- -->![](model_check_seed_mass_files/figure-gfm/data-exploration-2.png)<!-- -->

### Outliers, zero-inflation, transformations? (Step 1, 3, 4)

    ## # A tibble: 4 × 2
    ##   treatment      n
    ##   <fct>      <int>
    ## 1 control       63
    ## 2 cut_summer    30
    ## 3 cut_autumn    30
    ## 4 grazing       30

![](model_check_seed_mass_files/figure-gfm/outliers-1.png)<!-- -->

### Check collinearity part 1 (Step 5)

Exclude r \> 0.7 <br> Dormann et al. 2013 Ecography [DOI:
10.1111/j.1600-0587.2012.07348.x](https://doi.org/10.1111/j.1600-0587.2012.07348.x)

``` r
sites %>%
  select(height_vegetation, cover_vegetation) %>%
  GGally::ggpairs(lower = list(continuous = "smooth_loess")) +
  theme(strip.text = element_text(size = 7))
```

![](model_check_seed_mass_files/figure-gfm/collinearity-1.png)<!-- -->

## Models

Only here you have to modify the script to compare other models

``` r
load(file = here("outputs", "models", "model_seed_mass_1.Rdata"))
load(file = here("outputs", "models", "model_seed_mass_2.Rdata"))
m_1 <- m1
m_2 <- m2
```

``` r
#m_1@call
#m_2@call
m_1
## 
## Call:
## lm(formula = y ~ treatment, data = sites)
## 
## Coefficients:
##         (Intercept)  treatmentcut_summer     treatmentgrazing  
##           0.0029747            0.0005489           -0.0011408  
## treatmentcut_autumn  
##           0.0006940
m_2
## 
## Call:
## lm(formula = y ~ treatment * cover_vegetation, data = sites)
## 
## Coefficients:
##                          (Intercept)                   treatmentcut_summer  
##                            2.111e-03                             1.477e-03  
##                     treatmentgrazing                   treatmentcut_autumn  
##                           -8.064e-04                             2.560e-03  
##                     cover_vegetation  treatmentcut_summer:cover_vegetation  
##                            1.253e-05                            -1.360e-05  
##    treatmentgrazing:cover_vegetation  treatmentcut_autumn:cover_vegetation  
##                            7.181e-06                            -2.757e-05
```

## Model check

### DHARMa

``` r
simulation_output_1 <- simulateResiduals(m_1, plot = TRUE)
```

![](model_check_seed_mass_files/figure-gfm/dharma_all-1.png)<!-- -->

``` r
simulation_output_2 <- simulateResiduals(m_2, plot = TRUE)
```

![](model_check_seed_mass_files/figure-gfm/dharma_all-2.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$treatment)
```

![](model_check_seed_mass_files/figure-gfm/dharma_single-1.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$treatment)
```

![](model_check_seed_mass_files/figure-gfm/dharma_single-2.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$cover_vegetation)
```

![](model_check_seed_mass_files/figure-gfm/dharma_single-3.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$cover_vegetation)
```

![](model_check_seed_mass_files/figure-gfm/dharma_single-4.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$height_vegetation)
```

![](model_check_seed_mass_files/figure-gfm/dharma_single-5.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$height_vegetation)
```

![](model_check_seed_mass_files/figure-gfm/dharma_single-6.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$botanist)
```

![](model_check_seed_mass_files/figure-gfm/dharma_single-7.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$botanist)
```

![](model_check_seed_mass_files/figure-gfm/dharma_single-8.png)<!-- -->

### Check collinearity part 2 (Step 5)

Remove VIF \> 3 or \> 10 <br> Zuur et al. 2010 Methods Ecol Evol [DOI:
10.1111/j.2041-210X.2009.00001.x](https://doi.org/10.1111/j.2041-210X.2009.00001.x)

``` r
#car::vif(m_1)
car::vif(m_2)
```

    ## there are higher-order terms (interactions) in this model
    ## consider setting type = 'predictor'; see ?vif

    ##                                   GVIF Df GVIF^(1/(2*Df))
    ## treatment                  13151.56263  3        4.858433
    ## cover_vegetation              12.03565  1        3.469243
    ## treatment:cover_vegetation  8030.86572  3        4.475007

## Model comparison

### <i>R</i><sup>2</sup> values

``` r
MuMIn::r.squaredGLMM(m_1)
##            R2m       R2c
## [1,] 0.3751448 0.3751448
MuMIn::r.squaredGLMM(m_2)
##            R2m       R2c
## [1,] 0.3869826 0.3869826
```

### AICc

Use AICc and not AIC since ratio n/K \< 40 <br> Burnahm & Anderson 2002
p. 66 ISBN: 978-0-387-95364-9

``` r
MuMIn::AICc(m_1, m_2) %>%
  arrange(AICc)
##     df      AICc
## m_1  5 -1731.212
## m_2  9 -1726.969
```

## Predicted values

### Summary table

``` r
summary(m_1)
```

    ## 
    ## Call:
    ## lm(formula = y ~ treatment, data = sites)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0020681 -0.0004008 -0.0001258  0.0002503  0.0042158 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.0029747  0.0001042  28.538  < 2e-16 ***
    ## treatmentcut_summer  0.0005489  0.0001835   2.991 0.003255 ** 
    ## treatmentgrazing    -0.0011408  0.0001835  -6.216 4.87e-09 ***
    ## treatmentcut_autumn  0.0006940  0.0001835   3.781 0.000225 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0008274 on 149 degrees of freedom
    ## Multiple R-squared:  0.3798, Adjusted R-squared:  0.3673 
    ## F-statistic: 30.42 on 3 and 149 DF,  p-value: 2.124e-15

### Forest plot

``` r
dotwhisker::dwplot(
  list(m_1, m_2),
  ci = 0.95,
  show_intercept = FALSE,
  vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) +
  xlim(-0.3, 0.35) +
  theme_classic()
```

![](model_check_seed_mass_files/figure-gfm/predicted_values-1.png)<!-- -->

### Effect sizes

Effect sizes of chosen model just to get exact values of means etc. if
necessary.

``` r
(emm <- emmeans(
  m_2,
  revpairwise ~ treatment,
  type = "response"
  ))
```

    ## $emmeans
    ##  treatment   emmean       SE  df lower.CL upper.CL
    ##  control    0.00285 0.000160 145  0.00253  0.00316
    ##  cut_summer 0.00353 0.000153 145  0.00322  0.00383
    ##  grazing    0.00246 0.000526 145  0.00142  0.00350
    ##  cut_autumn 0.00379 0.000176 145  0.00344  0.00414
    ## 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast                 estimate       SE  df t.ratio p.value
    ##  cut_summer - control     0.000679 0.000221 145   3.069  0.0135
    ##  grazing - control       -0.000385 0.000550 145  -0.700  0.8971
    ##  grazing - cut_summer    -0.001064 0.000548 145  -1.941  0.2155
    ##  cut_autumn - control     0.000942 0.000238 145   3.963  0.0007
    ##  cut_autumn - cut_summer  0.000262 0.000233 145   1.125  0.6747
    ##  cut_autumn - grazing     0.001327 0.000555 145   2.390  0.0835
    ## 
    ## P value adjustment: tukey method for comparing a family of 4 estimates

``` r
plot(emm, comparison = TRUE)
```

![](model_check_seed_mass_files/figure-gfm/effect-sizes-1.png)<!-- -->

# Session info

    ## R version 4.4.2 (2024-10-31 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 11 x64 (build 26100)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8   
    ## [3] LC_MONETARY=German_Germany.utf8 LC_NUMERIC=C                   
    ## [5] LC_TIME=German_Germany.utf8    
    ## 
    ## time zone: Europe/Berlin
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] emmeans_1.10.5   DHARMa_0.4.7     blme_1.0-6       lme4_1.1-35.5   
    ##  [5] Matrix_1.7-1     patchwork_1.3.0  ggbeeswarm_0.7.2 lubridate_1.9.3 
    ##  [9] forcats_1.0.0    stringr_1.5.1    dplyr_1.1.4      purrr_1.0.2     
    ## [13] readr_2.1.5      tidyr_1.3.1      tibble_3.2.1     ggplot2_3.5.1   
    ## [17] tidyverse_2.0.0  here_1.0.1      
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.1   vipor_0.4.7        farver_2.1.2       fastmap_1.2.0     
    ##  [5] GGally_2.2.1       bayestestR_0.15.0  promises_1.3.2     digest_0.6.37     
    ##  [9] estimability_1.5.1 timechange_0.3.0   mime_0.12          lifecycle_1.0.4   
    ## [13] magrittr_2.0.3     compiler_4.4.2     rlang_1.1.4        tools_4.4.2       
    ## [17] utf8_1.2.4         yaml_2.3.10        knitr_1.49         labeling_0.4.3    
    ## [21] bit_4.5.0          ggstance_0.3.7     plyr_1.8.9         RColorBrewer_1.1-3
    ## [25] gap.datasets_0.0.6 abind_1.4-8        withr_3.0.2        datawizard_0.13.0 
    ## [29] stats4_4.4.2       grid_4.4.2         fansi_1.0.6        xtable_1.8-4      
    ## [33] colorspace_2.1-1   scales_1.3.0       iterators_1.0.14   MASS_7.3-61       
    ## [37] insight_1.0.0      cli_3.6.3          mvtnorm_1.3-2      dotwhisker_0.8.3  
    ## [41] rmarkdown_2.29     crayon_1.5.3       generics_0.1.3     performance_0.12.4
    ## [45] rstudioapi_0.17.1  tzdb_0.4.0         parameters_0.24.0  minqa_1.2.8       
    ## [49] splines_4.4.2      parallel_4.4.2     vctrs_0.6.5        boot_1.3-31       
    ## [53] carData_3.0-5      car_3.1-3          hms_1.1.3          bit64_4.5.2       
    ## [57] Formula_1.2-5      qgam_1.3.4         beeswarm_0.4.0     Rmisc_1.5.1       
    ## [61] foreach_1.5.2      gap_1.6            glue_1.8.0         nloptr_2.1.1      
    ## [65] ggstats_0.7.0      codetools_0.2-20   stringi_1.8.4      gtable_0.3.6      
    ## [69] later_1.4.1        munsell_0.5.1      pillar_1.9.0       htmltools_0.5.8.1 
    ## [73] R6_2.5.1           Rdpack_2.6.2       doParallel_1.0.17  rprojroot_2.0.4   
    ## [77] vroom_1.6.5        evaluate_1.0.1     shiny_1.9.1        lattice_0.22-6    
    ## [81] rbibutils_2.3      httpuv_1.6.15      Rcpp_1.0.13-1      gridExtra_2.3     
    ## [85] coda_0.19-4.1      nlme_3.1-166       MuMIn_1.48.4       mgcv_1.9-1        
    ## [89] xfun_0.49          pkgconfig_2.0.3
