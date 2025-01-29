Garchinger Heide and restoration sites: <br> Canopy height
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
  rename(y = CWM_Height)
```

# Statistics

## Data exploration

### Means and deviations

``` r
Rmisc::CI(sites$y, ci = .95)
```

    ##     upper      mean     lower 
    ## 0.3542787 0.3354947 0.3167106

``` r
median(sites$y)
```

    ## [1] 0.2914743

``` r
sd(sites$y)
```

    ## [1] 0.1195391

``` r
quantile(sites$y, probs = c(0.05, 0.95), na.rm = TRUE)
```

    ##        5%       95% 
    ## 0.1728743 0.4985857

### Graphs of raw data (Step 2, 6, 7)

![](model_check_canopy_height_files/figure-gfm/data-exploration-1.png)<!-- -->![](model_check_canopy_height_files/figure-gfm/data-exploration-2.png)<!-- -->

### Outliers, zero-inflation, transformations? (Step 1, 3, 4)

    ## # A tibble: 4 × 2
    ##   treatment      n
    ##   <fct>      <int>
    ## 1 control       68
    ## 2 cut_summer    30
    ## 3 cut_autumn    30
    ## 4 grazing       30

![](model_check_canopy_height_files/figure-gfm/outliers-1.png)<!-- -->

### Check collinearity part 1 (Step 5)

Exclude r \> 0.7 <br> Dormann et al. 2013 Ecography [DOI:
10.1111/j.1600-0587.2012.07348.x](https://doi.org/10.1111/j.1600-0587.2012.07348.x)

``` r
sites %>%
  select(height_vegetation, cover_vegetation) %>%
  GGally::ggpairs(lower = list(continuous = "smooth_loess")) +
  theme(strip.text = element_text(size = 7))
```

![](model_check_canopy_height_files/figure-gfm/collinearity-1.png)<!-- -->

## Models

Only here you have to modify the script to compare other models

``` r
load(file = here("outputs", "models", "model_height_1.Rdata"))
load(file = here("outputs", "models", "model_height_2.Rdata"))
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
##         (Intercept)  treatmentcut_autumn  treatmentcut_summer  
##              0.2570               0.2166               0.2163  
##    treatmentgrazing  
##             -0.0194
m_2
## 
## Call:
## lm(formula = y ~ treatment * cover_vegetation, data = sites)
## 
## Coefficients:
##                          (Intercept)                   treatmentcut_autumn  
##                            -0.001208                              0.430536  
##                  treatmentcut_summer                      treatmentgrazing  
##                             0.477861                              0.207211  
##                     cover_vegetation  treatmentcut_autumn:cover_vegetation  
##                             0.003744                             -0.003080  
## treatmentcut_summer:cover_vegetation     treatmentgrazing:cover_vegetation  
##                            -0.003799                             -0.002566
```

## Model check

### DHARMa

``` r
simulation_output_1 <- simulateResiduals(m_1, plot = TRUE)
```

![](model_check_canopy_height_files/figure-gfm/dharma_all-1.png)<!-- -->

``` r
simulation_output_2 <- simulateResiduals(m_2, plot = TRUE)
```

![](model_check_canopy_height_files/figure-gfm/dharma_all-2.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$treatment)
```

![](model_check_canopy_height_files/figure-gfm/dharma_single-1.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$treatment)
```

![](model_check_canopy_height_files/figure-gfm/dharma_single-2.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$cover_vegetation)
```

![](model_check_canopy_height_files/figure-gfm/dharma_single-3.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$cover_vegetation)
```

![](model_check_canopy_height_files/figure-gfm/dharma_single-4.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$height_vegetation)
```

![](model_check_canopy_height_files/figure-gfm/dharma_single-5.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$height_vegetation)
```

![](model_check_canopy_height_files/figure-gfm/dharma_single-6.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$botanist)
```

![](model_check_canopy_height_files/figure-gfm/dharma_single-7.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$botanist)
```

![](model_check_canopy_height_files/figure-gfm/dharma_single-8.png)<!-- -->

### Check collinearity part 2 (Step 5)

Remove VIF \> 3 or \> 10 <br> Zuur et al. 2010 Methods Ecol Evol [DOI:
10.1111/j.2041-210X.2009.00001.x](https://doi.org/10.1111/j.2041-210X.2009.00001.x)

``` r
#car::vif(m_1)
car::vif(m_2)
```

    ## there are higher-order terms (interactions) in this model
    ## consider setting type = 'predictor'; see ?vif

    ##                                  GVIF Df GVIF^(1/(2*Df))
    ## treatment                  13283.7591  3        4.866539
    ## cover_vegetation              11.7061  1        3.421418
    ## treatment:cover_vegetation  8085.4448  3        4.480062

## Model comparison

### <i>R</i><sup>2</sup> values

``` r
MuMIn::r.squaredGLMM(m_1)
##            R2m       R2c
## [1,] 0.8209962 0.8209962
MuMIn::r.squaredGLMM(m_2)
##            R2m       R2c
## [1,] 0.8523065 0.8523065
```

### AICc

Use AICc and not AIC since ratio n/K \< 40 <br> Burnahm & Anderson 2002
p. 66 ISBN: 978-0-387-95364-9

``` r
MuMIn::AICc(m_1, m_2) %>%
  arrange(AICc)
##     df      AICc
## m_2  9 -512.9776
## m_1  5 -487.7652
```

## Predicted values

### Summary table

``` r
summary(m_2)
```

    ## 
    ## Call:
    ## lm(formula = y ~ treatment * cover_vegetation, data = sites)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.104179 -0.022881 -0.002104  0.021046  0.195950 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                          -0.0012083  0.0451797  -0.027 0.978700    
    ## treatmentcut_autumn                   0.4305363  0.0623535   6.905 1.33e-10 ***
    ## treatmentcut_summer                   0.4778605  0.0584674   8.173 1.16e-13 ***
    ## treatmentgrazing                      0.2072108  0.0517063   4.007 9.64e-05 ***
    ## cover_vegetation                      0.0037435  0.0006500   5.759 4.63e-08 ***
    ## treatmentcut_autumn:cover_vegetation -0.0030798  0.0009067  -3.397 0.000873 ***
    ## treatmentcut_summer:cover_vegetation -0.0037987  0.0008785  -4.324 2.78e-05 ***
    ## treatmentgrazing:cover_vegetation    -0.0025664  0.0010965  -2.340 0.020578 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.04609 on 150 degrees of freedom
    ## Multiple R-squared:  0.858,  Adjusted R-squared:  0.8513 
    ## F-statistic: 129.4 on 7 and 150 DF,  p-value: < 2.2e-16

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

![](model_check_canopy_height_files/figure-gfm/predicted_values-1.png)<!-- -->

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
    ##  treatment  emmean      SE  df lower.CL upper.CL
    ##  control     0.220 0.00853 150    0.203    0.237
    ##  cut_autumn  0.469 0.00970 150    0.449    0.488
    ##  cut_summer  0.473 0.00851 150    0.457    0.490
    ##  grazing     0.276 0.02970 150    0.217    0.334
    ## 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast                estimate     SE  df t.ratio p.value
    ##  cut_autumn - control     0.24867 0.0129 150  19.253  <.0001
    ##  cut_summer - control     0.25354 0.0120 150  21.041  <.0001
    ##  cut_summer - cut_autumn  0.00487 0.0129 150   0.378  0.9816
    ##  grazing - control        0.05566 0.0309 150   1.803  0.2760
    ##  grazing - cut_autumn    -0.19301 0.0312 150  -6.184  <.0001
    ##  grazing - cut_summer    -0.19788 0.0309 150  -6.411  <.0001
    ## 
    ## P value adjustment: tukey method for comparing a family of 4 estimates

``` r
plot(emm, comparison = TRUE)
```

![](model_check_canopy_height_files/figure-gfm/effect-sizes-1.png)<!-- -->

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
