Garchinger Heide and restoration sites: <br> Species richness
================
<b>Sina Appeltauer</b> <br>
<b>2025-03-04</b>

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
Teschauer</b>, <b>Johannes Kollmann</b> & <b>Markus Bauer</b>\*

Technichal University of Munich, TUM School of Life Sciences, Chair of
Restoration Ecology, Emil-Ramann-Straße 6, 85354 Freising, Germany

\*<markus1.bauer@tum.de>

ORCiD ID: [0000-0001-5372-4174](https://orcid.org/0000-0001-5372-4174)
<br> [Google
Scholar](https://scholar.google.de/citations?user=oHhmOkkAAAAJ&hl=de&oi=ao)
<br> GitHub: [markus1bauer](https://github.com/markus1bauer) <br>
GitHub:
[TUM-Restoration-Ecology](https://github.com/TUM-Restoration-Ecology)

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
library(DHARMa)
library(emmeans)
```

#### Load data

``` r
sites <- read_csv(
  here("data", "processed", "data_processed_sites.csv"),
  col_names = TRUE, na = c("", "na", "NA"), col_types = cols(
    .default = "?",
      treatment = col_factor(
        levels = c("control", "cut_summer", "cut_autumn", "grazing")
      )
    )
  ) %>%
  rename(y = richness_total) %>%
  filter(is.na(location) | location != "rollfeld")
```

# Statistics

## Data exploration

### Means and deviations

``` r
Rmisc::CI(sites$y, ci = .95)
```

    ##    upper     mean    lower 
    ## 28.79882 27.57895 26.35908

``` r
median(sites$y)
```

    ## [1] 27

``` r
sd(sites$y)
```

    ## [1] 7.611882

``` r
quantile(sites$y, probs = c(0.05, 0.95), na.rm = TRUE)
```

    ##  5% 95% 
    ##  16  40

### Graphs of raw data (Step 2, 6, 7)

![](model_check_species_richness_files/figure-gfm/data-exploration-1.png)<!-- -->![](model_check_species_richness_files/figure-gfm/data-exploration-2.png)<!-- -->![](model_check_species_richness_files/figure-gfm/data-exploration-3.png)<!-- -->

### Outliers, zero-inflation, transformations? (Step 1, 3, 4)

    ## # A tibble: 4 × 2
    ##   treatment      n
    ##   <fct>      <int>
    ## 1 control       62
    ## 2 cut_summer    30
    ## 3 cut_autumn    30
    ## 4 grazing       30

![](model_check_species_richness_files/figure-gfm/outliers-1.png)<!-- -->

### Check collinearity part 1 (Step 5)

Exclude r \> 0.7 <br> Dormann et al. 2013 Ecography [DOI:
10.1111/j.1600-0587.2012.07348.x](https://doi.org/10.1111/j.1600-0587.2012.07348.x)

``` r
sites %>%
  select(height_vegetation, cover_vegetation) %>%
  GGally::ggpairs(lower = list(continuous = "smooth_loess")) +
  theme(strip.text = element_text(size = 7))
```

![](model_check_species_richness_files/figure-gfm/collinearity-1.png)<!-- -->

## Models

Only here you have to modify the script to compare other models

``` r
load(file = here("outputs", "models", "model_species_richness_1.Rdata"))
load(file = here("outputs", "models", "model_species_richness_2.Rdata"))
m_1 <- m1
m_2 <- m2
```

## Model check

### DHARMa

``` r
simulation_output_1 <- simulateResiduals(m_1, plot = TRUE)
```

![](model_check_species_richness_files/figure-gfm/dharma_all-1.png)<!-- -->

``` r
simulation_output_2 <- simulateResiduals(m_2, plot = TRUE)
```

![](model_check_species_richness_files/figure-gfm/dharma_all-2.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$treatment)
```

![](model_check_species_richness_files/figure-gfm/dharma_single-1.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$treatment)
```

![](model_check_species_richness_files/figure-gfm/dharma_single-2.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$cover_vegetation)
```

![](model_check_species_richness_files/figure-gfm/dharma_single-3.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$cover_vegetation)
```

![](model_check_species_richness_files/figure-gfm/dharma_single-4.png)<!-- -->

``` r
plotResiduals(simulation_output_1$scaledResiduals, sites$height_vegetation)
```

![](model_check_species_richness_files/figure-gfm/dharma_single-5.png)<!-- -->

``` r
plotResiduals(simulation_output_2$scaledResiduals, sites$height_vegetation)
```

![](model_check_species_richness_files/figure-gfm/dharma_single-6.png)<!-- -->

### Check collinearity part 2 (Step 5)

Remove VIF \> 3 or \> 10 <br> Zuur et al. 2010 Methods Ecol Evol [DOI:
10.1111/j.2041-210X.2009.00001.x](https://doi.org/10.1111/j.2041-210X.2009.00001.x)

``` r
#car::vif(m_1)
car::vif(m_2)
```

    ##                      GVIF Df GVIF^(1/(2*Df))
    ## treatment        3.567521  3        1.236121
    ## cover_vegetation 3.567521  1        1.888788

## Model comparison

### <i>R</i><sup>2</sup> values

``` r
MuMIn::r.squaredGLMM(m_1)
##            R2m       R2c
## [1,] 0.5579109 0.5579109
MuMIn::r.squaredGLMM(m_2)
##           R2m      R2c
## [1,] 0.568998 0.568998
```

### AICc

Use AICc and not AIC since ratio n/K \< 40 <br> Burnahm & Anderson 2002
p. 66 ISBN: 978-0-387-95364-9

``` r
MuMIn::AICc(m_1, m_2) %>%
  arrange(AICc)
##     df     AICc
## m_2  6 929.7003
## m_1  5 932.0186
```

## Predicted values

### Summary table

``` r
summary(m_2)
```

    ## 
    ## Call:
    ## lm(formula = y ~ treatment + cover_vegetation, data = sites)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -12.8421  -3.0397  -0.0913   2.9891  12.9970 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          28.19211    2.87025   9.822  < 2e-16 ***
    ## treatmentcut_summer  -9.17661    1.17917  -7.782 1.16e-12 ***
    ## treatmentcut_autumn  -8.30047    1.12885  -7.353 1.25e-11 ***
    ## treatmentgrazing    -10.62264    2.06429  -5.146 8.39e-07 ***
    ## cover_vegetation      0.08313    0.03961   2.099   0.0376 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.026 on 147 degrees of freedom
    ## Multiple R-squared:  0.5756, Adjusted R-squared:  0.564 
    ## F-statistic: 49.84 on 4 and 147 DF,  p-value: < 2.2e-16

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

![](model_check_species_richness_files/figure-gfm/predicted_values-1.png)<!-- -->

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
    ##  treatment  emmean    SE  df lower.CL upper.CL
    ##  control      33.1 0.780 147     31.6     34.7
    ##  cut_summer   23.9 0.920 147     22.1     25.8
    ##  cut_autumn   24.8 0.962 147     22.9     26.7
    ##  grazing      22.5 1.580 147     19.4     25.6
    ## 
    ## Confidence level used: 0.95 
    ## 
    ## $contrasts
    ##  contrast                estimate   SE  df t.ratio p.value
    ##  cut_summer - control      -9.177 1.18 147  -7.782  <.0001
    ##  cut_autumn - control      -8.300 1.13 147  -7.353  <.0001
    ##  cut_autumn - cut_summer    0.876 1.32 147   0.666  0.9098
    ##  grazing - control        -10.623 2.06 147  -5.146  <.0001
    ##  grazing - cut_summer      -1.446 1.88 147  -0.769  0.8682
    ##  grazing - cut_autumn      -2.322 2.04 147  -1.137  0.6675
    ## 
    ## P value adjustment: tukey method for comparing a family of 4 estimates

``` r
plot(emm, comparison = TRUE)
```

![](model_check_species_richness_files/figure-gfm/effect-sizes-1.png)<!-- -->

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
    ##  [1] emmeans_1.10.7   DHARMa_0.4.7     patchwork_1.3.0  ggbeeswarm_0.7.2
    ##  [5] lubridate_1.9.4  forcats_1.0.0    stringr_1.5.1    dplyr_1.1.4     
    ##  [9] purrr_1.0.4      readr_2.1.5      tidyr_1.3.1      tibble_3.2.1    
    ## [13] ggplot2_3.5.1    tidyverse_2.0.0  here_1.0.1      
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.1   vipor_0.4.7        farver_2.1.2       fastmap_1.2.0     
    ##  [5] GGally_2.2.1       bayestestR_0.15.2  promises_1.3.2     digest_0.6.37     
    ##  [9] estimability_1.5.1 timechange_0.3.0   mime_0.12          lifecycle_1.0.4   
    ## [13] magrittr_2.0.3     compiler_4.4.2     rlang_1.1.5        tools_4.4.2       
    ## [17] utf8_1.2.4         yaml_2.3.10        knitr_1.49         labeling_0.4.3    
    ## [21] bit_4.5.0.1        ggstance_0.3.7     plyr_1.8.9         RColorBrewer_1.1-3
    ## [25] gap.datasets_0.0.6 abind_1.4-8        withr_3.0.2        datawizard_1.0.0  
    ## [29] stats4_4.4.2       grid_4.4.2         xtable_1.8-4       colorspace_2.1-1  
    ## [33] scales_1.3.0       iterators_1.0.14   MASS_7.3-61        insight_1.0.2     
    ## [37] cli_3.6.4          mvtnorm_1.3-3      dotwhisker_0.8.3   rmarkdown_2.29    
    ## [41] crayon_1.5.3       reformulas_0.4.0   generics_0.1.3     performance_0.13.0
    ## [45] rstudioapi_0.17.1  tzdb_0.4.0         parameters_0.24.1  minqa_1.2.8       
    ## [49] splines_4.4.2      parallel_4.4.2     vctrs_0.6.5        boot_1.3-31       
    ## [53] Matrix_1.7-1       carData_3.0-5      car_3.1-3          hms_1.1.3         
    ## [57] bit64_4.6.0-1      Formula_1.2-5      qgam_1.3.4         beeswarm_0.4.0    
    ## [61] Rmisc_1.5.1        foreach_1.5.2      gap_1.6            glue_1.8.0        
    ## [65] nloptr_2.1.1       ggstats_0.8.0      codetools_0.2-20   stringi_1.8.4     
    ## [69] gtable_0.3.6       later_1.4.1        lme4_1.1-36        munsell_0.5.1     
    ## [73] pillar_1.10.1      htmltools_0.5.8.1  R6_2.6.1           Rdpack_2.6.2      
    ## [77] doParallel_1.0.17  rprojroot_2.0.4    vroom_1.6.5        evaluate_1.0.3    
    ## [81] shiny_1.10.0       lattice_0.22-6     rbibutils_2.3      httpuv_1.6.15     
    ## [85] Rcpp_1.0.14        gridExtra_2.3      coda_0.19-4.1      nlme_3.1-166      
    ## [89] MuMIn_1.48.4       mgcv_1.9-1         xfun_0.50          pkgconfig_2.0.3
