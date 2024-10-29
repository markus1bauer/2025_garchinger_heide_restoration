#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Garchinger Heide
# Prepare data ####
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sina Appeltauer, Markus bauer
# 2024-10-09



### Packages ###
library(renv)
library(installr)
library(here)
library(tidyverse)
library(TNRS)
library(GIFT)

### Start ###
# Create hashtag infront of a line: shift + strg + c
rm(list = ls())
# installr::updateR(
#   browse_news = FALSE,
#   install_R = TRUE,
#   copy_packages = TRUE,
#   copy_site_files = TRUE,
#   keep_old_packages = FALSE,
#   update_packages = FALSE,
#   start_new_R = FALSE,
#   quit_R = TRUE
#   )



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Load data #################################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Sites ####################################################################


sites_reference <- read_csv2(
  here("data", "raw", "data_raw_sites_reference.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types =
    cols(
      .default = "?",
      aufnahmedatum_2021 = col_date(format = "%d.%m.%Y")
    )
) %>%
  select(!ends_with("_2026")) %>%
  rename(
    botanist_2021 = botaniker_2021,
    survey_date_2021 = aufnahmedatum_2021,
    height_vegetation_2021 = vegetationshoehe_2021,
    cover_vegetation_2021 = vegetationsdeckung_2021,
    cover_moss_2021 = moosdeckung_2021,
    cover_litter_2021 = streudeckung_2021,
    cover_soil_2021 = rohbodendeckung_2021
  )

sites_restoration <- read_csv(
  here("data", "raw", "data_raw_sites_restoration.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types =
    cols(
    .default = "?"
  )
)

# Sina, renamed the plot ID with resXX



## 2 Species ##################################################################

species_reference <- read_csv2(
  here("data", "raw", "data_raw_species_reference.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types =
    cols(
      .default = "?"
    )
)

species_restoration <- read_csv(
  here("data", "raw", "data_raw_species_restoration.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types =
    cols(
      .default = "?"
    )
)

# Sina, renamed the plot ID with resXX



## 3 FloraVeg.EU species #######################################################


traits <- readxl::read_excel(
  here(
    "data", "raw",
    "Characteristic-species-combinations-EUNIS-habitats-2021-06-01.xlsx"
    ),
  col_names = TRUE, na = c("", "NA", "na")
)

# Bisher nur Zielarten in traits-Tabelle --> später müssen noch alle kartierten Arten
# eingefügt werden und die dazugehörigen Traits und Rote-Liste-Status. Aber das
# passiert weiter unten



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ###########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Combine reference and restoration plots ##################################


species <- species_reference %>%
  left_join(species_restoration, by = "name")

sites <- sites_reference %>%
  full_join(sites_restoration, by = "plot")

# Jetzt kann man die Datensätze zusammenfassen. Es muss noch geschaut werden,
# ob es funktioniert, wenn die Plotnamen stehen (s. oben)
# scheint zu funktionieren mit den neuen Plotnamen


## 2 Select target species from FloraVeg.EU ###################################


data <- traits %>%
  rename_with(~ tolower(gsub(" ", "_", .x))) %>%
  filter(
    habitat_code %in% c("R1A", "R22") &
      species_type %in% c("Diagnostic", "Constant")
    ) %>%
  select(species, habitat_code, species_type) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = "habitat_code", values_from = "value") %>%
  group_by(species) %>%
  summarize(across(c("R1A", "R22"), ~ sum(.x, na.rm = TRUE))) %>%
  mutate(
    across(c("R1A", "R22"), ~ if_else(. > 0, 1, 0)),
    both = if_else(R1A > 0 & R22 > 0, 1, 0)
    )
traits <- data %>%
  rename(name = species)

# Markus: get target species and put them in 'traits' matrix. Works.


## 2 Combine reference and restoration plots ##################################


species <- species_reference %>%
  left_join(species_restoration, by = "name")

sites <- sites_reference %>%
  full_join(sites_restoration, by = "plot")

#warum hier nochmal?

## 3 Malte?: Names from TNRS database #################################################


### a Harmonize names ----------------------------------------------------------

# Markus: Habe die kartierten Arten und die Zielarten zusammengefügt. Es läuft,
# glaube ich noch nicht. Malte, kannst du das prüfen?

data <- species %>%
  full_join(traits, by = "name") %>%
  rowid_to_column("id") %>%
  select(id, name) %>%
  TNRS::TNRS(
    sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
    classification = "wfo", # family classification
    mode = "resolve"
  )

names <- data %>%
  select(
    Name_submitted, Taxonomic_status, Accepted_name, Accepted_name_url,
    Accepted_family
  ) %>%
  rename_with(tolower)


### b Check and summarize duplicates -------------------------------------------

data <- species_ammer %>% 
  rename(name_submitted = name) %>%
  full_join(
    data_names %>% select(name_submitted, accepted_name), by = "name_submitted"
  )

data %>% filter(duplicated(accepted_name))

data2 <- data %>%
  group_by(accepted_name) %>%
  summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

data2 %>% filter(duplicated(accepted_name))



## 4 Get red list status ######################################################

### a Select red list status ---------------------------------------------------
redlist <- readxl::read_excel(here("data", "raw",
                                   "data_raw_species_redlist_2018.xlsx"),
                              col_names = TRUE, na = c("", "NA", "na"))
#Sina, imported the red list species

redlist_names <- TNRS(redlist$Name)
redlist[1:5312,]$Name <- redlist_names[1:5312,]$Accepted_name

#Sina, same names for species in redlist and species table, does not work

### b Combine red list status and traits




## 5 Maren?: Traits from GIFT database ################################################

# Maren kannst du den Code prüfen, wenn Sina ihn eingefügt hat?

## 5 Traits from GIFT database ################################################
traits_meta <- GIFT_traits_meta()
trait_values <- GIFT_traits(trait_IDs=c("1.6.3", "3.2.3", "4.1.3"))
#Sina, download the traits from GIFT database, no selecting of required species



## 6 Alpha diversity ##########################################################

# Dieser Code ist noch nciht auf unseren Datensatz abgestimmt. (Wenn das gemacht
# ist kann man die Sätze hier löschen)

### a Species richness -------------------------------------------------------

richness <- left_join(species_dikes, traits, by = "name") %>%
  select(
    name, rlg, rlb, target, target_herb, target_arrhenatherion,
    target_ellenberg, ffh6510, ffh6210, nitrogen_indicator, lean_indicator,
    table33, table34, starts_with("X")
  ) %>%
  pivot_longer(names_to = "id", values_to = "n", cols = starts_with("X")) %>%
  mutate(n = if_else(n > 0, 1, 0)) %>%
  group_by(id)

#### Total species richness ###
richness_total <- richness %>%
  summarise(species_richness = sum(n, na.rm = TRUE)) %>%
  ungroup()

#### Red list Germany (species richness) ###
richness_rlg <- richness %>%
  filter(rlg == "1" | rlg == "2" | rlg == "3" | rlg == "V") %>%
  summarise(rlg_richness = sum(n, na.rm = TRUE)) %>%
  ungroup()

#### Target species (species richness) ###
richness_ellenberg <- richness %>%
  filter(target != "no") %>%
  summarise(ellenberg_richness = sum(n, na.rm = TRUE)) %>%
  ungroup()

sites_dikes <- sites_dikes %>%
  right_join(richness_total, by = "id") %>%
  right_join(richness_rlg, by = "id") %>%
  right_join(richness_rlb, by = "id") %>%
  right_join(richness_target, by = "id")
  mutate(
    target_richness_ratio = target_richness / species_richness
  )


### b Species eveness and shannon ---------------------------------------------

data <- species_dikes %>%
  mutate(across(where(is.numeric), ~ replace(., is.na(.), 0))) %>%
  pivot_longer(-name, names_to = "id", values_to = "value") %>%
  pivot_wider(names_from = "name", values_from = "value") %>%
  column_to_rownames("id") %>%
  diversity(index = "shannon") %>%
  as_tibble(rownames = NA) %>%
  rownames_to_column(var = "id") %>%
  mutate(id = factor(id)) %>%
  rename(shannon = value)
sites_dikes <- sites_dikes %>%
  left_join(data, by = "id") %>%
  mutate(eveness = shannon / log(species_richness))

rm(
  list = setdiff(
    ls(), c(
      "sites_dikes", "species_dikes", "traits",
      "sites_temperature", "sites_precipitation",
      "pca_soil", "pca_construction_year", "pca_survey_year"
    )
  )
)




## 7 ?: Calculation of CWMs ######################################################

dbFD()
# dbFD(traits, species, w.abun = T, corr = "lingoes", calc.Fric = F, calc.CWM = T)
#Sina, It is better to calculate it with dbFD than as in the old script, 
# I have to look something up again 

## 8 Markus: ESy: EUNIS expert vegetation classification system #######################

#### Start ###
### Bruelheide et al. 2021 Appl Veg Sci
### https://doi.org/10.1111/avsc.12562

expertfile <- "EUNIS-ESy-2020-06-08.txt" ### file of 2021 is not working

obs <- species_dikes %>%
  pivot_longer(cols = -name,
               names_to = "RELEVE_NR",
               values_to = "Cover_Perc") %>%
  rename(TaxonName = "name") %>%
  mutate(
    TaxonName = str_replace_all(TaxonName, "_", " "),
    TaxonName = str_replace_all(TaxonName, "ssp", "subsp."),
    TaxonName = as.factor(TaxonName),
    TaxonName = fct_recode(
      TaxonName,
      "Carex praecox" = "Carex praecox subsp. curvata",
      "Cerastium fontanum" = "Cerastium fontanum subsp. vulgare",
      "Clinopodium acinos" = "Acinos arvensis",
      "Ranunculus polyanthemos" = "Ranunculus serpens subsp. nemorosus",
      "Silene latifolia" = "Silene latifolia subsp. alba",
      "Vicia villosa" = "Vicia villosa subsp. varia"
    )
  ) %>%
  data.table::as.data.table()

header <- sites_dikes %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 31468) %>%
  sf::st_transform(4326) %>%
  rename(
    RELEVE_NR = id
  ) %>%
  mutate(
    "Altitude (m)" = 313,
    Latitude = sf::st_coordinates(.)[, 2],
    Longitude = sf::st_coordinates(.)[, 1],
    Country = "Germany",
    Coast_EEA = "N_COAST",
    Dunes_Bohn = "N_DUNES",
    Ecoreg = 686,
    dataset = "Danube_dikes"
  ) %>%
  select(RELEVE_NR, "Altitude (m)", Latitude, Longitude, Country,
         Coast_EEA, Dunes_Bohn, Ecoreg, dataset) %>%
  sf::st_drop_geometry()

setwd(here("R", "esy"))
source(here("R", "esy", "code", "prep.R"))

#### Step 1 and 2: Load and parse the expert file ###
source(here("R", "esy", "code", "step1and2_load-and-parse-the-expert-file.R"))

#### Step 3: Create a numerical plot x membership condition matrix  ###
plot.cond <- array(
  0,
  c(length(unique(obs$RELEVE_NR)), length(conditions)),
  dimnames = list(
    as.character(unique(obs$RELEVE_NR)),
    conditions
  )
)

### Step 4: Aggregate taxon levels ###
source(here("R", "esy", "code", "step4_aggregate-taxon-levels.R"))

(data <- obs %>%
    group_by(TaxonName) %>%
    slice(1) %>%
    anti_join(AGG, by = c("TaxonName" = "ind")))

#### Step 5: Solve the membership conditions ###
mc <- 1
source(
  here(
    "R", "esy", "code", "step3and5_extract-and-solve-membership-conditions.R"
  )
)

table(result.classification)
eval.EUNIS(which(result.classification == "V39")[1], "V39")

sites_dikes <- sites_dikes %>%
  mutate(
    esy = result.classification,
    esy = if_else(id == "X05_m_2021", "R1A", esy),
    esy = if_else(id == "X62_m_2019", "R", esy),
    esy = if_else(id == "X67_o_2021", "R", esy)
  )
table(sites_dikes$esy)

rm(
  list = setdiff(
    ls(), c(
      "sites_dikes", "species_dikes", "traits",
      "pca_soil", "pca_construction_year", "pca_survey_year"
    )
  )
)

## 9 Finalization ############################################################


### a Rounding ----------------------------------------------------------------


### b Final selection of variables --------------------------------------------



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C Save processed data #######################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



write_csv(
  sites,
  here("data", "processed", "data_processed_sites.csv")
)
write_csv(
  species,
  here("data", "processed", "data_processed_species.csv")
)
write_csv(
  traits,
  here("data", "processed", "data_processed_traits.csv")
)
