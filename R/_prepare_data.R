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

coordinates_reference <- read_csv2(
  here("data", "raw", "data_raw_coordinates_reference.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types = cols(.default = "?")
) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 25832) %>%
  sf::st_transform(4326) %>%
  mutate(
    latitude = sf::st_coordinates(.)[, 2],
    longitude = sf::st_coordinates(.)[, 1]
  ) %>%
  sf::st_drop_geometry()

coordinates_restoration <- read_csv(
  here("data", "raw", "data_raw_coordinates_restoration.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types = cols(.default = "?")
) %>%
  mutate(
    id = paste0("X2024", plot)
  ) %>%
  select(id, plot, latitude, longitude)

coordinates <- coordinates_reference %>%
  bind_rows(coordinates_restoration)



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



## 3 FloraVeg.EU species #######################################################

# Chytrý et al. (2020) Appl Veg Sci https://doi.org/10.1111/avsc.12519
# Version 2021-06-01: https://doi.org/10.5281/zenodo.4812736


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

rm(
  list = setdiff(ls(), c(
    "species_reference", "species_restoration", "sites_reference",
    "sites_restoration", "traits", "coordinates"
    ))
  )



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B Create variables ###########################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Combine reference and restoration plots ##################################

# Markus: combination works

species <- species_reference %>%
  full_join(species_restoration, by = "name") %>%
  pivot_longer(-name, names_to = "plot", values_to = "value") %>%
  mutate(
    id = if_else(str_detect(plot, "^res"), paste0("X2024", plot), plot)
  ) %>%
  filter(!is.na(value)) %>%
  select(-plot) %>%
  pivot_wider(names_from = "id", values_from = "value")

sites <- sites_reference %>%
  full_join(sites_restoration, by = "plot") %>%
  mutate(
    id = if_else(str_detect(plot, "^tum"), paste0("X2021", plot), plot),
    id = if_else(str_detect(plot, "^res"), paste0("X2024", plot), id),
    elevation = if_else(is.na(elevation), 469, elevation),
    plot_size = if_else(is.na(plot_size), 4, plot_size),
    treatment = if_else(is.na(treatment), "control", treatment)
  ) %>%
  unite("botanist", c("botanist_2021", "botanist_2024"), na.rm = TRUE) %>%
  unite(
    "survey_date", c("survey_date_2021", "survey_date_2024"), na.rm = TRUE
    ) %>%
  unite(
    "height_vegetation", c("height_vegetation_2021", "height_vegetation_2024"),
    na.rm = TRUE
    ) %>%
  unite(
    "cover_vegetation", c("cover_vegetation_2021", "cover_vegetation_2024"),
    na.rm = TRUE
    ) %>%
  unite("cover_moss", c("cover_moss_2021", "cover_moss_2024"), na.rm = TRUE) %>%
  unite(
    "cover_litter", c("cover_litter_2021", "cover_litter_2024"), na.rm = TRUE
    ) %>%
  unite("cover_soil", c("cover_soil_2021", "cover_soil_2024"), na.rm = TRUE)
  

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 2 Select target species from FloraVeg.EU ###################################

# Markus: get target species and put them in 'traits' matrix. Works.

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



## 3 Names from TNRS database #################################################


### a Harmonize names of species and traits matrices ---------------------------

# Markus: API of TNRS was not reached. Run TNRS first once successfully

# Run only once to save time (following times load the file):
harmonized_names <- species %>%
  full_join(traits, by = "name") %>% # combine with target species list
  rowid_to_column("id") %>%
  select(id, name) %>%
  TNRS::TNRS(
    sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
    classification = "wfo", # family classification WFO
    mode = "resolve"
  )
write.csv(
  harmonized_names, here("data", "processed", "data_processed_names_tnrs.csv")
  )

names_species <- read.csv(
  here("data", "processed", "data_processed_names_tnrs.csv")
  ) %>%
  select(
    Name_submitted, Taxonomic_status, Accepted_name, Accepted_name_url,
    Accepted_family
  ) %>%
  rename_with(tolower)


### b Summarize duplicates of species matrix -----------------------------------

#Sina: works
data <- species %>% 
  rename(name_submitted = name) %>%
  full_join(
    names_species %>% select(name_submitted, accepted_name),
    by = "name_submitted"
  )

data %>% filter(duplicated(accepted_name))

data2 <- data %>%
  group_by(accepted_name) %>%
  summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

data2 %>% filter(duplicated(accepted_name))

species <- data2
rm(data2, data)


### c Harmonize names of traits matrix -----------------------------------------

harmonized_names <- traits %>%
  rowid_to_column("id") %>%
  select(id, name) %>%
  mutate(name = str_replace(name, "Cirsium acaulon", "Cirsium acaule")) %>%
  TNRS::TNRS(
    sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
    classification = "wfo", # family classification
    mode = "resolve"
    )
write_csv(
    harmonized_names, here("data", "processed", "data_processed_traits_tnrs.csv")
    )

names_traits <- read.csv(
  here("data", "processed", "data_processed_traits_tnrs.csv")
  ) %>%
  select(
    Name_submitted, Taxonomic_status, Accepted_name, Accepted_name_url,
    Accepted_family
  ) %>%
  rename_with(tolower)


### d Summarize duplicates of traits matrix -----------------------------------

data <- traits %>%
  full_join(
    names_traits, by = c("name" = "name_submitted")
  ) %>%
  select(name, everything())

data %>% filter(duplicated(name))

data2 <- data %>%
  merge(
    names_traits %>% select(accepted_name), 
    by.x = "name", by.y = "accepted_name", all.y = TRUE
  )

traits %>% filter(duplicated(name))

data3 <- data2 %>%
  group_by(name) %>%
  summarize(across(where(is.numeric), ~ first(.x)))

traits[is.na(traits)] <- 0



## 4 Get red list status ######################################################


### a Load red list ------------------------------------------------------------

data <- readxl::read_excel(
  here("data", "raw", "data_raw_species_redlist_2018.xlsx"),
  col_names = TRUE, na = c("", "NA", "na")
  ) %>%
  rename(redlist_germany = "RL Kat.", responsibility = Verantwortlichkeit) %>%
  rename_with(tolower) %>%
  select(name, status, redlist_germany)

# Calculate just once to save time

# harmonized_names <- data %>%
#   rowid_to_column("id") %>%
#   select(id, name) %>%
#   TNRS::TNRS(
#     sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
#     classification = "wfo", # family classification
#     mode = "resolve"
#   )
# 
# write_csv(
#   harmonized_names,
#   here("data", "processed", "data_processed_redlist_tnrs.csv")
#   )

redlist <- read_csv(
  here("data", "processed", "data_processed_redlist_tnrs.csv"),
  col_names = TRUE, na = c("", "NA", "na"), col_types =
    cols(.default = "?")
  ) %>%
  select(
    Name_submitted, Taxonomic_status, Accepted_name, Accepted_name_url,
    Accepted_family
    ) %>%
  rename_with(tolower) %>%
  rename(name = accepted_name, family = accepted_family) %>%
  full_join(data, by = "name")

# irgendwas passt hier nicht


### b Combine red list status and traits --------------------------------------

# Merge in traits table. Wait for step 3
# data2 <- traits %>%
#   left_join(
#     redlist %>% select(name, family, status, redlist_germany), by = "name"
#     )
# traits <- data2

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 5 Traits from GIFT database ################################################

#Markus: Works BUT see b)

### a Load traits from GIFT ---------------------------------------------------

trait_ids <- c("1.6.3", "3.2.3", "4.1.3")

GIFT::GIFT_traits_meta() %>%
  filter(Lvl3 %in% trait_ids) # Get an overview of selected traits


data <- GIFT::GIFT_traits(
  trait_IDs = trait_ids,
  agreement = 0.66, bias_ref = FALSE, bias_deriv = FALSE
)

# Run name harmonization just once to save time (following time load file)
# harmonized_names <- data %>%
#   rowid_to_column("id") %>%
#   select(id, work_species) %>%
#   TNRS::TNRS(
#     sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
#     classification = "wfo", # family classification
#     mode = "resolve"
#   )
# 
# write_csv(
#   harmonized_names, here("data", "processed", "data_processed_traits_tnrs.csv")
#   )

gift <- data.table::fread(
  here("data", "processed", "data_processed_traits_tnrs.csv")
  ) %>%
  select(Name_matched) %>%
  rename_with(tolower) %>%
  rename(name = name_matched) %>%
  full_join(data %>% rename(name = work_species), by = "name")


### b Combine gift and traits -------------------------------------------------

# Merge in traits table. Wait for step 3

traits <- traits %>%
  left_join(
    gift %>% select(name, trait_value_1.6.3, trait_value_3.2.3, trait_value_4.1.3), by = "name"
  )


rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))




## 6 Alpha diversity ##########################################################

# Dieser Code ist noch nciht auf unseren Datensatz abgestimmt. (Wenn das gemacht
# ist kann man die Sätze hier löschen)

### a Species richness -------------------------------------------------------

# warum nutzen wir nicht einfach die traits tabelle?
richness <- species %>%
  left_join(traits, by = "name") %>%
  select(
    name, status, redlist_germany, target, starts_with("X")
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
richness_target <- richness %>%
  filter(target != "no") %>%
  summarise(target_richness = sum(n, na.rm = TRUE)) %>%
  ungroup()

sites_dikes <- sites_dikes %>%
  right_join(richness_total, by = "id") %>%
  right_join(richness_rlg, by = "id") %>%
  right_join(richness_target, by = "id")
  mutate(
    target_richness_ratio = target_richness / species_richness
  )


### b Species eveness and shannon ---------------------------------------------

# Calculate Hill numbers with hillR-package and hill_taxa function
# of Chao et al. (2014) Annu Rev
# https://doi.org/10.1146/annurev-ecolsys-120213-091540
  
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

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))




## 7 ?: Calculation of CWMs ######################################################

dbFD()
# dbFD(traits, species, w.abun = T, corr = "lingoes", calc.Fric = F, calc.CWM = T)
#Sina, It is better to calculate it with dbFD than as in the old script, 
# I have to look something up again 

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 8 Markus: ESy: EUNIS expert vegetation classification system ################


### a Preparation --------------------------------------------------------------

# Markus: Calculate again with harmonized species names

expertfile <- "EUNIS-ESy-2020-06-08.txt" ### file of 2021 is not working

obs <- species %>%
  pivot_longer(
    cols = -name,
    names_to = "RELEVE_NR",
    values_to = "Cover_Perc"
  ) %>%
  rename(TaxonName = "name") %>%
  mutate(
    TaxonName = str_replace_all(TaxonName, "_", " "),
    TaxonName = str_replace_all(TaxonName, "ssp", "subsp."),
    TaxonName = as.factor(TaxonName),
    RELEVE_NR = as.factor(RELEVE_NR),
    TaxonName = fct_recode(
     TaxonName,
     "Centaurea pannonica" = "Centaurea angustifolia",
     "Euphrasia picta" = "Euphrasia officinalis subsp. picta",
     "Euphrasia officinalis subsp. pratensis" =
       "Euphrasia officinalis subsp. rostkoviana"#,
     # "Lotus corniculatus" = "Lotus corniculatus var corniculatus",
     # "Lotus corniculatus" = "Lotus corniculatus var hirsutus"
     )
  ) %>%
  filter(!is.na(Cover_Perc)) %>%
  data.table::as.data.table()


# Coordinates are in WGS84
header <- sites %>%
  left_join(coordinates %>% select(-plot), by = "id") %>%
  rename(
    RELEVE_NR = id,
    Latitude = latitude,
    Longitude = longitude,
    "Altitude (m)" = elevation
  ) %>%
  mutate(
    RELEVE_NR = as.factor(RELEVE_NR),
    Country = "Germany",
    Coast_EEA = "N_COAST",
    Dunes_Bohn = "N_DUNES",
    Ecoreg = 686,
    dataset = "garchinger_heide"
  ) %>%
  select(
    RELEVE_NR, "Altitude (m)", Latitude, Longitude, Country,
    Coast_EEA, Dunes_Bohn, Ecoreg, dataset
  )


### b Calculation --------------------------------------------------------------

### Bruelheide et al. 2021 Appl Veg Sci
### https://doi.org/10.1111/avsc.12562

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


### c Summary and integration --------------------------------------------------

table(result.classification)
eval.EUNIS(which(result.classification == "//?")[2], "//?")

data <- sites %>%
  mutate(
    esy = result.classification#,
    # esy = if_else(id == "X05_xxx", "R1A", esy) # nothing to change
  )
table(data$esy)
sites <- data

rm(list = setdiff(ls(), c("species", "sites", "traits")))



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
