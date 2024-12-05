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
library(FD)

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
  ) %>%
  mutate(name = str_replace_all(name, "_", " "))

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

# Get target species and put them in 'traits' matrix. Markus: Works

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

# harmonized_names <- traits %>%
#     rowid_to_column("id") %>%
#     select(id, name) %>%
#     TNRS::TNRS(
#       sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
#       classification = "wfo", # family classification
#       mode = "resolve"
#     )
 
# write_csv(
#     harmonized_names, here("data", "processed", "data_processed_traits_tnrs.csv")
#     )

names_traits <- read.csv("data/processed/data_processed_traits_tnrs.csv")

traits <- traits %>%
  rename("Name_submitted" = "name") %>%
  left_join(
    names_traits %>% select(Name_submitted, Name_matched)
    , by = "Name_submitted") %>%
  select(Name_submitted, Name_matched, everything())



rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 3 Names from TNRS database #################################################

#Sina + Markus: works

### a Harmonize names of species and traits matrices ---------------------------

metadata <- TNRS_metadata()
metadata$version
metadata$sources %>% tibble()

traits <- traits %>%
  mutate(Name_submitted = str_replace(Name_submitted, "Cirsium acaulon", "Cirsium acaule"))

# Harmonization ran once and were than saved --> load below
# harmonized_names <- species %>%
#   full_join(traits, by = "name") %>% # combine with target species list
#   rowid_to_column("id") %>%
#   select(id, name) %>%
#   TNRS::TNRS(
#     sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
#     classification = "wfo", # family classification WFO
#     mode = "resolve"
#   )
# write.csv(
#   harmonized_names, here("data", "processed", "data_processed_names_tnrs.csv")
#   )

data_names <- read.csv(
  here("data", "processed", "data_processed_names_tnrs.csv")
  ) %>%
  select(
    Name_submitted, Taxonomic_status, Accepted_name, Accepted_name_url,
    Accepted_family
  ) %>%
  rename_with(tolower)


### b Summarize duplicates of species matrix -----------------------------------

data <- species %>% 
  rename(name_submitted = name) %>%
  left_join(
    data_names %>% select(name_submitted, accepted_name),
    by = "name_submitted"
  ) %>%
  select(name_submitted, accepted_name, everything())

data %>% filter(duplicated(accepted_name))

data_summarized <- data %>%
  group_by(accepted_name) %>%
  summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

data_summarized %>% filter(duplicated(accepted_name))

species <- data_summarized


### c Summarize duplicates of traits matrix ------------------------------------
# wichtig fuer unten bitte nicht loeschen! # 
traits <- traits %>%
  merge(
    species %>% select(accepted_name), 
    by.x = "Name_matched", by.y ="accepted_name", all.y = T
  )

data <- traits %>% 
  rename(accepted_name = Name_matched) %>%
  left_join(data_names, by = "accepted_name") %>%
  select(name_submitted, accepted_name, everything())

data %>% filter(duplicated(accepted_name))

data_summarized <- data %>%
  group_by(accepted_name) %>%
  summarize(across(everything(), ~ first(.x))) %>%
  select(
    name_submitted, accepted_name, taxonomic_status, accepted_family,
    everything()
    )

data_summarized %>% filter(duplicated(accepted_name))

traits <- data_summarized %>%
  select(accepted_name, taxonomic_status, accepted_family, 
         R1A, R22, both)

traits[is.na(traits)] <- 0

write.csv2(traits,"data/processed/data_processed_traits_tnrs_alle.csv")

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 4 Get red list status ######################################################

# Markus: Works

### a Load red list ------------------------------------------------------------

data_redlist <- readxl::read_excel(
  here("data", "raw", "data_raw_species_redlist_2018.xlsx"),
  col_names = TRUE, na = c("", "NA", "na")
  ) %>%
  rename(redlist_germany = "RL Kat.", responsibility = Verantwortlichkeit) %>%
  rename_with(tolower) %>%
  select(name, status, redlist_germany) %>%
  mutate(
    name = str_replace(
      name, "Cirsium acaulon \\(L\\.\\) Scop\\.", "Cirsium acaule"
      )
    )

# Calculate just once to save time (afterwards load file)
# harmonized_names <- data_redlist %>%
#   rowid_to_column("id") %>%
#   select(id, name) %>%
#   TNRS::TNRS(
#     sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
#     classification = "wfo", # family classification
#     mode = "resolve"
#   )
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
  full_join(
    data_redlist %>% rename(name_submitted = name), by = "name_submitted"
    )


### b Combine red list status and traits --------------------------------------

data <- traits %>%
  left_join(
    redlist %>% select(accepted_name, status, redlist_germany),
    by = "accepted_name"
  )


data %>% filter(duplicated(accepted_name))

data_summarized <- data %>%
  group_by(accepted_name) %>%
  summarize(across(everything(), ~ first(.x))) %>%
  select(
    accepted_name, taxonomic_status, accepted_family,
    everything()
  )

data_summarized  %>% filter(duplicated(accepted_name))

traits <- data_summarized


rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 5 Traits from GIFT database ################################################

#Markus: Works BUT see b)

### a Load traits from GIFT ---------------------------------------------------

trait_ids <- c("1.2.2", "1.6.3", "3.2.3", "4.1.3")

GIFT::GIFT_traits_meta() %>%
  filter(Lvl3 %in% trait_ids) %>%
  tibble()

data_gift <- GIFT::GIFT_traits(
  trait_IDs = trait_ids,
  agreement = 0.66, bias_ref = FALSE, bias_deriv = FALSE
)

# Harmonization ran once and were than saved --> load below

# harmonized_names <- data_gift %>%
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

data_gift %>% filter(str_detect(work_species, "Cerastium f")) %>% select(1:2)

gift <- data.table::fread(
  here("data", "processed", "data_processed_traits_tnrs_alle.csv")
  ) %>%
  select(accepted_name) %>%
  rename_with(tolower) %>%
  rename(name = accepted_name) %>%
  left_join(
    data_gift %>%
      rename(name = work_species) %>%
      mutate(
        name = str_replace(name, "Betonica officinalis", "Stachys officinalis"),
        name = str_replace(
          name, "Cerastium fontanum", "Cerastium fontanum subsp. vulgare"
          ),
        name = str_replace(
          name, "Asperula cynanchica", "Cynanchica pyrenaica subsp. cynanchica"
          ),
        name = str_replace(
          name, "Potentilla verna", "Potentilla tabernaemontani"
          ),
      ),
    by = "name"
    ) %>%
  rename(accepted_name = name)

gift %>% filter(duplicated(accepted_name)) %>% tibble()
gift %>% filter(str_detect(accepted_name, "Dactylis g")) %>% select(1:2)


### b Combine gift and traits -------------------------------------------------

# Markus: Solve C. acaulon problem

data <- traits %>%
  left_join(
    gift %>%
      select(
        accepted_name, trait_value_1.6.3, trait_value_3.2.3, trait_value_4.1.3
        ),
    by = "accepted_name"
  )

data %>% filter(duplicated(accepted_name))

data_summarized <- data %>%
  group_by(accepted_name) %>%
  summarize(across(everything(), ~ first(.x))) %>%
  select(
    accepted_name, trait_value_1.6.3, trait_value_3.2.3, trait_value_4.1.3,
    everything()
  )

data_summarized  %>% filter(duplicated(accepted_name))

traits <- data_summarized



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


### b Species eveness ---------------------------------------------

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




## 7: Calculation of CWMs ######################################################

# CWM Plant height 1.6.3

traits_height <- traits[,c("accepted_name", "trait_value_1.6.3")]
traits_height <- na.omit(traits_height)

species_height <- species[species$accepted_name %in% traits_height$accepted_name, ]
species_height <- as.data.frame(t(species_height))
colnames(species_height) <- species_height[1,] 
species_height <- species_height[-1,]
species_height <- species_height %>% mutate_all(as.numeric)

traits_height <- traits_height %>%
  column_to_rownames(var="accepted_name")

CWM.Height <- dbFD(traits_height, species_height, 
                   w.abun=T, corr="lingoes", calc.FRic=F, calc.CWM=T)

# Mean value
CWM_Height <- CWM.Height$CWM
colnames(CWM_Height) <- "CWM_Height"
CWM_Height$id <- row.names(CWM_Height)

# CWM Seed mass 3.2.3

traits_seed <- traits[,c("accepted_name", "trait_value_3.2.3")]
traits_seed <- na.omit(traits_seed)

species_seed <- species[species$accepted_name %in% traits_seed$accepted_name, ]
species_seed <- as.data.frame(t(species_seed))
colnames(species_seed) <- species_seed[1,] 
species_seed <- species_seed[-1,]
species_seed <- species_seed %>% mutate_all(as.numeric)

traits_seed <- traits_seed %>%
  column_to_rownames(var="accepted_name")

CWM.Seed <- dbFD(traits_seed, species_seed, 
                   w.abun=T, corr="lingoes", calc.FRic=F, calc.CWM=T)

# Mean value
CWM_Seed <- CWM.Seed$CWM
colnames(CWM_Seed) <- "CWM_Seed"
CWM_Seed$id <- row.names(CWM_Seed)


# CWM SLA 4.1.3

traits_SLA <- traits[,c("accepted_name", "trait_value_4.1.3")]
traits_SLA <- na.omit(traits_SLA)

species_SLA <- species[species$accepted_name %in% traits_SLA$accepted_name, ]
species_SLA <- as.data.frame(t(species_SLA))
colnames(species_SLA) <- species_SLA[1,] 
species_SLA <- species_SLA[-1,]
species_SLA <- species_SLA %>% mutate_all(as.numeric)

traits_SLA <- traits_SLA %>%
  column_to_rownames(var="accepted_name")

CWM.SLA <- dbFD(traits_SLA, species_SLA, 
                 w.abun=T, corr="lingoes", calc.FRic=F, calc.CWM=T)

# Mean value
CWM_SLA <- CWM.SLA$CWM
colnames(CWM_SLA) <- "CWM_SLA"
CWM_SLA$id <- row.names(CWM_SLA)
 
### b: Add to sites table ------------------------------------------------------

CWM <- full_join(CWM_Height, CWM_Seed, by = "id")
CWM <- full_join(CWM, CWM_SLA, by = "id")

sites <- sites %>%
  left_join(CWM)

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
