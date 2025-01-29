#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Garchinger Heide
# Prepare data ####
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sina Appeltauer, Markus Bauer
# 2025-01-28



### Packages ###
library(renv)
library(installr)
library(here)
library(tidyverse)
library(TNRS)
library(GIFT)
library(FD)
library(vegan)
library(indicspecies)

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


# Get target species and put them in 'traits' matrix.

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

# Harmonization ran once and were than saved --> load below
#
# harmonized_names <- traits %>%
#     rowid_to_column("id") %>%
#     select(id, name) %>%
#     TNRS::TNRS(
#       sources = c("wcvp", "wfo"), # first use WCVP and alternatively WFO
#       classification = "wfo", # family classification
#       mode = "resolve"
#     )
# 
# write_csv(
#     harmonized_names, here("data", "processed", "data_processed_traits_tnrs.csv")
#     )

names_traits <- read.csv(
  here("data", "processed", "data_processed_traits_tnrs.csv")
  )

traits <- traits %>%
  rename("Name_submitted" = "name") %>%
  left_join(
    names_traits %>% select(Name_submitted, Name_matched), by = "Name_submitted"
    ) %>%
  select(Name_submitted, Name_matched, everything())



rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 3 Names from TNRS database #################################################


### a Harmonize names of species and traits matrices ---------------------------

metadata <- TNRS_metadata()
metadata$version
metadata$sources %>% tibble()

traits <- traits %>%
  mutate(
    Name_submitted = str_replace(
      Name_submitted, "Cirsium acaulon", "Cirsium acaule"
      )
    )

# Harmonization ran once and were than saved --> load below
#
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

data <- traits %>%
  right_join(
    species %>% select(accepted_name), by = c("Name_matched" = "accepted_name")
    ) %>%
  rename(accepted_name = Name_matched) %>%
  left_join(data_names, by = "accepted_name") %>%
  select(name_submitted, accepted_name, everything())

# traits <- traits %>%
#   merge(
#     species %>% select(accepted_name), 
#     by.x = "Name_matched", by.y ="accepted_name", all.y = T
#   )

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
  select(
    accepted_name, taxonomic_status, accepted_family, R1A, R22, both,
    accepted_name_url
    ) %>%
  mutate(across(where(is.numeric), replace_na, 0))

# write.csv2(traits,"data/processed/data_processed_traits_tnrs_alle.csv")

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 4 Get red list status ######################################################


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
#
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


### a Load traits from GIFT ---------------------------------------------------

trait_ids <- c("1.2.2", "1.6.3", "3.2.3", "4.1.3")

GIFT::GIFT_traits_meta() %>%
  filter(Lvl3 %in% trait_ids) %>%
  tibble()

data_gift <- GIFT::GIFT_traits(
  trait_IDs = trait_ids,
  agreement = 0.66, bias_ref = FALSE, bias_deriv = FALSE
)

# # Harmonization ran once and were than saved --> load below
# 
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
#   harmonized_names, here("data", "processed", "data_processed_gift_tnrs.csv")
#   )

# data_gift %>% filter(str_detect(work_species, "Cerastium f")) %>% select(1:2)

gift <- data.table::fread(
  # here("data", "processed", "data_processed_traits_tnrs_alle.csv")
  here("data", "processed", "data_processed_gift_tnrs.csv")
  ) %>%
  rename_with(tolower) %>%
  select(accepted_name) %>%
  left_join(
    data_gift %>%
      mutate(
        work_species = str_replace(
          work_species, "Betonica officinalis", "Stachys officinalis"
          ),
        work_species = str_replace(
          work_species, "Cerastium fontanum",
          "Cerastium fontanum subsp. vulgare"
          ),
        work_species = str_replace(
          work_species, "Asperula cynanchica",
          "Cynanchica pyrenaica subsp. cynanchica"
          ),
        work_species = str_replace(
          work_species, "Potentilla verna", "Potentilla tabernaemontani"
          ),
      ),
    by = c("accepted_name" = "work_species")
    )


### b Combine gift and traits -------------------------------------------------

data <- traits %>%
  left_join(
    gift %>%
      select(
        accepted_name, trait_value_1.2.2, trait_value_1.6.3, trait_value_3.2.3,
        trait_value_4.1.3
        ),
    by = "accepted_name"
  )

data %>% filter(duplicated(accepted_name))

data_summarized <- data %>%
  group_by(accepted_name) %>%
  summarize(across(everything(), ~ first(.x))) %>%
  select(
    accepted_name, trait_value_1.2.2, trait_value_1.6.3, trait_value_3.2.3, trait_value_4.1.3,
    everything()
  )

data_summarized  %>% filter(duplicated(accepted_name))

traits <- data_summarized



rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 6 Alpha diversity ##########################################################


### a Species richness -------------------------------------------------------

richness <- species %>%
  left_join(traits, by = "accepted_name") %>%
  select(
    accepted_name, status, redlist_germany, R1A, R22, both, starts_with("X")
  ) %>%
  pivot_longer(
    cols = starts_with("X"),  
    names_to = "plot_id",     
    values_to = "n"           
  ) %>%
  mutate(n = if_else(n > 0, 1, 0))

richness_total <- richness %>%
  group_by(plot_id) %>%
  summarise(richness_total = sum(n, na.rm = TRUE))

richness_R1A <- richness %>%
  mutate(R1A_occ = R1A * n) %>%
  group_by(plot_id) %>%
  summarise(richness_R1A = sum(R1A_occ, na.rm = TRUE)) 
  
richness_R22 <- richness %>%
  mutate(R22_occ = R22 * n) %>%
  group_by(plot_id) %>%
  summarise(richness_R22 = sum(R22_occ, na.rm = TRUE)) 

richness_both <- richness %>%
  mutate(both_occ = both * n) %>%
  group_by(plot_id) %>%
  summarise(richness_both = sum(both_occ, na.rm = TRUE))

richness_rlg <- richness %>%
  filter(n != 0) %>%
  group_by(plot_id, redlist_germany) %>%
  summarise(richness_rlg = n()) %>%
  pivot_wider(names_from = redlist_germany, values_from = richness_rlg) 

richness_rlg[is.na(richness_rlg)] <- 0

richness_rlg <- richness_rlg %>%
  rename(rlg_LC = `*`, 
         rlg_VU = `3`,
         rlg_EN = `2`,
         rlg_CR = `1`,
         rlg_NT = `V`,
         rlg_NE = `nb`,
         rlg_NA = `NA`)
         
  
full_richness <- full_join(richness_total, richness_R1A, by = "plot_id") %>%
  full_join(richness_R22, by = "plot_id") %>%
  full_join(richness_both, by = "plot_id") %>%
  full_join(richness_rlg, by = "plot_id") %>%
  rename(id = plot_id)

sites <- sites %>%
  left_join(full_richness, by = "id")

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))


### b Species eveness ---------------------------------------------

data <- as.data.frame(t(species))
colnames(data) <- data[1,]
data <- data[-1,]

data <- data %>% 
  mutate(across(where(is.character), str_trim)) %>%
  rownames_to_column(var = "id")

data <- data.frame(data[1], sapply(data[-1], as.numeric)) %>%
  column_to_rownames(var = "id")

shannon <- data %>%
  diversity(index = "shannon") %>%
  as.data.frame() %>%
  rownames_to_column(var = "id") %>%
  rename(shannon = ".")

sites <- sites %>%
  left_join(shannon, by = "id")

sites$evenness <- sites$shannon / log(sites$richness_total)

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 7 Calculation of CWMs ######################################################


trait_ids <- c("1.2.2", "1.6.3", "3.2.3", "4.1.3")

GIFT::GIFT_traits_meta() %>%
  filter(Lvl3 %in% trait_ids) %>%
  tibble()

traits_without_trees <- traits %>%
  filter(is.na(trait_value_1.2.2) | trait_value_1.2.2 != "tree") %>%
  filter(accepted_name != "Prunus spinosa") %>%
  mutate(
    trait_value_1.6.3 = log(trait_value_1.6.3),
    trait_value_3.2.3 = log(trait_value_3.2.3),
    trait_value_4.1.3 = log(trait_value_4.1.3)
      )


### a CWM Plant height 1.6.3 --------------------------------------------------

traits_height <- traits_without_trees[,c("accepted_name", "trait_value_1.6.3")]
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


### b CWM Seed mass 3.2.3 -----------------------------------------------------

traits_seed <- traits_without_trees[,c("accepted_name", "trait_value_3.2.3")]
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


### c CWM SLA 4.1.3 -----------------------------------------------------------

traits_SLA <- traits_without_trees[,c("accepted_name", "trait_value_4.1.3")]
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


### d Add to sites table ------------------------------------------------------

CWM <- full_join(CWM_Height, CWM_Seed, by = "id") %>%
  full_join(CWM_SLA, by = "id")

sites <- sites %>%
  left_join(CWM)

rm(list = setdiff(ls(), c("species", "sites", "traits", "coordinates")))



## 8 Markus: ESy: EUNIS expert vegetation classification system ################


### a Preparation --------------------------------------------------------------

# Markus: Calculate again with harmonized species names

expertfile <- "EUNIS-ESy-2020-06-08.txt" ### file of 2021 is not working

obs <- species %>%
  pivot_longer(
    cols = -accepted_name,
    names_to = "RELEVE_NR",
    values_to = "Cover_Perc"
  ) %>%
  rename(TaxonName = "accepted_name") %>%
  mutate(
    TaxonName = str_replace_all(TaxonName, "_", " "),
    TaxonName = str_replace_all(TaxonName, "ssp", "subsp."),
    TaxonName = as.factor(TaxonName),
    RELEVE_NR = as.factor(RELEVE_NR)#,
    # TaxonName = fct_recode(
    #  TaxonName,
    #  "Centaurea pannonica" = "Centaurea angustifolia",
    #  "Euphrasia picta" = "Euphrasia officinalis subsp. picta",
    #  "Euphrasia officinalis subsp. pratensis" =
    #    "Euphrasia officinalis subsp. rostkoviana",
     # "Lotus corniculatus" = "Lotus corniculatus var corniculatus",
     # "Lotus corniculatus" = "Lotus corniculatus var hirsutus"
     # )
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



## 9 Species ##################################################################

# species_pa <- species
# species_pa[,-1] <- ifelse(species[, -1] > 0, 1, 0) 
# 
# phi_taxa <- multipatt(species_pa, sites$treatment, 
#                       func="r.g", duleg=T, control=how(nperm=999))
# summary(phi_taxa)


# phi_taxa <- as.data.frame(phi_taxa$sign)
# phi_taxa$code_sp <- row.names(phi_taxa)
# phi_taxa <- merge(data.trait[,c("code_sp", "Taxa")], phi_taxa, by="code_sp")
# phi_taxa <- subset(phi_taxa, p.value<=0.051)
# write.table(phi_taxa,"output/Phi_individual_Taxa_by_Type.csv", sep=";", row.names=F, col.names=T)


## 10 Finalization ############################################################


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
