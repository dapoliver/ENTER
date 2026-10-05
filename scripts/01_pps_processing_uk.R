# devtools::install_github(
#   "ScotGovAnalysis/opendatascot",
#   upgrade = "never",
#   build_vignettes = TRUE
# )

# devtools::install_github("ropensci/PostcodesioR")

library(tidyverse) # For data wrangling
library(PostcodesioR) # For postcode mapping
library(nomisr) # For England and Wales census data
library(opendatascot) # For Scotland census data
library(FNN) # For nearest neighbours

setwd("/Users/domoliver/Library/CloudStorage/Dropbox/Work/PPS/ENTER/ENTER_PPS_Processing_UK") # Change to your working directory
PPS_ENTER <- read_csv("ENTEROnline-AllPps_DATA_2024-10-04_2124.csv")

PPS_ENTER$dob <- as.Date(paste(PPS_ENTER$pps_dob_year, PPS_ENTER$pps_dob_month, PPS_ENTER$pps_dob_day, sep = "-"))
PPS_ENTER$dob_father <- as.Date(paste(PPS_ENTER$pps_dobfather_year, PPS_ENTER$pps_dobfather_month, PPS_ENTER$pps_dobfather_day, sep = "-"))

# Reverse code CTQ items
PPS_ENTER$pps_trauma_16_rev <- 6- PPS_ENTER$pps_trauma_16
PPS_ENTER$pps_trauma_17_rev <- 6- PPS_ENTER$pps_trauma_17
PPS_ENTER$pps_trauma_18_rev <- 6- PPS_ENTER$pps_trauma_18
PPS_ENTER$pps_trauma_19_rev <- 6- PPS_ENTER$pps_trauma_19
PPS_ENTER$pps_trauma_20_rev <- 6- PPS_ENTER$pps_trauma_20
PPS_ENTER$pps_trauma_22_rev <- 6- PPS_ENTER$pps_trauma_22
PPS_ENTER$pps_trauma_25_rev <- 6- PPS_ENTER$pps_trauma_25

# Reverse code TEPS items
PPS_ENTER[,c(329,330, 332:346)] <- 7 - PPS_ENTER[,c(329,330, 332:346)]

##### Clean postcode data #####
PPS_ENTER <- PPS_ENTER %>%
  mutate(postcode_clean = gsub(" ", "", pps_postcode_1),
         postcode_clean = toupper(gsub(",", "", postcode_clean)),
         postcode_clean = case_when(nchar(postcode_clean)>4 ~ postcode_clean,
                                    TRUE ~ NA),
         postcode_fragment = case_when(nchar(postcode_clean)==6 ~ substr(postcode_clean, 1, 4),
                                       nchar(postcode_clean)==5 ~ substr(postcode_clean, 1, 3),
                                       TRUE ~ substr(postcode_clean, 1, 5)))# Modify this length based on your density fragments

scot_post_council <- read_csv("SmallUser.csv") # From https://www.nrscotland.gov.uk/statistics-and-data/geography/nrs-postcode-extract
scot_post_council <- scot_post_council %>%
  mutate(postcode_clean = gsub(" ", "", Postcode))
scot_post_council <- scot_post_council %>% subset(select=c("postcode_clean","CouncilArea2019Code"))

for (i in 1:nrow(PPS_ENTER)){
  if (!is.na(PPS_ENTER$postcode_clean[i])) {
    PPS_ENTER$EW_MSOA[i] <- postcode_lookup(PPS_ENTER$postcode_clean[i])$msoa_code
    PPS_ENTER$scot_MSOA[i] <- scottish_postcode_lookup(PPS_ENTER$postcode_clean[i])$scottish_parliamentary_constituency_code
  }
}

# Perform a left join to add CouncilArea2019Code to df1 based on matching postcodes
PPS_ENTER <- PPS_ENTER %>%
  left_join(scot_post_council, by = c("postcode_clean" = "postcode_clean"),relationship = "many-to-many") %>%
  dplyr::rename(CouncilArea = CouncilArea2019Code)  # Optionally rename to a more descriptive column name

##### Ethnic Density - England & Wales #####

ethnic_search <- nomis_search(name = "*ethnic*")
ethnic_check <- tibble(ethnic_search)

ethnic_measures <- nomis_get_metadata("NM_2041_1", "measures")

ethnic_geography <- nomis_get_metadata("NM_2041_1", "geography", "TYPE")

ethnic_density <- nomis_get_data(id = "NM_2041_1", time = "latest", geography = "TYPE152",
                    measures=c(20100, 20301))
ethnic_density <- ethnic_density %>% filter(C2021_ETH_20_NAME=="White" & MEASURES_NAME=="Percent" & !is.na(OBS_VALUE))
ethnic_density <- ethnic_density %>% mutate(ethnic_density=(100-OBS_VALUE)/100,
                                            ethnic_density_ew = case_when(ethnic_density>quantile(ethnic_density, 0.666, na.rm = TRUE) ~ "High",
                                                                            ethnic_density<quantile(ethnic_density, 0.333, na.rm = TRUE) ~ "Low",
                                                                            TRUE ~ "Medium")) %>% subset(select=c(GEOGRAPHY_CODE, ethnic_density_ew))

PPS_ENTER <- PPS_ENTER %>%
  left_join(ethnic_density, by = c("EW_MSOA" = "GEOGRAPHY_CODE"))

##### Ethnic Density - Scotland #####

scotland_ethnic_density <- read_csv("UV201 - Ethnic group.csv")
scotland_ethnic_density <- scotland_ethnic_density %>% mutate(ethnic_density=1-(`White: Total`/`All People`)) 
scotland_ethnic_density <- scotland_ethnic_density %>% mutate(ethnic_density_scot = case_when(ethnic_density>quantile(ethnic_density, 0.666, na.rm = TRUE) ~ "High",
                                                                                              ethnic_density<quantile(ethnic_density, 0.333, na.rm = TRUE) ~ "Low",
                                                                                              TRUE ~ "Medium")) %>%
  subset(select=c(...1, ethnic_density_scot))

OA <- read_csv("Postcode_to_OA.csv") # From https://www.nrscotland.gov.uk/statistics-and-data/geography/our-products/census-datasets/2022-census/2022-census-indexes
OA <- OA %>%
  mutate(postcode_clean = gsub(" ", "", Postcode))
OA <- OA %>% subset(select=c(postcode_clean, OutputArea2022Code))

PPS_ENTER <- PPS_ENTER %>%
  left_join(OA, by = c("postcode_clean" = "postcode_clean"))

PPS_ENTER <- PPS_ENTER %>%
  left_join(scotland_ethnic_density, by = c("OutputArea2022Code" = "...1"))

##### Population Density - England & Wales #####

population_density <- nomis_get_data(id = "NM_2026_1", time = "latest", geography = "TYPE152",
                                 measures=c(20100)) # km2
population_density <- population_density %>% mutate(urbanicity_ew = case_when(OBS_VALUE>300 ~ 1,
                                                                           TRUE ~ 0))
population_density <- population_density %>% subset(select=c(GEOGRAPHY_CODE, urbanicity_ew))

PPS_ENTER <- PPS_ENTER %>%
  left_join(population_density, by = c("EW_MSOA" = "GEOGRAPHY_CODE"))

##### Population Density - Scotland #####

scotland <- opendatascot::ods_all_datasets()
scot_land <- opendatascot::ods_dataset("land-area-2011-data-zone-based",
                                       geography = "la") # in hectares

scot_land <- scot_land %>% dplyr::rename(area = value) %>% subset(select=c(refArea, area))

scot_pop <- opendatascot::ods_dataset("population-estimates-2011-datazone-linked-dataset",
                                      geography = "la") %>% filter(age=="all" & sex=="all")
scot_pop_clean <- as.data.frame(scot_pop)
scot_pop_clean$refArea <- as.factor(scot_pop_clean$refArea)
scot_pop_clean <- scot_pop_clean %>% arrange(desc(refPeriod)) %>% group_by(refArea) %>% slice(1)
scot_pop_clean <- scot_pop_clean %>% dplyr::rename(population = value) %>% subset(select=c(refArea, population))

scot_pop_dens <- merge(scot_pop_clean, scot_land, by="refArea")
scot_pop_dens <- scot_pop_dens %>% mutate(density = population/area,
                                          urbanicity_scot = case_when(density>3 ~ 1, TRUE ~ 0)) %>%
  subset(select=c(refArea, urbanicity_scot))
PPS_ENTER <- PPS_ENTER %>%
  left_join(scot_pop_dens, by = c("CouncilArea" = "refArea"))

#### Pollution ####
# Extract the outward code by isolating all characters up to the last letter in each postcode
PPS_ENTER <- PPS_ENTER %>%
  mutate(outward_code = str_extract(postcode_clean, "^[A-Za-z]+[0-9]"))
pollution_area <- read_csv("postcodes.csv") # from https://github.com/radiac/UK-Postcodes/blob/master/postcodes.csv
pollution_area <- pollution_area %>% subset(select=c(postcode, eastings, northings)) 
pollution <- read_csv("mappm252023g.csv") # from UK AIR: Air Information Resource
pollution <- pollution %>% dplyr::rename(gridcode=pm2.5, eastings=...2, northings = ...3, pm2.5=...4)
pollution <- pollution %>% filter(!is.na(pm2.5) & gridcode!="2023") %>% subset(select=c(-gridcode))
# pollution <- pollution %>% mutate(pollution = case_when(pm2.5>quantile(pm2.5, 0.9, na.rm = TRUE) ~ 1,
#                                                         is.na(pm2.5) ~ NA,
#                                                         TRUE ~ 0))
pollution <- pollution %>% mutate(pollution = pm2.5)
# Join on the outward code
PPS_ENTER <- PPS_ENTER %>%
  left_join(pollution_area, by = c("outward_code" = "postcode"))

# Remove rows with NAs in eastings or northings in both data frames
PPS_ENTER_poll <- PPS_ENTER %>% filter(!is.na(eastings) & !is.na(northings))
pollution_clean <- pollution %>% filter(!is.na(eastings) & !is.na(northings))

# Convert to matrices after NA removal
PPS_matrix <- as.matrix(PPS_ENTER_poll[, c("eastings", "northings")])
pollution_matrix <- as.matrix(pollution_clean[, c("eastings", "northings")])

# Find nearest neighbor
nearest <- get.knnx(data = pollution_matrix, query = PPS_matrix, k = 1)

# Extract the nearest pollution data based on indices
nearest_pollution <- pollution_clean[nearest$nn.index[, 1], ]

# Combine the cleaned PPS_ENTER data with the nearest pollution data
PPS_ENTER_poll <- cbind(PPS_ENTER_poll, nearest_pollution)
PPS_ENTER_poll <- PPS_ENTER_poll %>% subset(select=c(enterid,pollution))

PPS_ENTER <- PPS_ENTER %>%
  left_join(PPS_ENTER_poll, by = "enterid", relationship = "many-to-many")

##### Clean up England/Wales and Scotland variables #####
PPS_ENTER <- PPS_ENTER %>% mutate(urbanicity = case_when(!is.na(urbanicity_ew) ~ urbanicity_ew,
                                                         TRUE ~ urbanicity_scot),
                                  ethnic_density = case_when(!is.na(ethnic_density_ew) ~ ethnic_density_ew,
                                                            TRUE ~ ethnic_density_scot))
# Deduplicate data frame so one row per ID
PPS_ENTER_dedup <- PPS_ENTER %>%
  group_by(enterid) %>%
  arrange(desc(!is.na(outward_code))) %>%  # Arrange so non-NA 'outward_code' comes first
  slice(1) %>%                             # Keep only the first row per group
  ungroup()

PPS_ENTER_clean <- data.frame(enterid=PPS_ENTER_dedup$enterid)

##### Assemble cleaned PPS factors ####
PPS_ENTER_clean <- PPS_ENTER_clean %>% mutate(age = as.numeric(difftime(PPS_ENTER_dedup$pps_start, PPS_ENTER_dedup$dob, units = "days")) / 365.25,
                                              gender = case_when(PPS_ENTER_dedup$pps_gend_a==1 ~ "Male",
                                                                 PPS_ENTER_dedup$pps_gend_a==2 ~ "Female",
                                                                 PPS_ENTER_dedup$pps_gend_a==3 ~ "Non-binary",
                                                                 PPS_ENTER_dedup$pps_gend_a==4 ~ "Other"),
                                              handedness = case_when(rowMeans(PPS_ENTER_dedup[,c("pps_hand_a","pps_hand_b","pps_hand_c","pps_hand_d")],na.rm=TRUE)>3 ~ 0,
                                                                     is.na(rowMeans(PPS_ENTER_dedup[,c("pps_hand_a","pps_hand_b","pps_hand_c","pps_hand_d")],na.rm=TRUE)) ~ NA,
                                                                     TRUE ~ 1),
                                              ethnicity = case_when(PPS_ENTER_dedup$pps_ethnic_a==1 ~ "White",
                                                                    PPS_ENTER_dedup$pps_ethnic_a==2 ~ "Black",
                                                                    PPS_ENTER_dedup$pps_ethnic_a==3 ~ "Asian",
                                                                    PPS_ENTER_dedup$pps_ethnic_a==4 ~ "Mixed",
                                                                    PPS_ENTER_dedup$pps_ethnic_a==5 ~ "Other"),
                                              ethnic_density = PPS_ENTER_dedup$ethnic_density,
                                              pollution = PPS_ENTER_dedup$pollution,
                                              high_urbanicity = PPS_ENTER_dedup$urbanicity,
                                              first_gen_migrant = case_when(PPS_ENTER_dedup$pps_migrant==2 | PPS_ENTER_dedup$pps_migrant==3 ~ 1,
                                                                            is.na(PPS_ENTER_dedup$pps_migrant) ~ NA,
                                                                            TRUE ~ 0),
                                              second_gen_migrant = case_when(PPS_ENTER_dedup$pps_migrant==4 ~ 1,
                                                                            is.na(PPS_ENTER_dedup$pps_migrant) ~ NA,
                                                                            TRUE ~ 0),
                                              north_african_first_gen_migrant = case_when(first_gen_migrant==1 & (PPS_ENTER_dedup$pps_country==3 | PPS_ENTER_dedup$pps_country==98 | PPS_ENTER_dedup$pps_country==109 | PPS_ENTER_dedup$pps_country==117 | PPS_ENTER_dedup$pps_country==179) ~ 1,
                                                                                          first_gen_migrant==0 | (PPS_ENTER_dedup$pps_country!=3 & PPS_ENTER_dedup$pps_country!=98 & PPS_ENTER_dedup$pps_country!=109 & PPS_ENTER_dedup$pps_country!=117 & PPS_ENTER_dedup$pps_country!=179) ~ 0,
                                                                                          TRUE ~ NA),
                                              north_african_second_gen_migrant = case_when(second_gen_migrant==1 & (PPS_ENTER_dedup$pps_countryfather_b==3 | PPS_ENTER_dedup$pps_countryfather_b==98 | PPS_ENTER_dedup$pps_countryfather_b==109 | PPS_ENTER_dedup$pps_countryfather_b==117 | PPS_ENTER_dedup$pps_countryfather_b==179 | PPS_ENTER_dedup$pps_countrymother_b==3 | PPS_ENTER_dedup$pps_countrymother_b==98 | PPS_ENTER_dedup$pps_countrymother_b==109 | PPS_ENTER_dedup$pps_countrymother_b==117 | PPS_ENTER_dedup$pps_countrymother_b==179) ~ 1,
                                                                                           second_gen_migrant==0 | (PPS_ENTER_dedup$pps_countryfather_b!=3 & PPS_ENTER_dedup$pps_countryfather_b!=98 & PPS_ENTER_dedup$pps_countryfather_b!=109 & PPS_ENTER_dedup$pps_countryfather_b!=117 & PPS_ENTER_dedup$pps_countryfather_b!=179 & PPS_ENTER_dedup$pps_countrymother_b!=3 & PPS_ENTER_dedup$pps_countrymother_b!=98 & PPS_ENTER_dedup$pps_countrymother_b!=109 & PPS_ENTER_dedup$pps_countrymother_b!=117 & PPS_ENTER_dedup$pps_countrymother_b!=179) ~ 0,
                                                                                          TRUE ~ NA),
                                              paternal_age = case_when((as.numeric(difftime(PPS_ENTER_dedup$pps_start, PPS_ENTER_dedup$dob_father, units = "days")) / 365.25) > 0 ~ (as.numeric(difftime(PPS_ENTER_dedup$pps_start, PPS_ENTER_dedup$dob_father, units = "days")) / 365.25)-age,
                                                                       TRUE ~ NA),
                                              paternal_ses = case_when(is.na(PPS_ENTER_dedup$pps_jobfather) ~ NA,
                                                                       PPS_ENTER_dedup$pps_jobfather>6 ~ 1,
                                                                       TRUE ~ 0),
                                              parent_smi = case_when(rowSums(PPS_ENTER_dedup[,c("pps_parental_a","pps_parental_b","pps_parental_c","pps_parental_d",
                                                                                          "pps_parental_e","pps_parental_f","pps_parental_g","pps_parental_h",
                                                                                          "pps_parental_i","pps_parental_j")],na.rm=TRUE)>0 ~ 1,
                                                                     is.na(rowSums(PPS_ENTER_dedup[,c("pps_parental_a","pps_parental_b","pps_parental_c","pps_parental_d",
                                                                                                 "pps_parental_e","pps_parental_f","pps_parental_g","pps_parental_h",
                                                                                                 "pps_parental_i","pps_parental_j")],na.rm=TRUE)) ~ NA,
                                                                     TRUE ~ 0),
                                              adult_life_events = case_when(PPS_ENTER_dedup$pps_events___14==1 ~ 0,
                                                                            is.na(rowSums(PPS_ENTER_dedup[,c("pps_events___1","pps_events___2","pps_events___3","pps_events___4",
                                                                                                       "pps_events___5","pps_events___6","pps_events___7","pps_events___8",
                                                                                                       "pps_events___9","pps_events___10","pps_events___11","pps_events___12"
                                                                                                       ,"pps_events___13","pps_events___14")],na.rm=TRUE)) ~ NA,
                                                                            TRUE ~ rowSums(PPS_ENTER_dedup[,c("pps_events___1","pps_events___2","pps_events___3","pps_events___4",
                                                                                                               "pps_events___5","pps_events___6","pps_events___7","pps_events___8",
                                                                                                               "pps_events___9","pps_events___10","pps_events___11","pps_events___12"
                                                                                                               ,"pps_events___13","pps_events___14")],na.rm=TRUE)),
                                              daily_tobacco = case_when(PPS_ENTER_dedup$pps_tobacco==1 ~ 1,
                                                                        is.na(PPS_ENTER_dedup$pps_tobacco) ~ NA,
                                                                        TRUE ~ 0),
                                              heavy_cannabis_use = case_when(PPS_ENTER_dedup$pps_cannabis==1 & PPS_ENTER_dedup$pps_tobaccofreq<3 ~ 1,
                                                                             is.na(PPS_ENTER_dedup$pps_cannabis) & is.na(PPS_ENTER_dedup$pps_tobaccofreq)~ NA,
                                                                             TRUE ~ 0),
                                              heavy_alcohol_use = case_when(
                                                PPS_ENTER_dedup$pps_subsyear_1==1 & PPS_ENTER_dedup$pps_subs_1 > 3 ~ 1,
                                                !is.na(PPS_ENTER_dedup$pps_subsyear_1) ~ 0
                                              ),
                                              past_month_stimulants = case_when(
                                                (PPS_ENTER_dedup$pps_subsyear_2 == 1 & PPS_ENTER_dedup$pps_subsmonth_2 == 1) | (PPS_ENTER_dedup$pps_subsyear_3 == 1 & PPS_ENTER_dedup$pps_subsmonth_3 == 1) | (PPS_ENTER_dedup$pps_subsyear_4 == 1 & PPS_ENTER_dedup$pps_subsmonth_4 == 1)  ~ 1,
                                                !is.na(PPS_ENTER_dedup$pps_subsyear_2) & !is.na(PPS_ENTER_dedup$pps_subsyear_3) & !is.na(PPS_ENTER_dedup$pps_subsyear_4) ~ 0
                                              ),
                                              past_month_mdma = case_when(
                                                PPS_ENTER_dedup$pps_subsyear_5 == 1 & PPS_ENTER_dedup$pps_subsmonth_5 == 1 ~ 1,
                                                !is.na(PPS_ENTER_dedup$pps_subsyear_5) ~ 0
                                              ),
                                              past_month_ketamine = case_when(
                                                PPS_ENTER_dedup$pps_subsyear_6 == 1 & PPS_ENTER_dedup$pps_subsmonth_6 == 1 ~ 1,
                                                !is.na(PPS_ENTER_dedup$pps_subsyear_6) ~ 0
                                              ),
                                              past_month_heroin = case_when(
                                                PPS_ENTER_dedup$pps_subsyear_7 == 1 & PPS_ENTER_dedup$pps_subsmonth_7 == 1 ~ 1,
                                                !is.na(PPS_ENTER_dedup$pps_subsyear_7) ~ 0
                                              ),
                                              past_month_benzos = case_when(
                                                PPS_ENTER_dedup$pps_subsyear_8 == 1 & PPS_ENTER_dedup$pps_subsmonth_8 == 1 ~ 1,
                                                !is.na(PPS_ENTER_dedup$pps_subsyear_8) ~ 0
                                              ),
                                              past_month_nitrous = case_when(
                                                PPS_ENTER_dedup$pps_subsyear_9 == 1 & PPS_ENTER_dedup$pps_subsmonth_9 == 1 ~ 1,
                                                !is.na(PPS_ENTER_dedup$pps_subsyear_9) ~ 0
                                              ),
                                              past_month_ghb = case_when(
                                                PPS_ENTER_dedup$pps_subsyear_10 == 1 & PPS_ENTER_dedup$pps_subsmonth_10 == 1 ~ 1,
                                                !is.na(PPS_ENTER_dedup$pps_subsyear_10) ~ 0
                                              ),
                                              past_month_synth_cannabinoids = case_when(
                                                PPS_ENTER_dedup$pps_subsyear_13 == 1 & PPS_ENTER_dedup$pps_subsmonth_13 == 1 ~ 1,
                                                !is.na(PPS_ENTER_dedup$pps_subsyear_13) ~ 0
                                              ),
                                              past_month_psychedelics = case_when(
                                                PPS_ENTER_dedup$pps_subsyear_14 == 1 & PPS_ENTER_dedup$pps_subsmonth_14 == 1 ~ 1,
                                                !is.na(PPS_ENTER_dedup$pps_subsyear_14) ~ 0
                                              ),
                                              childhood_trauma = case_when(rowSums(PPS_ENTER_dedup[,c("pps_trauma_1","pps_trauma_2","pps_trauma_3","pps_trauma_4",
                                                                                      "pps_trauma_5","pps_trauma_6","pps_trauma_7","pps_trauma_8",
                                                                                      "pps_trauma_9","pps_trauma_10",
                                                                                      "pps_trauma_11","pps_trauma_12","pps_trauma_13","pps_trauma_14",
                                                                                      "pps_trauma_15","pps_trauma_16_rev","pps_trauma_17_rev","pps_trauma_18_rev",
                                                                                      "pps_trauma_19_rev","pps_trauma_20_rev",
                                                                                      "pps_trauma_21","pps_trauma_22_rev","pps_trauma_23","pps_trauma_24",
                                                                                      "pps_trauma_25_rev")],na.rm=TRUE)>0 ~ rowSums(PPS_ENTER_dedup[,c("pps_trauma_1","pps_trauma_2","pps_trauma_3","pps_trauma_4",
                                                                                                                                                       "pps_trauma_5","pps_trauma_6","pps_trauma_7","pps_trauma_8",
                                                                                                                                                       "pps_trauma_9","pps_trauma_10",
                                                                                                                                                       "pps_trauma_11","pps_trauma_12","pps_trauma_13","pps_trauma_14",
                                                                                                                                                       "pps_trauma_15","pps_trauma_16_rev","pps_trauma_17_rev","pps_trauma_18_rev",
                                                                                                                                                       "pps_trauma_19_rev","pps_trauma_20_rev",
                                                                                                                                                       "pps_trauma_21","pps_trauma_22_rev","pps_trauma_23","pps_trauma_24",
                                                                                                                                                       "pps_trauma_25_rev")],na.rm=TRUE),
                                                                           is.na(rowSums(PPS_ENTER_dedup[,c("pps_trauma_1","pps_trauma_2","pps_trauma_3","pps_trauma_4",
                                                                                                            "pps_trauma_5","pps_trauma_6","pps_trauma_7","pps_trauma_8",
                                                                                                            "pps_trauma_9","pps_trauma_10",
                                                                                                            "pps_trauma_11","pps_trauma_12","pps_trauma_13","pps_trauma_14",
                                                                                                            "pps_trauma_15","pps_trauma_16_rev","pps_trauma_17_rev","pps_trauma_18_rev",
                                                                                                            "pps_trauma_19_rev","pps_trauma_20_rev",
                                                                                                            "pps_trauma_21","pps_trauma_22_rev","pps_trauma_23","pps_trauma_24",
                                                                                                            "pps_trauma_25_rev")],na.rm=FALSE)) ~ NA,
                                                                           TRUE ~ 0),
                                              emotional_abuse = case_when(
                                                rowSums(PPS_ENTER_dedup[, c(
                                                  "pps_trauma_1", "pps_trauma_2", "pps_trauma_3",
                                                  "pps_trauma_4", "pps_trauma_5"
                                                )], na.rm = TRUE) > 0 ~ rowSums(PPS_ENTER_dedup[, c(
                                                  "pps_trauma_1", "pps_trauma_2", "pps_trauma_3",
                                                  "pps_trauma_4", "pps_trauma_5"
                                                )], na.rm = TRUE),
                                                is.na(rowSums(PPS_ENTER_dedup[, c(
                                                  "pps_trauma_1", "pps_trauma_2", "pps_trauma_3",
                                                  "pps_trauma_4", "pps_trauma_5"
                                                )], na.rm = FALSE)) ~ NA_real_,
                                                TRUE ~ 0
                                              ),
                                              
                                              physical_abuse = case_when(
                                                rowSums(PPS_ENTER_dedup[, c(
                                                  "pps_trauma_6", "pps_trauma_7", "pps_trauma_8",
                                                  "pps_trauma_9", "pps_trauma_10"
                                                )], na.rm = TRUE) > 0 ~ rowSums(PPS_ENTER_dedup[, c(
                                                  "pps_trauma_6", "pps_trauma_7", "pps_trauma_8",
                                                  "pps_trauma_9", "pps_trauma_10"
                                                )], na.rm = TRUE),
                                                is.na(rowSums(PPS_ENTER_dedup[, c(
                                                  "pps_trauma_6", "pps_trauma_7", "pps_trauma_8",
                                                  "pps_trauma_9", "pps_trauma_10"
                                                )], na.rm = FALSE)) ~ NA_real_,
                                                TRUE ~ 0
                                              ),
                                              
                                              sexual_abuse = case_when(
                                                rowSums(PPS_ENTER_dedup[, c(
                                                  "pps_trauma_11", "pps_trauma_12", "pps_trauma_13",
                                                  "pps_trauma_14", "pps_trauma_15"
                                                )], na.rm = TRUE) > 0 ~ rowSums(PPS_ENTER_dedup[, c(
                                                  "pps_trauma_11", "pps_trauma_12", "pps_trauma_13",
                                                  "pps_trauma_14", "pps_trauma_15"
                                                )], na.rm = TRUE),
                                                is.na(rowSums(PPS_ENTER_dedup[, c(
                                                  "pps_trauma_11", "pps_trauma_12", "pps_trauma_13",
                                                  "pps_trauma_14", "pps_trauma_15"
                                                )], na.rm = FALSE)) ~ NA_real_,
                                                TRUE ~ 0
                                              ),
                                              
                                              emotional_neglect = case_when(
                                                rowSums(PPS_ENTER_dedup[, c(
                                                  "pps_trauma_16_rev", "pps_trauma_17_rev", "pps_trauma_18_rev",
                                                  "pps_trauma_19_rev", "pps_trauma_20_rev"
                                                )], na.rm = TRUE) > 0 ~ rowSums(PPS_ENTER_dedup[, c(
                                                  "pps_trauma_16_rev", "pps_trauma_17_rev", "pps_trauma_18_rev",
                                                  "pps_trauma_19_rev", "pps_trauma_20_rev"
                                                )], na.rm = TRUE),
                                                is.na(rowSums(PPS_ENTER_dedup[, c(
                                                  "pps_trauma_16_rev", "pps_trauma_17_rev", "pps_trauma_18_rev",
                                                  "pps_trauma_19_rev", "pps_trauma_20_rev"
                                                )], na.rm = FALSE)) ~ NA_real_,
                                                TRUE ~ 0
                                              ),
                                              
                                              physical_neglect = case_when(
                                                rowSums(PPS_ENTER_dedup[, c(
                                                  "pps_trauma_21", "pps_trauma_22_rev", "pps_trauma_23",
                                                  "pps_trauma_24", "pps_trauma_25_rev"
                                                )], na.rm = TRUE) > 0 ~ rowSums(PPS_ENTER_dedup[, c(
                                                  "pps_trauma_21", "pps_trauma_22_rev", "pps_trauma_23",
                                                  "pps_trauma_24", "pps_trauma_25_rev"
                                                )], na.rm = TRUE),
                                                is.na(rowSums(PPS_ENTER_dedup[, c(
                                                  "pps_trauma_21", "pps_trauma_22_rev", "pps_trauma_23",
                                                  "pps_trauma_24", "pps_trauma_25_rev"
                                                )], na.rm = FALSE)) ~ NA_real_,
                                                TRUE ~ 0
                                              ),
                                              neighbourhood_cohesion = case_when(rowSums(is.na(PPS_ENTER_dedup[,c("pps_neighbourhood_1_a","pps_neighbourhood_1_b","pps_neighbourhood_1_c","pps_neighbourhood_1_d","pps_neighbourhood_1_e","pps_neighbourhood_1_f","pps_neighbourhood_1_g","pps_neighbourhood_1_h")])) <5 ~ rowSums(PPS_ENTER_dedup[,c("pps_neighbourhood_1_a","pps_neighbourhood_1_b","pps_neighbourhood_1_c","pps_neighbourhood_1_d","pps_neighbourhood_1_e","pps_neighbourhood_1_f","pps_neighbourhood_1_g","pps_neighbourhood_1_h")])),
                                              neighbourhood_social_disorganisation = case_when(rowSums(is.na(PPS_ENTER_dedup[,c("pps_neighbourhood_2_a","pps_neighbourhood_2_b","pps_neighbourhood_2_c","pps_neighbourhood_2_d","pps_neighbourhood_2_e")])) <5 ~ rowSums(PPS_ENTER_dedup[,c("pps_neighbourhood_2_a","pps_neighbourhood_2_b","pps_neighbourhood_2_c","pps_neighbourhood_2_d","pps_neighbourhood_2_e")])),
                                              anhedonia = case_when(rowSums(PPS_ENTER_dedup[,c(329:346)], na.rm=TRUE)>0 ~ rowSums(PPS_ENTER_dedup[,c(329:346)], na.rm=TRUE),
                                                                    is.na(rowSums(PPS_ENTER_dedup[,c(329:346)], na.rm=FALSE)) ~ NA,
                                                                    TRUE ~ 0),
                                              PPS = 5.297 +
                                                (0.085 * handedness) +
                                                (-0.318 * ifelse(!is.na(ethnic_density) & !is.na(ethnicity) & ethnic_density == "High" & ethnicity == "Black", 1, 0)) +
                                                (0.726 * ifelse(!is.na(ethnic_density) & !is.na(ethnicity) & ethnic_density == "Medium" & ethnicity == "Black", 1, 0)) +
                                                (0.726 * ifelse(!is.na(ethnicity) & ethnicity == "Black", 1, 0)) +
                                                (-1.473 * ifelse(!is.na(ethnic_density) & !is.na(ethnicity) & ethnic_density == "High" & ethnicity != "Black" & ethnicity != "White", 1, 0)) +
                                                (-3.996 * ifelse(!is.na(pollution), pollution, 0)) +
                                                (-0.581 * ifelse(!is.na(high_urbanicity), high_urbanicity, 0)) +
                                                (-1.657 * ifelse(!is.na(first_gen_migrant) & !is.na(second_gen_migrant) & first_gen_migrant == 0 & second_gen_migrant == 0, 1, 0)) +
                                                (-0.466 * ifelse(!is.na(first_gen_migrant) & first_gen_migrant == 1, 1, 0)) +
                                                (2.142 * ifelse(!is.na(paternal_ses), paternal_ses, 0)) +
                                                (0.934 * ifelse(!is.na(parent_smi), parent_smi, 0)) +
                                                (0.263 * ifelse(!is.na(adult_life_events), adult_life_events, 0)) +
                                                (0.671 * ifelse(!is.na(daily_tobacco), daily_tobacco, 0)) +
                                                (0.703 * ifelse(!is.na(heavy_cannabis_use), heavy_cannabis_use, 0)) +
                                                (1.800 * ifelse(!is.na(childhood_trauma) & childhood_trauma > 55, 1, 0)) +
                                                (2.410 * ifelse(!is.na(anhedonia) & anhedonia > 35, 1, 0)),
                                              PPS_new = 1.19 +
                                                   (0.351 * ifelse(age>24 & age<36 & gender=='Male', 1, 0)) - 
                                                   (2.174 * ifelse(!is.na(ethnic_density) & !is.na(ethnicity) & ethnic_density == "Low" & ethnicity == "Black", 1, 0)) +
                                                   (-2.431 * ifelse(!is.na(ethnic_density) & !is.na(ethnicity) & ethnic_density == "High" & ethnicity != "Black" & ethnicity != "White", 1, 0)) +
                                                   (3.067 * ifelse(!is.na(ethnic_density) & !is.na(ethnicity) & ethnic_density == "Medium" & ethnicity != "Black" & ethnicity != "White", 1, 0)) +
                                                   (1.459 * ifelse(!is.na(ethnic_density) & !is.na(ethnicity) & ethnic_density == "Low" & ethnicity != "Black" & ethnicity != "White", 1, 0)) +
                                                   (0.376 * ifelse(!is.na(ethnicity) & ethnicity == "White", 1, 0)) +
                                                   (-4.463 * ifelse(!is.na(pollution), pollution, 0)) +
                                                   (0.976 * ifelse(!is.na(second_gen_migrant) & second_gen_migrant == 1, 1, 0)) +
                                                   (2.340 * ifelse(!is.na(north_african_second_gen_migrant) & north_african_second_gen_migrant == 1, 1, 0)) +
                                                  (0.593 * ifelse(!is.na(first_gen_migrant) & first_gen_migrant == 1, 1, 0)) +
                                                   (1.262 * ifelse(!is.na(paternal_ses), paternal_ses, 0)) +
                                                   (1.191 * ifelse(!is.na(parent_smi), parent_smi, 0)) +
                                                   (0.228 * ifelse(!is.na(adult_life_events), adult_life_events, 0)) +
                                                   (1.187 * ifelse(!is.na(heavy_cannabis_use), heavy_cannabis_use, 0)) +
                                                   (3.153 * ifelse(!is.na(childhood_trauma) & childhood_trauma > 55, 1, 0)) +
                                                  (0.746 * ifelse(!is.na(daily_tobacco) & daily_tobacco > 0, 1, 0)) +
                                                  (3.039 * ifelse(!is.na(anhedonia) & anhedonia > 35, 1, 0)))


write_csv(PPS_ENTER_clean, "PPS_ENTER_clean_190626.csv")
                                                   