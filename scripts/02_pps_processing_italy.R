library(tidyverse) # For data wrangling
library(readxl) # For reading in data

setwd("/Users/domoliver/Library/CloudStorage/Dropbox/Work/PPS/ENTER/ENTER_PPS_Processing_IT") # Change to your working directory
PPS_ENTER <- read_csv("ENTERScreenerIT.csv")
postcodes <- read_excel("postcodes.xlsx", sheet = "Postcodes")

PPS_ENTER$dob <- as.Date(paste(PPS_ENTER$pps_dob_year, PPS_ENTER$pps_dob_month, PPS_ENTER$pps_dob_day, sep = "-"))
PPS_ENTER$dob_father <- as.Date(paste(PPS_ENTER$pps_dobfather_year, PPS_ENTER$pps_dobfather_month, PPS_ENTER$pps_dobfather_day, sep = "-"))
PPS_ENTER$pps_start <- as.Date(PPS_ENTER$pps_start, format = "%d/%m/%Y")

# Reverse code CTQ items
PPS_ENTER$pps_trauma_16_rev <- 6 - PPS_ENTER$pps_trauma_16
PPS_ENTER$pps_trauma_17_rev <- 6 - PPS_ENTER$pps_trauma_17
PPS_ENTER$pps_trauma_18_rev <- 6 - PPS_ENTER$pps_trauma_18
PPS_ENTER$pps_trauma_19_rev <- 6 - PPS_ENTER$pps_trauma_19
PPS_ENTER$pps_trauma_20_rev <- 6 - PPS_ENTER$pps_trauma_20
PPS_ENTER$pps_trauma_22_rev <- 6 - PPS_ENTER$pps_trauma_22
PPS_ENTER$pps_trauma_25_rev <- 6 - PPS_ENTER$pps_trauma_25

# Reverse code TEPS items
PPS_ENTER[, c(329, 330, 332:346)] <- 7 - PPS_ENTER[, c(329, 330, 332:346)]

##### Ethnic Density - England & Wales #####
ethnicity <- read_excel("Ethnicities.xlsx")
file_path <- "Ethnicities.xlsx"

# Get the sheet names
sheet_names <- excel_sheets(file_path)

# Initialize an empty list to store key numbers
ethnic_density <- data.frame(
  city = rep(NA, length(sheet_names)),
  foreigners = rep(NA, length(sheet_names)),
  asia_africa = rep(NA, length(sheet_names)),
  ethnic_density_prop = rep(NA, length(sheet_names))
)
i <- 1
# Loop over each sheet and extract key numbers
for (sheet in sheet_names) {
  # Read the sheet into a data frame
  sheet_data <- read_excel(file_path, sheet = sheet)
  ethnic_density$city[i] <- sheet
  column_name <- colnames(sheet_data)[2]
  ethnic_density$foreigners[i] <- as.numeric(str_extract(column_name, "\\d+\\.\\d+")) / 100
  asia <- sheet_data %>% filter(grepl("Total Asia", ...1))
  africa <- sheet_data %>% filter(grepl("Total Africa", ...1))

  ethnic_density$asia_africa[i] <- (as.numeric(str_extract(asia[, 6], "\\d+\\.\\d+")) / 100) + (as.numeric(str_extract(africa[, 6], "\\d+\\.\\d+")) / 100)
  ethnic_density$ethnic_density_prop[i] <- ethnic_density$foreigners[i] * ethnic_density$asia_africa[i]

  i <- i + 1
}

ethnic_density <- ethnic_density %>% mutate(ethnic_density = case_when(
  ethnic_density_prop > quantile(ethnic_density_prop, 0.666, na.rm = TRUE) ~ "High",
  ethnic_density_prop < quantile(ethnic_density_prop, 0.333, na.rm = TRUE) ~ "Low",
  TRUE ~ "Medium"
))


ethnic_density <- ethnic_density %>%
  left_join(postcodes, by = c("city" = "City"))

ethnic_density <- ethnic_density[complete.cases(ethnic_density), ] %>% subset(select = c(Postcode, ethnic_density))

PPS_ENTER <- PPS_ENTER %>%
  left_join(ethnic_density, by = c("pps_postcode_1" = "Postcode"))

##### Population Density #####
urbanicity <- read_excel("postcodes.xlsx", sheet = "Density")
urbanicity <- merge(urbanicity, postcodes, by = "City", all.x = TRUE)
urbanicity <- urbanicity %>%
  mutate(urbanicity = case_when(
    `Density (citizens per square kilometer)` > 300 ~ 1,
    is.na(`Density (citizens per square kilometer)`) ~ NA,
    TRUE ~ 0
  )) %>%
  subset(select = c(Postcode, urbanicity)) %>%
  group_by(Postcode) %>%
  slice(1)

PPS_ENTER <- PPS_ENTER %>%
  left_join(urbanicity, by = c("pps_postcode_1" = "Postcode"))

#### Pollution ####
pollution <- read_excel("postcodes.xlsx", sheet = "pm2.5")
pollution <- pollution[complete.cases(pollution), ] %>%
  group_by(city, province) %>%
  slice(1)
pollution <- merge(pollution, postcodes, by.x = c("province", "city"), by.y = c("Province", "City"), all.x = TRUE)
# pollution <- pollution %>%
#   mutate(pollution = case_when(
#     `average yearly PM2,5` > quantile(`average yearly PM2,5`, 0.9, na.rm = TRUE) ~ 1,
#     is.na(`average yearly PM2,5`) ~ NA,
#     TRUE ~ 0
#   )) %>%
#   subset(select = c(Postcode, pollution))
pollution <- pollution %>%
  mutate(pollution = `average yearly PM2,5`
  ) %>%
  subset(select = c(Postcode, pollution))
pollution <- pollution[complete.cases(pollution), ]

# Join on postcode
PPS_ENTER <- PPS_ENTER %>%
  left_join(pollution, by = c("pps_postcode_1" = "Postcode"))

# Deduplicate data frame so one row per ID
PPS_ENTER_dedup <- PPS_ENTER %>%
  group_by(enterid) %>%
  arrange(desc(!is.na(pps_postcode_1))) %>% # Arrange so non-NA 'Postcode' comes first
  slice(1) %>% # Keep only the first row per group
  ungroup()

PPS_ENTER_clean <- data.frame(enterid = PPS_ENTER_dedup$enterid)

##### Assemble cleaned PPS factors ####
PPS_ENTER_clean <- PPS_ENTER_clean %>% mutate(
  age = as.numeric(difftime(PPS_ENTER_dedup$pps_start, PPS_ENTER_dedup$dob, units = "days")) / 365.25,
  gender = case_when(
    PPS_ENTER_dedup$pps_gend_a == 1 ~ "Male",
    PPS_ENTER_dedup$pps_gend_a == 2 ~ "Female",
    PPS_ENTER_dedup$pps_gend_a == 3 ~ "Non-binary",
    PPS_ENTER_dedup$pps_gend_a == 4 ~ "Other"
  ),
  handedness = case_when(
    rowMeans(PPS_ENTER_dedup[, c("pps_hand_a", "pps_hand_b", "pps_hand_c", "pps_hand_d")], na.rm = TRUE) > 3 ~ 0,
    is.na(rowMeans(PPS_ENTER_dedup[, c("pps_hand_a", "pps_hand_b", "pps_hand_c", "pps_hand_d")], na.rm = TRUE)) ~ NA,
    TRUE ~ 1
  ),
  ethnicity = case_when(
    PPS_ENTER_dedup$pps_ethnic_a == 1 ~ "White",
    PPS_ENTER_dedup$pps_ethnic_a == 2 ~ "Black",
    PPS_ENTER_dedup$pps_ethnic_a == 3 ~ "Asian",
    PPS_ENTER_dedup$pps_ethnic_a == 4 ~ "Mixed",
    PPS_ENTER_dedup$pps_ethnic_a == 5 ~ "Other"
  ),
  ethnic_density = PPS_ENTER_dedup$ethnic_density,
  pollution = PPS_ENTER_dedup$pollution,
  high_urbanicity = PPS_ENTER_dedup$urbanicity,
  first_gen_migrant = case_when(
    PPS_ENTER_dedup$pps_migrant == 2 | PPS_ENTER_dedup$pps_migrant == 3 ~ 1,
    is.na(PPS_ENTER_dedup$pps_migrant) ~ NA,
    TRUE ~ 0
  ),
  second_gen_migrant = case_when(
    PPS_ENTER_dedup$pps_migrant == 4 ~ 1,
    is.na(PPS_ENTER_dedup$pps_migrant) ~ NA,
    TRUE ~ 0
  ),
  north_african_first_gen_migrant = case_when(
    first_gen_migrant == 1 & (PPS_ENTER_dedup$pps_country == 3 | PPS_ENTER_dedup$pps_country == 98 | PPS_ENTER_dedup$pps_country == 109 | PPS_ENTER_dedup$pps_country == 117 | PPS_ENTER_dedup$pps_country == 179) ~ 1,
    first_gen_migrant == 0 | (PPS_ENTER_dedup$pps_country != 3 & PPS_ENTER_dedup$pps_country != 98 & PPS_ENTER_dedup$pps_country != 109 & PPS_ENTER_dedup$pps_country != 117 & PPS_ENTER_dedup$pps_country != 179) ~ 0,
    TRUE ~ NA
  ),
  north_african_second_gen_migrant = case_when(
    second_gen_migrant == 1 & (PPS_ENTER_dedup$pps_countryfather_b == 3 | PPS_ENTER_dedup$pps_countryfather_b == 98 | PPS_ENTER_dedup$pps_countryfather_b == 109 | PPS_ENTER_dedup$pps_countryfather_b == 117 | PPS_ENTER_dedup$pps_countryfather_b == 179 | PPS_ENTER_dedup$pps_countrymother_b == 3 | PPS_ENTER_dedup$pps_countrymother_b == 98 | PPS_ENTER_dedup$pps_countrymother_b == 109 | PPS_ENTER_dedup$pps_countrymother_b == 117 | PPS_ENTER_dedup$pps_countrymother_b == 179) ~ 1,
    second_gen_migrant == 0 | (PPS_ENTER_dedup$pps_countryfather_b != 3 & PPS_ENTER_dedup$pps_countryfather_b != 98 & PPS_ENTER_dedup$pps_countryfather_b != 109 & PPS_ENTER_dedup$pps_countryfather_b != 117 & PPS_ENTER_dedup$pps_countryfather_b != 179 & PPS_ENTER_dedup$pps_countrymother_b != 3 & PPS_ENTER_dedup$pps_countrymother_b != 98 & PPS_ENTER_dedup$pps_countrymother_b != 109 & PPS_ENTER_dedup$pps_countrymother_b != 117 & PPS_ENTER_dedup$pps_countrymother_b != 179) ~ 0,
    TRUE ~ NA
  ),
  paternal_age = case_when(
    (as.numeric(difftime(PPS_ENTER_dedup$pps_start, PPS_ENTER_dedup$dob_father, units = "days")) / 365.25) > 0 ~ (as.numeric(difftime(PPS_ENTER_dedup$pps_start, PPS_ENTER_dedup$dob_father, units = "days")) / 365.25) - age,
    TRUE ~ NA
  ),
  paternal_ses = case_when(
    is.na(PPS_ENTER_dedup$pps_jobfather) ~ NA,
    PPS_ENTER_dedup$pps_jobfather > 6 ~ 1,
    TRUE ~ 0
  ),
  parent_smi = case_when(
    rowSums(PPS_ENTER_dedup[, c(
      "pps_parental_a", "pps_parental_b", "pps_parental_c", "pps_parental_d",
      "pps_parental_e", "pps_parental_f", "pps_parental_g", "pps_parental_h",
      "pps_parental_i", "pps_parental_j"
    )], na.rm = TRUE) > 0 ~ 1,
    is.na(rowSums(PPS_ENTER_dedup[, c(
      "pps_parental_a", "pps_parental_b", "pps_parental_c", "pps_parental_d",
      "pps_parental_e", "pps_parental_f", "pps_parental_g", "pps_parental_h",
      "pps_parental_i", "pps_parental_j"
    )], na.rm = TRUE)) ~ NA,
    TRUE ~ 0
  ),
  adult_life_events = case_when(
    PPS_ENTER_dedup$pps_events___14 == 1 ~ 0,
    is.na(rowSums(PPS_ENTER_dedup[, c(
      "pps_events___1", "pps_events___2", "pps_events___3", "pps_events___4",
      "pps_events___5", "pps_events___6", "pps_events___7", "pps_events___8",
      "pps_events___9", "pps_events___10", "pps_events___11", "pps_events___12",
      "pps_events___13", "pps_events___14"
    )], na.rm = TRUE)) ~ NA,
    TRUE ~ rowSums(PPS_ENTER_dedup[,c("pps_events___1","pps_events___2","pps_events___3","pps_events___4",
                                      "pps_events___5","pps_events___6","pps_events___7","pps_events___8",
                                      "pps_events___9","pps_events___10","pps_events___11","pps_events___12"
                                      ,"pps_events___13","pps_events___14")],na.rm=TRUE)
  ),
  daily_tobacco = case_when(
    PPS_ENTER_dedup$pps_tobacco == 1 ~ 1,
    is.na(PPS_ENTER_dedup$pps_tobacco) ~ NA,
    TRUE ~ 0
  ),
  heavy_cannabis_use = case_when(
    PPS_ENTER_dedup$pps_cannabis == 1 & PPS_ENTER_dedup$pps_tobaccofreq < 3 ~ 1,
    is.na(PPS_ENTER_dedup$pps_cannabis) & is.na(PPS_ENTER_dedup$pps_tobaccofreq) ~ NA,
    TRUE ~ 0
  ),
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
  childhood_trauma = case_when(
    rowSums(PPS_ENTER_dedup[, c(
      "pps_trauma_1", "pps_trauma_2", "pps_trauma_3", "pps_trauma_4",
      "pps_trauma_5", "pps_trauma_6", "pps_trauma_7", "pps_trauma_8",
      "pps_trauma_9", "pps_trauma_10",
      "pps_trauma_11", "pps_trauma_12", "pps_trauma_13", "pps_trauma_14",
      "pps_trauma_15", "pps_trauma_16_rev", "pps_trauma_17_rev", "pps_trauma_18_rev",
      "pps_trauma_19_rev", "pps_trauma_20_rev",
      "pps_trauma_21", "pps_trauma_22_rev", "pps_trauma_23", "pps_trauma_24",
      "pps_trauma_25_rev"
    )], na.rm = TRUE) > 0 ~ rowSums(PPS_ENTER_dedup[, c(
      "pps_trauma_1", "pps_trauma_2", "pps_trauma_3", "pps_trauma_4",
      "pps_trauma_5", "pps_trauma_6", "pps_trauma_7", "pps_trauma_8",
      "pps_trauma_9", "pps_trauma_10",
      "pps_trauma_11", "pps_trauma_12", "pps_trauma_13", "pps_trauma_14",
      "pps_trauma_15", "pps_trauma_16_rev", "pps_trauma_17_rev", "pps_trauma_18_rev",
      "pps_trauma_19_rev", "pps_trauma_20_rev",
      "pps_trauma_21", "pps_trauma_22_rev", "pps_trauma_23", "pps_trauma_24",
      "pps_trauma_25_rev"
    )], na.rm = TRUE),
    is.na(rowSums(PPS_ENTER_dedup[, c(
      "pps_trauma_1", "pps_trauma_2", "pps_trauma_3", "pps_trauma_4",
      "pps_trauma_5", "pps_trauma_6", "pps_trauma_7", "pps_trauma_8",
      "pps_trauma_9", "pps_trauma_10",
      "pps_trauma_11", "pps_trauma_12", "pps_trauma_13", "pps_trauma_14",
      "pps_trauma_15", "pps_trauma_16_rev", "pps_trauma_17_rev", "pps_trauma_18_rev",
      "pps_trauma_19_rev", "pps_trauma_20_rev",
      "pps_trauma_21", "pps_trauma_22_rev", "pps_trauma_23", "pps_trauma_24",
      "pps_trauma_25_rev"
    )], na.rm = FALSE)) ~ NA,
    TRUE ~ 0
  ),
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
  anhedonia = case_when(
    rowSums(PPS_ENTER_dedup[, c(329:346)], na.rm = TRUE) > 0 ~ rowSums(PPS_ENTER_dedup[, c(329:346)], na.rm = TRUE),
    is.na(rowSums(PPS_ENTER_dedup[, c(329:346)], na.rm = FALSE)) ~ NA,
    TRUE ~ 0
  ),
  PPS = 1.19 +
    (0.351 * ifelse(age > 24 & age < 36 & gender == "Male", 1, 0)) -
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
    (3.039 * ifelse(!is.na(anhedonia) & anhedonia > 35, 1, 0))
)


write_csv(PPS_ENTER_clean, "PPS_ENTER_IT_clean_190626.csv")
