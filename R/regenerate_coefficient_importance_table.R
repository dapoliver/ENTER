library(dplyr)
library(readr)
library(tidyr)

analyses_dir <- "/Users/domoliver/Library/CloudStorage/Dropbox/Work/ENTER/CAARMS Prediction/Analyses"
old_output_dir <- file.path(analyses_dir, "revised_enter_unimodal_unbalanced_one_se_regularised_trees_steps1to4")
elastic_net_output_dir <- file.path(analyses_dir, "revised_enter_step5_unbalanced_one_se_regularised_trees")
output_dir <- file.path(analyses_dir, "paper_outputs_final_one_se")

step_1_to_3 <- c("Step_1_pq16_score", "Step_2_pq_distress", "Step_3_dsst", "Step_4_demographic_clinical")
step_4_to_5 <- c("Step_5_full_model")

model_labels <- c(
  Step_1_pq16_score = "PQ-16 Score",
  Step_2_pq_distress = "PQ-16 Distress",
  Step_3_dsst = "DSST",
  Step_4_demographic_clinical = "PPS",
  Step_5_full_model = "All"
)

feature_labels <- c(
  pq16_score = "PQ-16 Score",
  pq_distress = "PQ-16 Distress",
  dsst = "DSST",
  high_urbanicity = "High urbanicity",
  parent_smi = "Parent SMI",
  genderOther = "Other gender",
  ethnicityOther = "Other ethnicity",
  first_gen_migrant = "First-generation migrant",
  paternal_age_35 = "Paternal age >=35",
  genderMale = "Male gender",
  ethnic_densityMedium = "Medium ethnic density",
  second_gen_migrant = "Second-generation migrant",
  `genderNon-binary` = "Non-binary gender",
  ethnicityMixed = "Mixed ethnicity",
  ethnic_densityLow = "Low ethnic density",
  adult_life_events = "Adult life events",
  family_history = "Family history",
  ethnicityBlack = "Black ethnicity",
  age = "Age",
  pollution = "Pollution",
  ethnicityWhite = "White ethnicity",
  paternal_ses = "Paternal SES",
  paternal_age_45 = "Paternal age >=45",
  anhedonia = "Anhedonia",
  north_african_second_gen_migrant = "North African second-generation migrant",
  north_african_first_gen_migrant = "North African first-generation migrant",
  genderFemale = "Female gender"
)

read_harmonised <- function(dir, steps) {
  read_csv(file.path(dir, "harmonised_model_summary_table.csv"), show_col_types = FALSE) %>%
    filter(base_model %in% steps)
}

fmt <- function(value, low, high, digits = 2) {
  clean <- function(x) {
    x <- ifelse(!is.na(x) & abs(x) < 0.5 * 10^-digits, 0, x)
    formatC(x, format = "f", digits = digits)
  }

  ifelse(
    is.na(value) | is.na(low) | is.na(high),
    "NA (NA - NA)",
    paste0(
      clean(value), " (",
      clean(low), " - ",
      clean(high), ")"
    )
  )
}

combined <- bind_rows(
  read_harmonised(old_output_dir, step_1_to_3),
  read_harmonised(elastic_net_output_dir, step_4_to_5)
) %>%
  mutate(
    Model = unname(model_labels[base_model]),
    Feature = recode(Feature, !!!feature_labels, .default = Feature),
    model_order = match(base_model, names(model_labels)),
    feature_order = row_number()
  )

clean_table <- combined %>%
  transmute(
    Model,
    Feature,
    `Elastic net coefficient` = fmt(glmnet_pooled_coefficient, glmnet_ci_low, glmnet_ci_high),
    `Random forest importance` = fmt(rf_pooled_importance, rf_ci_low, rf_ci_high),
    `XGBoost gain` = fmt(xgb_gain_pooled_importance, xgb_gain_ci_low, xgb_gain_ci_high),
    `XGBoost cover` = fmt(xgb_cover_pooled_importance, xgb_cover_ci_low, xgb_cover_ci_high),
    `XGBoost frequency` = fmt(xgb_frequency_pooled_importance, xgb_frequency_ci_low, xgb_frequency_ci_high),
    model_order,
    feature_order
  ) %>%
  arrange(model_order, feature_order) %>%
  select(-model_order, -feature_order)

compact_table <- clean_table %>%
  select(Model, Feature, `Elastic net coefficient`, `Random forest importance`, `XGBoost gain`)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(clean_table, file.path(output_dir, "etable5_coefficient_variable_importance.csv"))
write_csv(compact_table, file.path(output_dir, "etable5_coefficient_variable_importance_compact.csv"))

message("Saved coefficient and variable-importance tables to ", output_dir)
