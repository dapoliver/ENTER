library(dplyr)
library(readr)

analyses_dir <- "/Users/domoliver/Library/CloudStorage/Dropbox/Work/ENTER/CAARMS Prediction/Analyses"
output_dir <- file.path(analyses_dir, "paper_outputs_final_one_se")

model_sources <- tibble(
  directory = c(
    "pq16_cutoff_unbalanced_one_se_regularised_trees",
    "step5_original_predictors_unbalanced_one_se_regularised_trees"
  ),
  base_model = c("Step_1b_pq16_cutoff", "Step_5_full_model"),
  Model = c("PQ-16 cut-off", "ENTER multimodal screener"),
  model_order = c(1L, 2L)
)

feature_labels <- c(
  pq_cutoff = "PQ-16 cut-off",
  pq16_score = "PQ-16 Score",
  pq_distress = "PQ-16 Distress",
  dsst = "DSST",
  parent_smi = "Parent severe mental illness",
  genderOther = "Other gender",
  ethnicityOther = "Other ethnicity",
  first_gen_migrant = "First-generation migration",
  paternal_age_35 = "Paternal age >=35 years",
  genderMale = "Male gender",
  ethnic_densityMedium = "Medium ethnic density",
  second_gen_migrant = "Second-generation migration",
  `genderNon-binary` = "Non-binary gender",
  ethnicityMixed = "Mixed ethnicity",
  ethnic_densityLow = "Low ethnic density",
  adult_life_events = "Adult life events",
  family_history = "Family history of psychosis",
  ethnicityBlack = "Black ethnicity",
  age = "Age",
  pollution = "Pollution",
  ethnicityWhite = "White ethnicity",
  paternal_ses = "Paternal socioeconomic status",
  paternal_age_45 = "Paternal age >=45 years",
  anhedonia = "Trait anhedonia",
  north_african_second_gen_migrant = "North African second-generation migration",
  north_african_first_gen_migrant = "North African first-generation migration",
  genderFemale = "Female gender"
)

required_columns <- c(
  "glmnet_pooled_coefficient", "glmnet_ci_low", "glmnet_ci_high",
  "rf_pooled_importance", "rf_ci_low", "rf_ci_high",
  "xgb_gain_pooled_importance", "xgb_gain_ci_low", "xgb_gain_ci_high",
  "xgb_cover_pooled_importance", "xgb_cover_ci_low", "xgb_cover_ci_high",
  "xgb_frequency_pooled_importance", "xgb_frequency_ci_low", "xgb_frequency_ci_high"
)

fmt <- function(value, low, high, digits = 2) {
  clean <- function(x) {
    x <- ifelse(!is.na(x) & abs(x) < 0.5 * 10^-digits, 0, x)
    formatC(x, format = "f", digits = digits)
  }

  ifelse(
    is.na(value) | is.na(low) | is.na(high),
    "NA (NA - NA)",
    paste0(clean(value), " (", clean(low), " - ", clean(high), ")")
  )
}

combined <- model_sources %>%
  rowwise() %>%
  do({
    source <- .
    table <- read_csv(
      file.path(analyses_dir, source$directory, "harmonised_model_summary_table.csv"),
      show_col_types = FALSE
    )
    missing_columns <- setdiff(required_columns, names(table))
    table[missing_columns] <- NA_real_
    table %>%
      filter(base_model == source$base_model) %>%
      mutate(Model = source$Model, model_order = source$model_order)
  }) %>%
  ungroup() %>%
  mutate(
    Feature = recode(Feature, !!!feature_labels, .default = Feature),
    feature_order = row_number()
  )

full_table <- combined %>%
  transmute(
    Model,
    Feature,
    `Elastic net coefficient` = fmt(glmnet_pooled_coefficient, glmnet_ci_low, glmnet_ci_high, digits = 3),
    `Random forest importance` = fmt(rf_pooled_importance, rf_ci_low, rf_ci_high, digits = 3),
    `XGBoost gain` = fmt(xgb_gain_pooled_importance, xgb_gain_ci_low, xgb_gain_ci_high, digits = 3),
    `XGBoost cover` = fmt(xgb_cover_pooled_importance, xgb_cover_ci_low, xgb_cover_ci_high, digits = 3),
    `XGBoost frequency` = fmt(xgb_frequency_pooled_importance, xgb_frequency_ci_low, xgb_frequency_ci_high, digits = 3),
    model_order,
    feature_order
  ) %>%
  arrange(model_order, feature_order) %>%
  select(-model_order, -feature_order)

compact_table <- full_table %>%
  select(Model, Feature, `Elastic net coefficient`, `Random forest importance`, `XGBoost gain`)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(full_table, file.path(output_dir, "etable5_coefficient_variable_importance.csv"))
write_csv(compact_table, file.path(output_dir, "etable5_coefficient_variable_importance_compact.csv"))

message("Saved two-model eTable 5 to ", output_dir)
