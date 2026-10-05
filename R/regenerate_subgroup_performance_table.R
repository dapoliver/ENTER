library(dplyr)
library(readr)

input_dir <- "/Users/domoliver/Library/CloudStorage/Dropbox/Work/ENTER/CAARMS Prediction/Analyses/step5_unbalanced_one_se_regularised_trees"
output_dir <- "/Users/domoliver/Library/CloudStorage/Dropbox/Work/ENTER/CAARMS Prediction/Analyses/paper_outputs_final_one_se"

format_percent <- function(x, digits = 2) {
  ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = digits))
}

normalise_interval_text <- function(x) {
  x <- gsub("%", "", x, fixed = TRUE)
  x <- gsub("<NA>", "NA", x, fixed = TRUE)
  x
}

learner_label <- function(x) {
  case_when(
    grepl("__final_glmnet$", x) ~ "Elastic net",
    grepl("__final_rf$", x) ~ "Random forest",
    grepl("__final_xgb$", x) ~ "XGBoost",
    TRUE ~ x
  )
}

subgroup_table <- bind_rows(
  read_csv(file.path(input_dir, "subgroup_performance_gender.csv"), show_col_types = FALSE),
  read_csv(file.path(input_dir, "subgroup_performance_ethnicity.csv"), show_col_types = FALSE),
  read_csv(file.path(input_dir, "subgroup_performance_age.csv"), show_col_types = FALSE),
  read_csv(file.path(input_dir, "subgroup_performance_country.csv"), show_col_types = FALSE)
) %>%
  filter(!is.na(subgroup), subgroup != "<NA>") %>%
  mutate(
    subgroup_type_order = case_when(
      subgroup_type == "gender" ~ 1,
      subgroup_type == "age_group" ~ 2,
      subgroup_type == "country" ~ 3,
      subgroup_type == "ethnicity" ~ 4,
      TRUE ~ 99
    ),
    learner_order = case_when(
      grepl("__final_glmnet$", model) ~ 1,
      grepl("__final_rf$", model) ~ 2,
      grepl("__final_xgb$", model) ~ 3,
      TRUE ~ 99
    ),
    subgroup_order = case_when(
      subgroup_type == "gender" & subgroup == "Female" ~ 1,
      subgroup_type == "gender" & subgroup == "Male" ~ 2,
      subgroup_type == "gender" & subgroup == "Non-binary" ~ 3,
      subgroup_type == "age_group" & subgroup == "Under 18" ~ 1,
      subgroup_type == "age_group" & subgroup == "18 and over" ~ 2,
      subgroup_type == "country" & subgroup == "Italy (PV)" ~ 1,
      subgroup_type == "country" & subgroup == "UK" ~ 2,
      subgroup_type == "ethnicity" & subgroup == "Asian" ~ 1,
      subgroup_type == "ethnicity" & subgroup == "Black" ~ 2,
      subgroup_type == "ethnicity" & subgroup == "Mixed" ~ 3,
      subgroup_type == "ethnicity" & subgroup == "Other" ~ 4,
      subgroup_type == "ethnicity" & subgroup == "White" ~ 5,
      TRUE ~ 99
    )
  ) %>%
  arrange(subgroup_type_order, learner_order, subgroup_order) %>%
  transmute(
    Model = learner_label(model),
    Subgroup = subgroup,
    n = n,
    `Event rate` = format_percent(event_rate, 2),
    `Sparse?` = ifelse(sparse_stratum, "Yes", "No"),
    `C-index` = C,
    `Balanced accuracy` = normalise_interval_text(balanced_accuracy),
    `Brier score` = brier,
    `Calibration intercept` = calibration_intercept,
    `Calibration slope` = calibration_slope
  )

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(subgroup_table, file.path(output_dir, "etable6_subgroup_performance.csv"))

message("Saved updated subgroup table to ", file.path(output_dir, "etable6_subgroup_performance.csv"))
