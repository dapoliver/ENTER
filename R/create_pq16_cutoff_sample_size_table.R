library(dplyr)
library(readr)
library(pmsampsize)

analyses_dir <- "/Users/domoliver/Library/CloudStorage/Dropbox/Work/ENTER/CAARMS Prediction/Analyses"
input_dir <- file.path(analyses_dir, "pq16_cutoff_unbalanced_one_se_regularised_trees")
output_dir <- file.path(analyses_dir, "paper_outputs_final_one_se")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

extract_mean <- function(x) as.numeric(sub(" .*", "", x))

run_pmsampsize <- function(cstatistic, prevalence) {
  fit <- pmsampsize(
    type = "b",
    cstatistic = cstatistic,
    parameters = 1,
    prevalence = prevalence,
    shrinkage = 0.9
  )

  tibble(
    nagelkerke_r2 = fit$nag_r2,
    shrinkage = fit$final_shrinkage,
    criterion_1_n = fit$results_table["Criteria 1", "Samp_size"],
    criterion_2_n = fit$results_table["Criteria 2", "Samp_size"],
    criterion_3_n = fit$results_table["Criteria 3", "Samp_size"],
    required_n = fit$sample_size,
    required_events = fit$events
  )
}

results <- read_csv(file.path(input_dir, "results_summary.csv"), show_col_types = FALSE) %>%
  filter(grepl("^Step_1b_pq16_cutoff__final_", model)) %>%
  transmute(
    learner = sub("^.*__final_", "", model),
    c_statistic = extract_mean(C)
  )

predictions <- read_csv(file.path(input_dir, "test_set_predictions.csv"), show_col_types = FALSE) %>%
  filter(grepl("^Step_1b_pq16_cutoff__final_", model)) %>%
  distinct(Subject_ID, obs)

diagnostics <- read_csv(file.path(input_dir, "ssl_training_diagnostics.csv"), show_col_types = FALSE) %>%
  filter(model == "Step_1b_pq16_cutoff")

labelled_n <- nrow(predictions)
labelled_events <- sum(predictions$obs == 1, na.rm = TRUE)
mean_test_n <- labelled_n - mean(diagnostics$n_labelled_train)
mean_test_events <- labelled_events - mean(diagnostics$events_labelled_train)
observed_n <- mean(diagnostics$n_final_train) + mean_test_n
observed_events <- mean(diagnostics$events_final_train) + mean_test_events
observed_nonevents <- observed_n - observed_events
prevalence <- observed_events / observed_n

compact_table <- results %>%
  rowwise() %>%
  mutate(pmsampsize = list(run_pmsampsize(c_statistic, prevalence))) %>%
  tidyr::unnest(pmsampsize) %>%
  ungroup() %>%
  mutate(
    Model = recode(learner, glmnet = "Elastic net", rf = "Random forest", xgb = "XGBoost"),
    `Observed N` = as.integer(round(observed_n)),
    `Observed events` = as.integer(round(observed_events)),
    `Event rate` = formatC(prevalence, format = "f", digits = 2),
    `Predictors, n` = 1L,
    `Nagelkerke R2` = formatC(nagelkerke_r2, format = "f", digits = 2),
    Shrinkage = formatC(shrinkage, format = "f", digits = 2),
    `Criterion 1 n` = as.integer(ceiling(criterion_1_n)),
    `Criterion 2 n` = as.integer(ceiling(criterion_2_n)),
    `Criterion 3 n` = as.integer(ceiling(criterion_3_n)),
    `Required n` = as.integer(ceiling(required_n)),
    `Required events` = as.integer(ceiling(required_events)),
    `Required non-events` = as.integer(ceiling(required_n - required_events)),
    `n minus required n` = as.integer(floor(observed_n - required_n)),
    `events minus required events` = as.integer(floor(observed_events - required_events)),
    `non-events minus required non-events` = as.integer(floor(observed_nonevents - (required_n - required_events))),
    `Meets requirements` = if_else(
      observed_n >= required_n & observed_events >= required_events & observed_nonevents >= (required_n - required_events),
      "Yes", "No"
    )
  ) %>%
  select(
    Model, `Observed N`, `Observed events`, `Event rate`, `Predictors, n`,
    `Nagelkerke R2`, Shrinkage, `Criterion 1 n`, `Criterion 2 n`, `Criterion 3 n`,
    `Required n`, `Required events`, `Required non-events`,
    `n minus required n`, `events minus required events`,
    `non-events minus required non-events`, `Meets requirements`
  )

write_csv(compact_table, file.path(output_dir, "etable3_pq16_cutoff_sample_size_compact.csv"))
message("Saved PQ-16 cut-off compact sample-size table to ", output_dir)
