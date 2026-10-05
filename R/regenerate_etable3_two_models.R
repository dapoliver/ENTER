library(dplyr)
library(readr)
library(tidyr)
library(pmsampsize)

analyses_dir <- "/Users/domoliver/Library/CloudStorage/Dropbox/Work/ENTER/CAARMS Prediction/Analyses"
output_dir <- file.path(analyses_dir, "paper_outputs_final_one_se")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

model_sources <- tibble(
  directory = c(
    "pq16_cutoff_unbalanced_one_se_regularised_trees",
    "step5_original_predictors_unbalanced_one_se_regularised_trees"
  ),
  base_model = c("Step_1b_pq16_cutoff", "Step_5_full_model"),
  Model = c("PQ-16 cut-off", "ENTER multimodal screener"),
  model_order = c(1L, 2L),
  provisional = c(TRUE, FALSE)
)

extract_mean <- function(x) as.numeric(sub(" .*", "", x))

run_pmsampsize <- function(cstatistic, parameters, prevalence) {
  fit <- pmsampsize(
    type = "b",
    cstatistic = cstatistic,
    parameters = parameters,
    prevalence = prevalence,
    shrinkage = 0.9
  )

  tibble(
    cox_snell_r2 = fit$rsquared,
    nagelkerke_r2 = fit$nag_r2,
    required_n = fit$sample_size,
    required_events = fit$events,
    required_epp = fit$EPP,
    final_shrinkage = fit$final_shrinkage,
    criterion_1_n = fit$results_table["Criteria 1", "Samp_size"],
    criterion_2_n = fit$results_table["Criteria 2", "Samp_size"],
    criterion_3_n = fit$results_table["Criteria 3", "Samp_size"]
  )
}

read_model_data <- function(directory, base_model, label, model_order, provisional) {
  input_dir <- file.path(analyses_dir, directory)

  results <- read_csv(file.path(input_dir, "results_summary.csv"), show_col_types = FALSE) %>%
    filter(grepl(paste0("^", base_model, "__final_"), model)) %>%
    transmute(
      final_learner = sub("^.*__final_", "", model),
      c_statistic = extract_mean(C)
    )

  predictions <- read_csv(file.path(input_dir, "test_set_predictions.csv"), show_col_types = FALSE) %>%
    filter(grepl(paste0("^", base_model, "__final_"), model)) %>%
    distinct(Subject_ID, obs)

  diagnostics <- read_csv(file.path(input_dir, "ssl_training_diagnostics.csv"), show_col_types = FALSE) %>%
    filter(model == base_model)

  coefficients <- read_csv(file.path(input_dir, "pooled_implementation_coefficients.csv"), show_col_types = FALSE) %>%
    filter(model == paste0(base_model, "__final_glmnet"), Feature != "(Intercept)")

  labelled_n <- nrow(predictions)
  labelled_events <- sum(predictions$obs == 1, na.rm = TRUE)
  mean_test_n <- labelled_n - mean(diagnostics$n_labelled_train, na.rm = TRUE)
  mean_test_events <- labelled_events - mean(diagnostics$events_labelled_train, na.rm = TRUE)
  mean_ssl_train_n <- mean(diagnostics$n_final_train, na.rm = TRUE)
  mean_ssl_train_events <- mean(diagnostics$events_final_train, na.rm = TRUE)
  observed_n <- mean_ssl_train_n + mean_test_n
  observed_events <- mean_ssl_train_events + mean_test_events

  results %>%
    mutate(
      base_model = base_model,
      Model = label,
      model_order = model_order,
      provisional = provisional,
      labelled_n = labelled_n,
      labelled_events = labelled_events,
      mean_labelled_test_n = mean_test_n,
      mean_labelled_test_events = mean_test_events,
      mean_ssl_train_n = mean_ssl_train_n,
      mean_ssl_train_events = mean_ssl_train_events,
      mean_pseudolabelled_train_n = mean(diagnostics$n_pseudolabelled_train, na.rm = TRUE),
      mean_pseudolabelled_train_events = mean(diagnostics$events_pseudolabelled_train, na.rm = TRUE),
      observed_n = observed_n,
      observed_events = observed_events,
      observed_nonevents = observed_n - observed_events,
      prevalence = observed_events / observed_n,
      candidate_parameters = n_distinct(coefficients$Feature)
    )
}

sample_size_table <- model_sources %>%
  rowwise() %>%
  do(read_model_data(.$directory, .$base_model, .$Model, .$model_order, .$provisional)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(pmsampsize = list(run_pmsampsize(c_statistic, candidate_parameters, prevalence))) %>%
  unnest(pmsampsize) %>%
  ungroup() %>%
  mutate(
    Learner = recode(final_learner, glmnet = "Elastic net", rf = "Random forest", xgb = "XGBoost"),
    required_nonevents = required_n - required_events,
    observed_n_minus_required_n = observed_n - required_n,
    observed_events_minus_required_events = observed_events - required_events,
    observed_nonevents_minus_required_nonevents = observed_nonevents - required_nonevents,
    meets_requirements = observed_n >= required_n &
      observed_events >= required_events &
      observed_nonevents >= required_nonevents
  ) %>%
  arrange(model_order, final_learner)

compact_table <- sample_size_table %>%
  transmute(
    Model,
    Learner,
    `Observed N` = as.integer(round(observed_n)),
    `Observed events` = as.integer(round(observed_events)),
    `Event rate` = formatC(prevalence, format = "f", digits = 2),
    `Predictors, n` = candidate_parameters,
    `Nagelkerke R2` = formatC(nagelkerke_r2, format = "f", digits = 2),
    Shrinkage = formatC(final_shrinkage, format = "f", digits = 2),
    `Criterion 1 n` = as.integer(ceiling(criterion_1_n)),
    `Criterion 2 n` = as.integer(ceiling(criterion_2_n)),
    `Criterion 3 n` = as.integer(ceiling(criterion_3_n)),
    `Required n` = as.integer(ceiling(required_n)),
    `Required events` = as.integer(ceiling(required_events)),
    `Required non-events` = as.integer(ceiling(required_nonevents)),
    `n minus required n` = as.integer(floor(observed_n_minus_required_n)),
    `events minus required events` = as.integer(floor(observed_events_minus_required_events)),
    `non-events minus required non-events` = as.integer(floor(observed_nonevents_minus_required_nonevents)),
    `Meets requirements` = if_else(meets_requirements, "Yes", "No"),
    `Provisional?` = if_else(provisional, "Yes", "No")
  )

full_table <- sample_size_table %>%
  select(
    Model, Learner, labelled_n, labelled_events,
    mean_ssl_train_n, mean_ssl_train_events,
    mean_labelled_test_n, mean_labelled_test_events,
    mean_pseudolabelled_train_n, mean_pseudolabelled_train_events,
    observed_n, observed_events, observed_nonevents, prevalence,
    candidate_parameters, c_statistic, cox_snell_r2, nagelkerke_r2,
    required_n, required_events, required_nonevents, required_epp,
    criterion_1_n, criterion_2_n, criterion_3_n,
    observed_n_minus_required_n, observed_events_minus_required_events,
    observed_nonevents_minus_required_nonevents, meets_requirements, provisional
  )

write_csv(full_table, file.path(output_dir, "pmsampsize_main_models.csv"))
write_csv(compact_table, file.path(output_dir, "pmsampsize_compact_table.csv"))
write_csv(full_table, file.path(output_dir, "etable3_sample_size_calculations.csv"))
write_csv(compact_table, file.path(output_dir, "etable3_sample_size_calculations_compact.csv"))

markdown_lines <- c(
  "| Model | Learner | Observed N | Observed events | Event rate | Predictors, n | Required N | Required events | Required non-events | Meets requirements | Provisional? |",
  "|---|---|---:|---:|---:|---:|---:|---:|---:|---|---|",
  apply(compact_table[, c("Model", "Learner", "Observed N", "Observed events", "Event rate", "Predictors, n", "Required n", "Required events", "Required non-events", "Meets requirements", "Provisional?")], 1, function(row) paste0("| ", paste(row, collapse = " | "), " |")),
  "",
  "Note. Calculations used pmsampsize for a binary outcome with target shrinkage 0.90, the mean SSL-augmented training sample plus the labelled held-out test fold, and the cross-validated C-statistic. The ENTER multimodal screener used 26 encoded predictor parameters. PQ-16 cut-off estimates are provisional because its current SSL implementation selects all records tied at either risk value; they must be regenerated after ranked-tail tie handling is implemented."
)
writeLines(markdown_lines, file.path(output_dir, "pmsampsize_supplement_table.md"))

message("Saved rebuilt two-model eTable 3 outputs to ", output_dir)
