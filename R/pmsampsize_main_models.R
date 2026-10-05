library(dplyr)
library(readr)
library(pmsampsize)

analyses_dir <- "/Users/domoliver/Library/CloudStorage/Dropbox/Work/ENTER/CAARMS Prediction/Analyses"
old_output_dir <- file.path(analyses_dir, "revised_enter_unimodal_unbalanced_one_se_regularised_trees_steps1to4")
elastic_net_output_dir <- file.path(analyses_dir, "revised_enter_step5_unbalanced_one_se_regularised_trees")
output_dir <- file.path(analyses_dir, "paper_outputs_final_one_se")

step_1_to_3 <- c("Step_1_pq16_score", "Step_2_pq_distress", "Step_3_dsst", "Step_4_demographic_clinical")
step_4_to_5 <- c("Step_5_full_model")
main_steps <- c(step_1_to_3, step_4_to_5)

extract_mean <- function(x) {
  as.numeric(sub(" .*", "", x))
}

extract_ci_low <- function(x) {
  as.numeric(sub("^.*\\(([-0-9.]+)-[-0-9.]+\\)$", "\\1", x))
}

extract_ci_high <- function(x) {
  as.numeric(sub("^.*\\([-0-9.]+-([-0-9.]+)\\)$", "\\1", x))
}

format_number <- function(x, digits = 2) {
  ifelse(is.na(x), NA_character_, formatC(x, format = "f", digits = digits))
}

format_percent <- function(x, digits = 1) {
  ifelse(is.na(x), NA_character_, paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}

strip_step <- function(model) sub("__final_.*$", "", model)

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

read_main_step_outputs <- function(filename) {
  old_data <- read_csv(file.path(old_output_dir, filename), show_col_types = FALSE)
  elastic_net_data <- read_csv(file.path(elastic_net_output_dir, filename), show_col_types = FALSE)

  bind_rows(
    old_data %>%
      mutate(base_model = strip_step(model)) %>%
      filter(base_model %in% step_1_to_3),
    elastic_net_data %>%
      mutate(base_model = strip_step(model)) %>%
      filter(base_model %in% step_4_to_5)
  )
}

results <- read_main_step_outputs("results_summary.csv") %>%
  mutate(
    base_model = strip_step(model),
    final_learner = sub("^.*__final_", "", model),
    c_statistic = extract_mean(C),
    c_statistic_ci_low = extract_ci_low(C),
    c_statistic_ci_high = extract_ci_high(C)
  )

predictions <- read_main_step_outputs("test_set_predictions.csv") %>%
  mutate(base_model = strip_step(model)) %>%
  distinct(base_model, Subject_ID, obs)

labelled_summary <- predictions %>%
  group_by(base_model) %>%
  summarise(
    labelled_n = n(),
    labelled_events = sum(obs == 1, na.rm = TRUE),
    labelled_nonevents = sum(obs == 0, na.rm = TRUE),
    labelled_prevalence = mean(obs == 1, na.rm = TRUE),
    .groups = "drop"
  )

ssl_diagnostics <- read_main_step_outputs("ssl_training_diagnostics.csv")

sample_summary <- ssl_diagnostics %>%
  inner_join(labelled_summary, by = c("model" = "base_model")) %>%
  mutate(
    labelled_test_n = labelled_n - n_labelled_train,
    labelled_test_events = labelled_events - events_labelled_train,
    labelled_test_nonevents = labelled_test_n - labelled_test_events,
    observed_n = n_final_train + labelled_test_n,
    observed_events = events_final_train + labelled_test_events,
    observed_nonevents = observed_n - observed_events,
    prevalence = observed_events / observed_n
  ) %>%
  group_by(base_model = model) %>%
  summarise(
    labelled_n = first(labelled_n),
    labelled_events = first(labelled_events),
    labelled_nonevents = first(labelled_nonevents),
    labelled_prevalence = first(labelled_prevalence),
    mean_labelled_train_n = mean(n_labelled_train, na.rm = TRUE),
    mean_labelled_train_events = mean(events_labelled_train, na.rm = TRUE),
    mean_labelled_test_n = mean(labelled_test_n, na.rm = TRUE),
    mean_labelled_test_events = mean(labelled_test_events, na.rm = TRUE),
    mean_ssl_train_n = mean(n_final_train, na.rm = TRUE),
    mean_ssl_train_events = mean(events_final_train, na.rm = TRUE),
    mean_pseudolabelled_train_n = mean(n_pseudolabelled_train, na.rm = TRUE),
    mean_pseudolabelled_train_events = mean(events_pseudolabelled_train, na.rm = TRUE),
    observed_n = mean(observed_n, na.rm = TRUE),
    observed_events = mean(observed_events, na.rm = TRUE),
    observed_nonevents = mean(observed_nonevents, na.rm = TRUE),
    prevalence = mean(observed_events, na.rm = TRUE) / mean(observed_n, na.rm = TRUE),
    mean_split_prevalence = mean(prevalence, na.rm = TRUE),
    min_observed_n = min(observed_n, na.rm = TRUE),
    max_observed_n = max(observed_n, na.rm = TRUE),
    min_prevalence = min(prevalence, na.rm = TRUE),
    max_prevalence = max(prevalence, na.rm = TRUE),
    n_ssl_splits = n(),
    .groups = "drop"
  )

coefficients <- read_main_step_outputs("pooled_implementation_coefficients.csv") %>%
  mutate(base_model = strip_step(model))

parameter_counts <- coefficients %>%
  filter(Feature != "(Intercept)") %>%
  group_by(base_model) %>%
  summarise(candidate_parameters = n_distinct(Feature), .groups = "drop")

base_model_labels <- c(
  Step_1_pq16_score = "PQ-16 score",
  Step_2_pq_distress = "PQ-16 distress",
  Step_3_dsst = "DSST",
  Step_4_demographic_clinical = "PPS",
  Step_5_full_model = "Full model"
)

learner_labels <- c(
  glmnet = "Elastic net logistic regression",
  rf = "Random forest",
  xgb = "Extreme gradient boosting"
)

sample_size_table <- results %>%
  inner_join(sample_summary, by = "base_model") %>%
  inner_join(parameter_counts, by = "base_model") %>%
  rowwise() %>%
  mutate(pmsampsize = list(run_pmsampsize(
    cstatistic = c_statistic,
    parameters = candidate_parameters,
    prevalence = prevalence
  ))) %>%
  tidyr::unnest(pmsampsize) %>%
  ungroup() %>%
  mutate(
    base_model_label = recode(base_model, !!!base_model_labels),
    final_learner_label = recode(final_learner, !!!learner_labels),
    required_nonevents = required_n - required_events,
    observed_n_minus_required_n = observed_n - required_n,
    observed_events_minus_required_events = observed_events - required_events,
    observed_nonevents_minus_required_nonevents = observed_nonevents - required_nonevents,
    observed_meets_required_n = observed_n >= required_n,
    observed_meets_required_events = observed_events >= required_events,
    observed_meets_required_nonevents = observed_nonevents >= required_nonevents,
    events_per_candidate_parameter = observed_events / candidate_parameters
  ) %>%
  select(
    base_model,
    base_model_label,
    final_learner,
    final_learner_label,
    labelled_n,
    labelled_events,
    labelled_nonevents,
    labelled_prevalence,
    mean_labelled_test_n,
    mean_labelled_test_events,
    mean_ssl_train_n,
    mean_ssl_train_events,
    mean_pseudolabelled_train_n,
    mean_pseudolabelled_train_events,
    observed_n,
    observed_events,
    observed_nonevents,
    prevalence,
    mean_split_prevalence,
    min_observed_n,
    max_observed_n,
    min_prevalence,
    max_prevalence,
    n_ssl_splits,
    candidate_parameters,
    events_per_candidate_parameter,
    c_statistic,
    c_statistic_ci_low,
    c_statistic_ci_high,
    cox_snell_r2,
    nagelkerke_r2,
    required_n,
    required_events,
    required_nonevents,
    required_epp,
    final_shrinkage,
    criterion_1_n,
    criterion_2_n,
    criterion_3_n,
    observed_n_minus_required_n,
    observed_events_minus_required_events,
    observed_nonevents_minus_required_nonevents,
    observed_meets_required_n,
    observed_meets_required_events,
    observed_meets_required_nonevents
  )

write_csv(sample_size_table, file.path(output_dir, "pmsampsize_main_models.csv"))

supplement_table <- sample_size_table %>%
  transmute(
    `Model` = base_model_label,
    `Learner` = final_learner_label,
    `Labelled N` = labelled_n,
    `Labelled events` = labelled_events,
    `Mean SSL training N` = format_number(mean_ssl_train_n, 1),
    `Mean SSL training events` = format_number(mean_ssl_train_events, 1),
    `Mean labelled test N` = format_number(mean_labelled_test_n, 1),
    `Mean labelled test events` = format_number(mean_labelled_test_events, 1),
    `Mean pseudo-labelled N` = format_number(mean_pseudolabelled_train_n, 1),
    `Mean pseudo-labelled events` = format_number(mean_pseudolabelled_train_events, 1),
    `Mean SSL + test N` = format_number(observed_n, 1),
    `Mean SSL + test events` = format_number(observed_events, 1),
    `Mean SSL + test non-events` = format_number(observed_nonevents, 1),
    `Outcome prevalence` = format_percent(prevalence),
    `Candidate parameters` = candidate_parameters,
    `Events per parameter` = format_number(events_per_candidate_parameter, 1),
    `C-statistic input` = paste0(
      format_number(c_statistic, 3),
      " (",
      format_number(c_statistic_ci_low, 3),
      "-",
      format_number(c_statistic_ci_high, 3),
      ")"
    ),
    `Cox-Snell R2` = format_number(cox_snell_r2, 4),
    `Nagelkerke R2` = format_number(nagelkerke_r2, 3),
    `Required N` = as.integer(ceiling(required_n)),
    `Required events` = as.integer(ceiling(required_events)),
    `Required non-events` = as.integer(ceiling(required_nonevents)),
    `Required EPP` = format_number(required_epp, 1),
    `Criterion 1 N` = as.integer(ceiling(criterion_1_n)),
    `Criterion 2 N` = as.integer(ceiling(criterion_2_n)),
    `Criterion 3 N` = as.integer(ceiling(criterion_3_n)),
    `Observed N - required N` = as.integer(floor(observed_n_minus_required_n)),
    `Observed events - required events` = as.integer(floor(observed_events_minus_required_events)),
    `Observed non-events - required non-events` = as.integer(floor(observed_nonevents_minus_required_nonevents)),
    `Meets required N` = ifelse(observed_meets_required_n, "Yes", "No"),
    `Meets required events` = ifelse(observed_meets_required_events, "Yes", "No"),
    `Meets required non-events` = ifelse(observed_meets_required_nonevents, "Yes", "No")
  )

compact_table <- sample_size_table %>%
  transmute(
    Predictors = base_model_label,
    Model = case_when(
      final_learner == "glmnet" ~ "Elastic net",
      final_learner == "rf" ~ "Random forest",
      final_learner == "xgb" ~ "XGBoost",
      TRUE ~ final_learner_label
    ),
    N = as.integer(round(observed_n)),
    `Event rate` = format_number(prevalence, 2),
    `Predictors, n` = candidate_parameters,
    `Nagelkerke R2` = format_number(nagelkerke_r2, 2),
    Shrinkage = format_number(final_shrinkage, 2),
    `Criterion 1 n` = as.integer(ceiling(criterion_1_n)),
    `Criterion 2 n` = as.integer(ceiling(criterion_2_n)),
    `Criterion 3 n` = as.integer(ceiling(criterion_3_n)),
    `n minus required n` = as.integer(floor(observed_n_minus_required_n)),
    `events minus required events` = as.integer(floor(observed_events_minus_required_events)),
    `non-events minus required non-events` = as.integer(floor(observed_nonevents_minus_required_nonevents))
  )

markdown_lines <- c(
  "| Model | Learner | Labelled N | Labelled events | Mean SSL training N | Mean SSL training events | Mean labelled test N | Mean labelled test events | Mean pseudo-labelled N | Mean pseudo-labelled events | Mean SSL + test N | Mean SSL + test events | Mean SSL + test non-events | Outcome prevalence | Candidate parameters | Events per parameter | C-statistic input | Cox-Snell R2 | Nagelkerke R2 | Required N | Required events | Required non-events | Required EPP | Criterion 1 N | Criterion 2 N | Criterion 3 N | Observed N - required N | Observed events - required events | Observed non-events - required non-events | Meets required N | Meets required events | Meets required non-events |",
  "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|",
  apply(supplement_table, 1, function(row) {
    paste0("| ", paste(row, collapse = " | "), " |")
  }),
  "",
  "Note. Sample size calculations used pmsampsize for a binary outcome with target shrinkage 0.90, the SSL-augmented prevalence, and the bootstrap mean cross-validated C-statistic from the main analyses as the anticipated C-statistic. The SSL-augmented sample size was calculated within each outer repeat/fold as the final semi-supervised training set plus the labelled held-out test fold, then averaged across 50 splits per base model. This is equivalent to the unique labelled sample plus the mean pseudo-labelled training sample for that base model. Candidate parameter counts exclude the intercept and include dummy variables for categorical predictors. Criterion 1 targets shrinkage, criterion 2 targets the optimism in Nagelkerke R2, and criterion 3 targets precision in the overall outcome proportion."
)

writeLines(markdown_lines, file.path(output_dir, "pmsampsize_supplement_table.md"))
write_csv(compact_table, file.path(output_dir, "pmsampsize_compact_table.csv"))

range_required <- range(sample_size_table$required_n, na.rm = TRUE)
range_required_events <- range(sample_size_table$required_events, na.rm = TRUE)
range_required_nonevents <- range(sample_size_table$required_nonevents, na.rm = TRUE)
range_observed <- range(sample_size_table$observed_n, na.rm = TRUE)
range_observed_events <- range(sample_size_table$observed_events, na.rm = TRUE)
range_prevalence <- range(sample_size_table$prevalence, na.rm = TRUE)
multivariable <- sample_size_table %>%
  filter(base_model %in% c("Step_4_demographic_clinical", "Step_5_full_model"))
range_multivariable_required <- range(multivariable$required_n, na.rm = TRUE)
range_multivariable_events <- range(multivariable$required_events, na.rm = TRUE)
range_multivariable_nonevents <- range(multivariable$required_nonevents, na.rm = TRUE)
range_multivariable_observed <- range(multivariable$observed_n, na.rm = TRUE)
range_multivariable_observed_events <- range(multivariable$observed_events, na.rm = TRUE)
range_multivariable_prevalence <- range(multivariable$prevalence, na.rm = TRUE)
met_n <- sum(sample_size_table$observed_meets_required_n)
met_events <- sum(sample_size_table$observed_meets_required_events)
met_nonevents <- sum(sample_size_table$observed_meets_required_nonevents)
total_models <- nrow(sample_size_table)
multivariable_met_n <- sum(multivariable$observed_meets_required_n)
multivariable_total <- nrow(multivariable)
met_all_counts <- sum(
  sample_size_table$observed_meets_required_n &
    sample_size_table$observed_meets_required_events &
    sample_size_table$observed_meets_required_nonevents
)
multivariable_met_all_counts <- sum(
  multivariable$observed_meets_required_n &
    multivariable$observed_meets_required_events &
    multivariable$observed_meets_required_nonevents
)

main_text_sentence <- paste0(
  "Post hoc sample size checks using pmsampsize (binary outcome, target shrinkage 0.90) used the mean SSL-augmented sample size and event count from each outer repeat/fold, combined with the labelled held-out test fold, to estimate model-specific outcome prevalence; across learner-specific main models, the mean SSL-augmented sample sizes were ",
  format_number(range_observed[1], 1),
  " to ",
  format_number(range_observed[2], 1),
  " participants with ",
  format_number(range_observed_events[1], 1),
  " to ",
  format_number(range_observed_events[2], 1),
  " CHR-P events, giving prevalence estimates of ",
  format_percent(range_prevalence[1], 1),
  " to ",
  format_percent(range_prevalence[2], 1),
  ". Using candidate parameter counts of ",
  paste(sort(unique(sample_size_table$candidate_parameters)), collapse = ", "),
  " and the cross-validated C-statistics from the main analyses, the SSL-augmented sample met the pmsampsize total-N, event, and non-event requirements for ",
  met_all_counts,
  " of ",
  total_models,
  " learner-specific main models, including all ",
  multivariable_met_all_counts,
  " of ",
  multivariable_total,
  " multivariable PPS/full models; for these multivariable models, the mean SSL-augmented sample size was ",
  format_number(range_multivariable_observed[1], 1),
  " to ",
  format_number(range_multivariable_observed[2], 1),
  " participants (",
  format_number(range_multivariable_observed_events[1], 1),
  " to ",
  format_number(range_multivariable_observed_events[2], 1),
  " events; prevalence ",
  format_percent(range_multivariable_prevalence[1], 1),
  " to ",
  format_percent(range_multivariable_prevalence[2], 1),
  "), compared with pmsampsize requirements of ",
  ceiling(range_multivariable_required[1]),
  " to ",
  ceiling(range_multivariable_required[2]),
  " participants (",
  ceiling(range_multivariable_events[1]),
  " to ",
  ceiling(range_multivariable_events[2]),
  " events and ",
  ceiling(range_multivariable_nonevents[1]),
  " to ",
  ceiling(range_multivariable_nonevents[2]),
  " non-events)."
)

writeLines(main_text_sentence, file.path(output_dir, "pmsampsize_main_text_sentence.txt"))

message("Saved pmsampsize outputs to ", output_dir)
