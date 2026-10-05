library(dplyr)
library(readr)

output_dir <- "paper_outputs_final_one_se"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

step_labels <- c(
  Step_1_pq16_score = "PQ-16 Score",
  Step_2_pq_distress = "PQ-16 Distress",
  Step_3_dsst = "DSST",
  Step_4_demographic_clinical = "PPS",
  Step_5_full_model = "All"
)
learner_labels <- c(glmnet = "Elastic net", rf = "Random forest", xgb = "XGBoost")
strip_step <- function(x) sub("__final_.*$", "", x)
strip_learner <- function(x) sub("^.*__final_", "", x)

results <- bind_rows(
  read_csv("main_primary_unbalanced_one_se_regularised_trees_steps1to4/results_summary.csv", show_col_types = FALSE) %>%
    filter(strip_step(model) %in% names(step_labels)[1:4]),
  read_csv("step5_unbalanced_one_se_regularised_trees/results_summary.csv", show_col_types = FALSE) %>%
    filter(strip_step(model) == "Step_5_full_model")
) %>%
  mutate(
    `Predictor set` = unname(step_labels[strip_step(model)]),
    Learner = unname(learner_labels[strip_learner(model)]),
    step_order = match(strip_step(model), names(step_labels)),
    learner_order = match(strip_learner(model), names(learner_labels))
  ) %>%
  arrange(step_order, learner_order) %>%
  transmute(
    `Predictor set`, Learner, `C-index` = C,
    `Balanced accuracy` = balanced_accuracy, Sensitivity = sensitivity,
    Specificity = specificity, PPV = ppv, NPV = npv, `Brier score` = brier,
    `Calibration intercept` = calibration_intercept,
    `Calibration slope` = calibration_slope
  )

write_csv(results, file.path(output_dir, "table2_main_model_performance.csv"))
