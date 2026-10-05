# Paired bootstrap comparison of model performance.
# The same participant-level bootstrap sample is used for every model.

suppressPackageStartupMessages(library(tidyverse))

input_dir <- Sys.getenv(
  "PAIRED_COMPARISON_INPUT_DIR",
  unset = "/Users/domoliver/Library/CloudStorage/Dropbox/Work/ENTER/CAARMS Prediction/Analyses/fixed_selected_ssl_rule_main_all_learners"
)
output_dir <- Sys.getenv(
  "PAIRED_COMPARISON_OUTPUT_DIR",
  unset = file.path(input_dir, "paired_model_comparisons")
)
B <- suppressWarnings(as.integer(Sys.getenv("PAIRED_COMPARISON_B", "2000")))
if (is.na(B) || B < 100) B <- 2000
seed <- suppressWarnings(as.integer(Sys.getenv("PAIRED_COMPARISON_SEED", "123")))
if (is.na(seed)) seed <- 123
model_pattern <- Sys.getenv(
  "PAIRED_COMPARISON_MODEL_PATTERN",
  unset = "^(Step_1b_pq16_cutoff|Step_5_full_model)"
)

predictions <- read_csv(
  file.path(input_dir, "test_set_predictions.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    Subject_ID = as.character(Subject_ID),
    obs = as.numeric(obs),
    pred = as.numeric(pred),
    chr_pred = as.numeric(chr_pred)
  ) %>%
  group_by(model, Subject_ID) %>%
  summarise(
    obs = first(obs),
    pred = mean(pred, na.rm = TRUE),
    chr_pred = mean(chr_pred, na.rm = TRUE),
    .groups = "drop"
  )

predictions <- predictions %>% filter(grepl(model_pattern, model))
if (nrow(predictions) == 0) stop("No models matched PAIRED_COMPARISON_MODEL_PATTERN.")

models <- sort(unique(predictions$model))
if (length(models) < 2) stop("At least two models are required.")

# AUC/C-statistic calculated without requiring a model-fitting step.
auc <- function(obs, pred) {
  keep <- !is.na(obs) & !is.na(pred)
  obs <- obs[keep]; pred <- pred[keep]
  if (length(unique(obs)) < 2) return(NA_real_)
  n1 <- sum(obs == 1); n0 <- sum(obs == 0)
  (sum(rank(pred)[obs == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

metric_values <- function(obs, pred, chr_pred) {
  keep <- !is.na(obs) & !is.na(pred) & !is.na(chr_pred)
  obs <- obs[keep]; pred <- pred[keep]; chr_pred <- chr_pred[keep]
  if (!length(obs)) return(c(C = NA, balanced_accuracy = NA, brier = NA,
                             calibration_intercept = NA, calibration_slope = NA))
  pred_class <- as.integer(chr_pred >= 0.5)
  tp <- sum(pred_class == 1 & obs == 1); tn <- sum(pred_class == 0 & obs == 0)
  fp <- sum(pred_class == 1 & obs == 0); fn <- sum(pred_class == 0 & obs == 1)
  sens <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  spec <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
  cal <- tryCatch({
    p <- pmin(pmax(as.numeric(pred), 0.001), 0.999)
    fit <- suppressWarnings(glm(obs ~ p, family = binomial()))
    coefs <- coef(fit)
    c(intercept = unname(coefs[1]), slope = unname(coefs[2]))
  }, error = function(e) c(intercept = NA, slope = NA))
  c(
    C = auc(obs, pred),
    balanced_accuracy = mean(c(sens, spec), na.rm = TRUE),
    brier = mean((obs - pred)^2),
    calibration_intercept = unname(cal["intercept"]),
    calibration_slope = unname(cal["slope"])
  )
}

set.seed(seed)
participants <- predictions %>% distinct(Subject_ID, obs)
pred_wide <- predictions %>%
  select(Subject_ID, model, pred, chr_pred) %>%
  pivot_wider(names_from = model, values_from = c(pred, chr_pred))

compare_pair <- function(model_a, model_b) {
  metric_names <- c("C", "balanced_accuracy", "brier",
                    "calibration_intercept", "calibration_slope")
  out <- matrix(NA_real_, nrow = B, ncol = length(metric_names),
                dimnames = list(NULL, metric_names))
  for (b in seq_len(B)) {
    sampled_ids <- sample(participants$Subject_ID, nrow(participants), replace = TRUE)
    sampled <- participants[match(sampled_ids, participants$Subject_ID), , drop = FALSE] %>%
      left_join(pred_wide, by = "Subject_ID")
    a <- metric_values(sampled$obs, sampled[[paste0("pred_", model_a)]],
                       sampled[[paste0("chr_pred_", model_a)]])
    bb <- metric_values(sampled$obs, sampled[[paste0("pred_", model_b)]],
                        sampled[[paste0("chr_pred_", model_b)]])
    out[b, ] <- a[metric_names] - bb[metric_names]
  }
  tibble(
    model_a = model_a, model_b = model_b,
    metric = metric_names,
    difference = apply(out, 2, mean, na.rm = TRUE),
    ci_low = apply(out, 2, quantile, probs = 0.025, na.rm = TRUE),
    ci_high = apply(out, 2, quantile, probs = 0.975, na.rm = TRUE),
    p_value = apply(out, 2, function(x) {
      x <- x[is.finite(x)]
      if (!length(x)) return(NA_real_)
      min(1, 2 * min(mean(x <= 0), mean(x >= 0)))
    })
  )
}

results <- combn(models, 2, simplify = FALSE) %>%
  map_dfr(~ compare_pair(.x[1], .x[2]))

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(results, file.path(output_dir, "paired_model_performance_differences.csv"))
write_csv(tibble(model = models, n_participants = nrow(participants), bootstrap_B = B,
                 seed = seed), file.path(output_dir, "paired_model_comparison_metadata.csv"))
message("Saved paired model comparisons to ", output_dir)
