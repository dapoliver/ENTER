library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)
library(scales)

old_output_dir <- "main_primary_unbalanced_one_se_regularised_trees_steps1to4"
elastic_net_output_dir <- "step5_unbalanced_one_se_regularised_trees"
output_dir <- "paper_outputs_final_one_se"
figure_dir <- file.path(output_dir, "updated_figures")
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

step_labels <- c(
  Step_1_pq16_score = "PQ-16 Score",
  Step_2_pq_distress = "PQ-16 Distress",
  Step_3_dsst = "DSST",
  Step_4_demographic_clinical = "PPS",
  Step_5_full_model = "All"
)

learner_labels <- c(
  glmnet = "Elastic net",
  rf = "Random forest",
  xgb = "XGBoost"
)

learner_short <- c(
  glmnet = "Elastic net",
  rf = "RF",
  xgb = "XGBoost"
)

learner_colours <- c(
  `Elastic net` = "#4E79A7",
  `Random forest` = "#59A14F",
  XGBoost = "#E15759",
  RF = "#59A14F"
)

parse_ci <- function(x) {
  m <- str_match(x, "^([0-9.]+) \\(([0-9.]+)-([0-9.]+)\\)$")
  tibble(
    estimate = as.numeric(m[, 2]),
    lower = as.numeric(m[, 3]),
    upper = as.numeric(m[, 4])
  )
}

strip_step <- function(model) sub("__final_.*$", "", model)
strip_learner <- function(model) sub("^.*__final_", "", model)

read_combined_results <- function() {
  old_results <- read_csv(file.path(old_output_dir, "results_summary.csv"), show_col_types = FALSE)
  elastic_net_results <- read_csv(file.path(elastic_net_output_dir, "results_summary.csv"), show_col_types = FALSE)

  bind_rows(
    old_results %>%
      mutate(step = strip_step(model)) %>%
      filter(step %in% c("Step_1_pq16_score", "Step_2_pq_distress", "Step_3_dsst", "Step_4_demographic_clinical")),
    elastic_net_results %>%
      mutate(step = strip_step(model)) %>%
      filter(step %in% c("Step_5_full_model"))
  ) %>%
    select(-step)
}

read_combined_predictions <- function() {
  old_predictions <- read_csv(file.path(old_output_dir, "test_set_predictions.csv"), show_col_types = FALSE)
  elastic_net_predictions <- read_csv(file.path(elastic_net_output_dir, "test_set_predictions.csv"), show_col_types = FALSE)

  bind_rows(
    old_predictions %>%
      mutate(step = strip_step(model)) %>%
      filter(step %in% c("Step_1_pq16_score", "Step_2_pq_distress", "Step_3_dsst", "Step_4_demographic_clinical")),
    elastic_net_predictions %>%
      mutate(step = strip_step(model)) %>%
      filter(step %in% c("Step_5_full_model"))
  ) %>%
    select(-step)
}

results <- read_combined_results() %>%
  mutate(
    step = strip_step(model),
    learner = strip_learner(model)
  ) %>%
  filter(step %in% names(step_labels)) %>%
  bind_cols(parse_ci(.$C)) %>%
  mutate(
    predictor_set = factor(step_labels[step], levels = unname(step_labels)),
    learner_short = factor(learner_short[learner], levels = c("Elastic net", "RF", "XGBoost"))
  )

cindex_plot <- ggplot(results, aes(x = predictor_set, y = estimate, colour = learner_short)) +
  annotate("rect", xmin = 0.5, xmax = 5.5, ymin = 0.40, ymax = 0.50, fill = "grey92") +
  annotate("rect", xmin = 0.5, xmax = 5.5, ymin = 0.70, ymax = 0.80, fill = "grey92") +
  geom_vline(xintercept = 1.5:4.5, linetype = 2, colour = "grey80", linewidth = 0.35) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.55),
    width = 0,
    linewidth = 0.6
  ) +
  geom_point(position = position_dodge(width = 0.55), size = 2.2) +
  geom_text(
    aes(y = upper + 0.025, label = sprintf("%.2f", estimate)),
    position = position_dodge(width = 0.55),
    size = 3,
    show.legend = FALSE
  ) +
  annotate("text", x = 3, y = 0.855, label = "Excellent", colour = "grey65", size = 3) +
  annotate("text", x = 3, y = 0.745, label = "Acceptable", colour = "grey65", size = 3) +
  annotate("text", x = 4.5, y = 0.600, label = "Above chance", colour = "grey65", size = 3) +
  annotate("text", x = 3, y = 0.455, label = "Below chance", colour = "grey65", size = 3) +
  scale_colour_manual(values = learner_colours[c("Elastic net", "RF", "XGBoost")], drop = FALSE) +
  scale_y_continuous(limits = c(0.38, 1.02), breaks = seq(0.4, 1.0, 0.1)) +
  labs(x = "Predictor set", y = "C-index", colour = "Final learner") +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 9, face = "bold"),
    plot.margin = margin(8, 12, 8, 12)
  )

figure_2a_width <- 8.2
figure_2a_height <- figure_2a_width * 0.64
ggsave(file.path(figure_dir, "figure_2a_c_index_no_pq_cutoff.png"), cindex_plot, width = figure_2a_width, height = figure_2a_height, dpi = 300)
ggsave(file.path(figure_dir, "figure_2a_c_index_no_pq_cutoff.pdf"), cindex_plot, width = figure_2a_width, height = figure_2a_height)

predictions <- read_combined_predictions() %>%
  mutate(
    step = strip_step(model),
    learner = strip_learner(model)
  ) %>%
  filter(step %in% names(step_labels)) %>%
  mutate(
    predictor_set = factor(step_labels[step], levels = unname(step_labels)),
    learner = factor(learner_labels[learner], levels = unname(learner_labels)),
    pred = pmin(pmax(pred, 1e-6), 1 - 1e-6)
  )

calibration_plot <- ggplot(predictions, aes(x = pred, y = obs, colour = learner, fill = learner)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey55", linewidth = 0.45) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE, span = 0.85, linewidth = 0.9, alpha = 0.14) +
  facet_grid(learner ~ predictor_set) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), breaks = c(0, 0.25, 0.50, 0.75, 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), breaks = c(0, 0.25, 0.50, 0.75, 1)) +
  scale_colour_manual(values = learner_colours[unname(learner_labels)], drop = FALSE) +
  scale_fill_manual(values = learner_colours[unname(learner_labels)], drop = FALSE) +
  labs(x = "Predicted risk", y = "Observed event rate") +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.text.x = element_text(face = "bold", size = 8),
    strip.text.y = element_text(face = "bold", size = 8),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    plot.margin = margin(8, 10, 8, 10)
  )

ggsave(file.path(figure_dir, "figure_2b_calibration_no_pq_cutoff.png"), calibration_plot, width = 9.8, height = 5.6, dpi = 300)
ggsave(file.path(figure_dir, "figure_2b_calibration_no_pq_cutoff.pdf"), calibration_plot, width = 9.8, height = 5.6)

calculate_net_benefit <- function(obs, pred, thresholds, prevalence = NULL) {
  observed_prevalence <- mean(obs == 1, na.rm = TRUE)
  event_weight <- 1
  nonevent_weight <- 1
  if (!is.null(prevalence) && is.finite(prevalence) && observed_prevalence > 0 && observed_prevalence < 1) {
    event_weight <- prevalence / observed_prevalence
    nonevent_weight <- (1 - prevalence) / (1 - observed_prevalence)
  }
  total_weight <- sum(ifelse(obs == 1, event_weight, nonevent_weight), na.rm = TRUE)
  vapply(thresholds, function(pt) {
    classified_positive <- pred >= pt
    tp_weight <- sum(ifelse(obs == 1 & classified_positive, event_weight, 0), na.rm = TRUE)
    fp_weight <- sum(ifelse(obs == 0 & classified_positive, nonevent_weight, 0), na.rm = TRUE)
    tp_weight / total_weight - fp_weight / total_weight * (pt / (1 - pt))
  }, numeric(1))
}

thresholds <- seq(0.01, 0.50, by = 0.01)
dca_models <- predictions %>%
  filter(step == "Step_5_full_model") %>%
  group_by(learner) %>%
  summarise(
    threshold = list(thresholds),
    net_benefit = list(calculate_net_benefit(obs, pred, thresholds, prevalence = 0.283)),
    .groups = "drop"
  ) %>%
  unnest(c(threshold, net_benefit)) %>%
  rename(curve = learner)

dca_defaults <- tibble(
  threshold = rep(thresholds, 2),
  curve = rep(c("Assess all", "Assess none"), each = length(thresholds)),
  net_benefit = c(0.283 - (1 - 0.283) * thresholds / (1 - thresholds), rep(0, length(thresholds)))
)

dca_plot_df <- bind_rows(dca_models, dca_defaults) %>%
  mutate(curve = factor(curve, levels = c(unname(learner_labels), "Assess all", "Assess none")))

dca_colours <- c(
  learner_colours[unname(learner_labels)],
  `Assess all` = "#000000",
  `Assess none` = "#666666"
)

dca_plot <- ggplot(dca_plot_df, aes(x = threshold, y = net_benefit, colour = curve)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.5) +
  geom_line(linewidth = 1) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0.01, 0.50), breaks = seq(0, 0.5, 0.1)) +
  scale_y_continuous(limits = c(-0.45, 0.30), breaks = seq(-0.4, 0.2, 0.2)) +
  scale_colour_manual(values = dca_colours, drop = FALSE) +
  labs(x = "Threshold probability", y = "Net benefit", colour = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.margin = margin(8, 10, 8, 10)
  )

write_csv(dca_plot_df, file.path(figure_dir, "figure_2c_dca_full_model_prevalence_28_3.csv"))

dca_incremental_table <- dca_plot_df %>%
  filter(curve %in% unname(learner_labels)) %>%
  left_join(
    dca_defaults %>%
      select(threshold, curve, net_benefit) %>%
      pivot_wider(names_from = curve, values_from = net_benefit),
    by = "threshold"
  ) %>%
  mutate(
    best_default = pmax(`Assess all`, `Assess none`),
    incremental_net_benefit = net_benefit - best_default,
    standardised_incremental_net_benefit = incremental_net_benefit / 0.283
  ) %>%
  transmute(
    `Threshold probability` = threshold,
    Learner = as.character(curve),
    `Model net benefit` = net_benefit,
    `Assess-all net benefit` = `Assess all`,
    `Assess-none net benefit` = `Assess none`,
    `Best default net benefit` = best_default,
    `Incremental net benefit` = incremental_net_benefit,
    `Standardised incremental net benefit` = standardised_incremental_net_benefit
  )
write_csv(dca_incremental_table, file.path(output_dir, "etable4_dca_incremental_net_benefit_prevalence_28_3.csv"))
ggsave(file.path(figure_dir, "figure_2c_dca_full_model_prevalence_28_3.png"), dca_plot, width = 8.2, height = 5.0, dpi = 300)
ggsave(file.path(figure_dir, "figure_2c_dca_full_model_prevalence_28_3.pdf"), dca_plot, width = 8.2, height = 5.0)

message("Saved updated figures to: ", normalizePath(figure_dir))
