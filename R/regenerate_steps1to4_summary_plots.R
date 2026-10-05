library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(scales)

input_dir <- "revised_enter_unimodal_unbalanced_one_se_regularised_trees_steps1to4"
output_dir <- file.path(input_dir, "updated_figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

steps <- c(
  Step_1_pq16_score = "PQ-16 Score",
  Step_2_pq_distress = "PQ-16 Distress",
  Step_3_dsst = "DSST",
  Step_4_demographic_clinical = "PPS"
)
learner_labels <- c(glmnet = "Logistic regression", rf = "Random forest", xgb = "XGBoost")
learner_colours <- c(`Logistic regression` = "#4E79A7", `Random forest` = "#59A14F", XGBoost = "#E15759")

strip_step <- function(x) sub("__final_.*$", "", x)
strip_learner <- function(x) sub("^.*__final_", "", x)

parse_ci <- function(x) {
  match <- str_match(x, "^([0-9.]+) \\(([0-9.]+)-([0-9.]+)\\)$")
  tibble(estimate = as.numeric(match[, 2]), lower = as.numeric(match[, 3]), upper = as.numeric(match[, 4]))
}

results <- read_csv(file.path(input_dir, "results_summary.csv"), show_col_types = FALSE) %>%
  mutate(step = strip_step(model), learner_code = strip_learner(model)) %>%
  filter(step %in% names(steps)) %>%
  bind_cols(parse_ci(.$C)) %>%
  mutate(
    predictor_set = factor(unname(steps[step]), levels = unname(steps)),
    learner = factor(unname(learner_labels[learner_code]), levels = unname(learner_labels)),
    label_y = pmin(upper + if_else(step == "Step_3_dsst", 0.060, 0.025), 0.985)
  )

discrimination_plot <- ggplot(results, aes(x = predictor_set, y = estimate, colour = learner)) +
  annotate("rect", xmin = 0.5, xmax = 4.5, ymin = 0.40, ymax = 0.50, fill = "grey92") +
  annotate("rect", xmin = 0.5, xmax = 4.5, ymin = 0.70, ymax = 0.80, fill = "grey92") +
  geom_vline(xintercept = 1.5:3.5, linetype = 2, colour = "grey80", linewidth = 0.35) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.55), width = 0, linewidth = 0.6) +
  geom_point(position = position_dodge(width = 0.55), size = 2.2) +
  geom_text(
    aes(y = label_y, label = sprintf("%.2f", estimate)),
    position = position_dodge(width = 0.55), size = 3, show.legend = FALSE
  ) +
  annotate("text", x = 2.5, y = 0.855, label = "Excellent", colour = "grey65", size = 3) +
  annotate("text", x = 2.5, y = 0.745, label = "Acceptable", colour = "grey65", size = 3) +
  annotate("text", x = 2.5, y = 0.600, label = "Above chance", colour = "grey65", size = 3) +
  annotate("text", x = 2.5, y = 0.455, label = "Below chance", colour = "grey65", size = 3) +
  scale_colour_manual(values = learner_colours) +
  scale_y_continuous(limits = c(0.38, 1.02), breaks = seq(0.4, 1, 0.1)) +
  labs(x = "Predictor set", y = "C-index", colour = "Final learner") +
  theme_classic(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 9, face = "bold"),
    plot.margin = margin(8, 12, 8, 12)
  )

predictions <- read_csv(file.path(input_dir, "test_set_predictions.csv"), show_col_types = FALSE) %>%
  mutate(step = strip_step(model), learner_code = strip_learner(model)) %>%
  filter(step %in% names(steps)) %>%
  transmute(
    predictor_set = factor(unname(steps[step]), levels = unname(steps)),
    learner = factor(unname(learner_labels[learner_code]), levels = unname(learner_labels)),
    obs = as.numeric(obs),
    pred = pmin(pmax(as.numeric(pred), 1e-6), 1 - 1e-6)
  )

calibration_plot <- ggplot(predictions, aes(x = pred, y = obs, colour = learner, fill = learner)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey55", linewidth = 0.45) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE, span = 0.85, linewidth = 0.9, alpha = 0.14) +
  facet_grid(learner ~ predictor_set) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), breaks = c(0, .25, .5, .75, 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), breaks = c(0, .25, .5, .75, 1)) +
  scale_colour_manual(values = learner_colours, drop = FALSE) +
  scale_fill_manual(values = learner_colours, drop = FALSE) +
  labs(x = "Predicted risk", y = "Observed event rate") +
  theme_classic(base_size = 10) +
  theme(
    legend.position = "none",
    strip.text.x = element_text(face = "bold", size = 8),
    strip.text.y = element_text(face = "bold", size = 8),
    axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7),
    plot.margin = margin(8, 10, 8, 10)
  )

width <- 7.6
ggsave(file.path(output_dir, "figure_summary_c_index_unimodal.png"), discrimination_plot,
  width = width, height = width * 0.64, dpi = 300)
ggsave(file.path(output_dir, "figure_summary_c_index_unimodal.pdf"), discrimination_plot,
  width = width, height = width * 0.64)
ggsave(file.path(output_dir, "figure_summary_calibration_unimodal.png"), calibration_plot,
  width = 8.2, height = 5.6, dpi = 300)
ggsave(file.path(output_dir, "figure_summary_calibration_unimodal.pdf"), calibration_plot,
  width = 8.2, height = 5.6)
