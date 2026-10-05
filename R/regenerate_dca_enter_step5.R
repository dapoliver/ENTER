library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(scales)

input_file <- "revised_enter_step5_unbalanced_one_se_regularised_trees/test_set_predictions.csv"
pq16_input_file <- "revised_pq16_cutoff_unbalanced_one_se_regularised_trees/test_set_predictions.csv"
output_dir <- "revised_enter_step5_unbalanced_one_se_regularised_trees/updated_figures"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

prevalence <- 0.283
thresholds <- seq(0.01, 0.50, by = 0.01)
learner_labels <- c(glmnet = "Logistic regression", rf = "Random forest", xgb = "XGBoost")
learner_colours <- c(`Logistic regression` = "#4E79A7", `Random forest` = "#59A14F", XGBoost = "#E15759")

strip_learner <- function(x) sub("^.*__final_", "", x)

calculate_net_benefit <- function(obs, pred, thresholds, prevalence) {
  observed_prevalence <- mean(obs == 1, na.rm = TRUE)
  event_weight <- prevalence / observed_prevalence
  nonevent_weight <- (1 - prevalence) / (1 - observed_prevalence)
  total_weight <- sum(ifelse(obs == 1, event_weight, nonevent_weight), na.rm = TRUE)

  vapply(thresholds, function(threshold) {
    classify_positive <- pred >= threshold
    true_positive_weight <- sum(ifelse(obs == 1 & classify_positive, event_weight, 0), na.rm = TRUE)
    false_positive_weight <- sum(ifelse(obs == 0 & classify_positive, nonevent_weight, 0), na.rm = TRUE)
    true_positive_weight / total_weight - false_positive_weight / total_weight * threshold / (1 - threshold)
  }, numeric(1))
}

predictions <- read_csv(input_file, show_col_types = FALSE) %>%
  filter(grepl("^(Step_5_full_model|Step_1b_pq16_cutoff)__final_", model)) %>%
  filter(grepl("^Step_5_full_model", model) | strip_learner(model) == "glmnet") %>%
  transmute(
    model_family = if_else(grepl("^Step_5_full_model", model), "ENTER", "PQ-16 cut-off"),
    learner = factor(learner_labels[strip_learner(model)], levels = unname(learner_labels)),
    obs = as.numeric(obs),
    pred = pmin(pmax(as.numeric(pred), 1e-6), 1 - 1e-6)
  ) %>%
  bind_rows(
    read_csv(pq16_input_file, show_col_types = FALSE) %>%
      filter(grepl("^Step_1b_pq16_cutoff__final_", model)) %>%
      filter(strip_learner(model) == "glmnet") %>%
      transmute(
        model_family = "PQ-16 cut-off",
        learner = factor(learner_labels[strip_learner(model)], levels = unname(learner_labels)),
        obs = as.numeric(obs),
        pred = pmin(pmax(as.numeric(pred), 1e-6), 1 - 1e-6)
      )
  )

model_curves <- predictions %>%
  group_by(model_family, learner) %>%
  summarise(
    threshold = list(thresholds),
    net_benefit = list(calculate_net_benefit(obs, pred, thresholds, prevalence)),
    .groups = "drop"
  ) %>%
  unnest(c(threshold, net_benefit)) %>%
  rename(curve = learner)

default_curves <- tibble(
  threshold = rep(thresholds, 2),
  curve = rep(c("Assess all", "Assess none"), each = length(thresholds)),
  net_benefit = c(
    prevalence - (1 - prevalence) * thresholds / (1 - thresholds),
    rep(0, length(thresholds))
  ),
  model_family = "Reference"
)

dca_data <- bind_rows(model_curves, default_curves) %>%
  mutate(
    curve = factor(curve, levels = c(unname(learner_labels), "Assess all", "Assess none")),
    model_family = factor(model_family, levels = c("ENTER", "PQ-16 cut-off", "Reference"))
  )

plot <- ggplot(dca_data, aes(x = threshold, y = net_benefit, colour = curve, linetype = model_family, alpha = model_family, group = interaction(model_family, curve))) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.5) +
  geom_line(linewidth = 1) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(.01, .50), breaks = seq(0, .5, .1)) +
  scale_y_continuous(limits = c(-.45, .30), breaks = seq(-.4, .2, .2)) +
  scale_colour_manual(values = c(learner_colours, `Assess all` = "#000000", `Assess none` = "#666666")) +
  scale_linetype_manual(values = c(ENTER = "solid", `PQ-16 cut-off` = "dashed", Reference = "solid"), name = "Model") +
  scale_alpha_manual(values = c(ENTER = 1, `PQ-16 cut-off` = 0.55, Reference = 0.8), guide = "none") +
  labs(x = "Threshold probability", y = "Net benefit", colour = "Learner") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", legend.box = "vertical", plot.margin = margin(8, 10, 8, 10))

write_csv(dca_data, file.path(output_dir, "figure_dca_enter_step5_prevalence_28_3.csv"))
ggsave(file.path(output_dir, "figure_dca_enter_step5_prevalence_28_3.png"), plot, width = 8.2, height = 5.0, dpi = 300)
ggsave(file.path(output_dir, "figure_dca_enter_step5_prevalence_28_3.pdf"), plot, width = 8.2, height = 5.0)

ggsave(file.path(output_dir, "figure_dca_pq16_cutoff_and_enter_prevalence_28_3.png"), plot, width = 8.2, height = 5.0, dpi = 300)
ggsave(file.path(output_dir, "figure_dca_pq16_cutoff_and_enter_prevalence_28_3.pdf"), plot, width = 8.2, height = 5.0)
write_csv(dca_data, file.path(output_dir, "figure_dca_pq16_cutoff_and_enter_prevalence_28_3.csv"))
