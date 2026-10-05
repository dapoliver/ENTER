library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(scales)

final_dir <- "revised_enter_step5_unbalanced_one_se_regularised_trees"
step5_file <- file.path(final_dir, "test_set_predictions.csv")
pq16_file <- "revised_pq16_cutoff_unbalanced_one_se_regularised_trees/test_set_predictions.csv"
output_dir <- file.path(final_dir, "updated_figures")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

thresholds <- seq(0, 1, by = 0.01)
learner_labels <- c(glmnet = "Logistic regression", rf = "Random forest", xgb = "XGBoost")
learner_colours <- c(`Logistic regression` = "#4E79A7", `Random forest` = "#59A14F", XGBoost = "#E15759")

strip_learner <- function(x) sub("^.*__final_", "", x)

read_predictions <- function(file, model_family, pattern) {
  read_csv(file, show_col_types = FALSE) %>%
    filter(grepl(pattern, model)) %>%
    filter(model_family == "ENTER" | strip_learner(model) == "glmnet") %>%
    transmute(
      model_family = model_family,
      learner = unname(learner_labels[strip_learner(model)]),
      Subject_ID = as.character(Subject_ID),
      obs = as.numeric(obs),
      pred = pmin(pmax(as.numeric(pred), 0), 1)
    )
}

predictions <- bind_rows(
  read_predictions(step5_file, "ENTER", "^Step_5_full_model__final_"),
  read_predictions(pq16_file, "PQ-16 cut-off", "^Step_1b_pq16_cutoff__final_")
)

classification_data <- predictions %>%
  tidyr::crossing(threshold = thresholds) %>%
  group_by(model_family, learner, threshold) %>%
  summarise(
    tp = sum(obs == 1 & pred >= threshold, na.rm = TRUE),
    fn = sum(obs == 1 & pred < threshold, na.rm = TRUE),
    fp = sum(obs == 0 & pred >= threshold, na.rm = TRUE),
    tn = sum(obs == 0 & pred < threshold, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    TPR = if_else(tp + fn > 0, tp / (tp + fn), NA_real_),
    FPR = if_else(fp + tn > 0, fp / (fp + tn), NA_real_)
  ) %>%
  select(model_family, learner, threshold, TPR, FPR) %>%
  pivot_longer(c(TPR, FPR), names_to = "metric", values_to = "proportion") %>%
  mutate(
    metric = recode(metric, TPR = "True positive rate", FPR = "False positive rate"),
    metric = factor(metric, levels = c("True positive rate", "False positive rate")),
    model_family = factor(model_family, levels = c("ENTER", "PQ-16 cut-off")),
    learner = factor(learner, levels = unname(learner_labels))
  )

plot <- ggplot(
  classification_data,
  aes(
    x = threshold, y = proportion,
    colour = learner, linetype = model_family, alpha = model_family,
    group = interaction(model_family, learner)
  )
) +
  geom_line(linewidth = 1) +
  facet_wrap(~metric, ncol = 1) +
  scale_x_continuous(
    labels = percent_format(accuracy = 10),
    limits = c(0, 1), breaks = seq(0, 1, 0.1)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 10),
    limits = c(0, 1), breaks = seq(0, 1, 0.1)
  ) +
  scale_colour_manual(values = learner_colours, name = "Learner") +
  scale_linetype_manual(
    values = c(ENTER = "solid", `PQ-16 cut-off` = "dashed"),
    name = "Model"
  ) +
  scale_alpha_manual(values = c(ENTER = 1, `PQ-16 cut-off` = 0.55), guide = "none") +
  labs(x = "Risk threshold", y = "Proportion") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "vertical",
    strip.text = element_text(face = "bold"),
    plot.margin = margin(8, 10, 8, 10)
  )

write_csv(
  classification_data,
  file.path(output_dir, "classification_plot_data.csv")
)
ggsave(
  file.path(output_dir, "classification_plot_pq16_cutoff_and_enter.png"),
  plot, width = 8.2, height = 7.0, dpi = 300
)
ggsave(
  file.path(output_dir, "classification_plot_pq16_cutoff_and_enter.pdf"),
  plot, width = 8.2, height = 7.0
)

message("Saved classification plot to ", output_dir)
