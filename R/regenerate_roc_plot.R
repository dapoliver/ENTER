library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

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
      obs = as.numeric(obs),
      pred = pmin(pmax(as.numeric(pred), 0), 1)
    )
}

predictions <- bind_rows(
  read_predictions(step5_file, "ENTER", "^Step_5_full_model__final_"),
  read_predictions(pq16_file, "PQ-16 cut-off", "^Step_1b_pq16_cutoff__final_")
)

roc_data <- predictions %>%
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
    FPR = if_else(fp + tn > 0, fp / (fp + tn), NA_real_),
    model_family = factor(model_family, levels = c("ENTER", "PQ-16 cut-off")),
    learner = factor(learner, levels = unname(learner_labels))
  )

auc <- predictions %>%
  group_by(model_family, learner) %>%
  summarise(
    auc = {
      r <- rank(pred)
      n1 <- sum(obs == 1); n0 <- sum(obs == 0)
      (sum(r[obs == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
    },
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(model_family, ": ", learner, " (AUC = ", sprintf("%.3f", auc), ")")
  )

roc_data <- roc_data %>%
  left_join(auc %>% select(model_family, learner, label), by = c("model_family", "learner"))

plot <- ggplot(
  roc_data,
  aes(
    x = FPR, y = TPR, colour = learner, linetype = model_family,
    alpha = model_family, group = interaction(model_family, learner)
  )
) +
  geom_abline(slope = 1, intercept = 0, colour = "grey60", linewidth = 0.6) +
  geom_step(linewidth = 1) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = scales::percent) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = scales::percent) +
  scale_colour_manual(values = learner_colours, name = "Learner") +
  scale_linetype_manual(values = c(ENTER = "solid", `PQ-16 cut-off` = "dashed"), name = "Model") +
  scale_alpha_manual(values = c(ENTER = 1, `PQ-16 cut-off` = 0.55), guide = "none") +
  labs(x = "False-positive rate", y = "True-positive rate") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", legend.box = "vertical")

write_csv(roc_data, file.path(output_dir, "roc_plot_data.csv"))
write_csv(auc, file.path(output_dir, "roc_auc_summary.csv"))
ggsave(file.path(output_dir, "roc_plot_pq16_cutoff_and_enter.png"), plot, width = 8.2, height = 6.5, dpi = 300)
ggsave(file.path(output_dir, "roc_plot_pq16_cutoff_and_enter.pdf"), plot, width = 8.2, height = 6.5)
message("Saved ROC plot to ", output_dir)
