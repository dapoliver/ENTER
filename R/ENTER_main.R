ENTER_PROJECT_ROOT <- Sys.getenv("ENTER_PROJECT_ROOT", unset = getwd())
ENTER_WORK_ROOT <- Sys.getenv("ENTER_WORK_ROOT", unset = dirname(dirname(ENTER_PROJECT_ROOT)))
setwd(ENTER_PROJECT_ROOT)
output_dir_default <- file.path(ENTER_PROJECT_ROOT, "Analyses")
output_dir_env <- Sys.getenv("OUTPUT_DIR", "")
output_dir <- if (nzchar(output_dir_env)) output_dir_env else output_dir_default
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(missForest) # For random forest imputation
library(mice) # For MICE imputation
library(VIM) # For kNN imputation
library(caret) # For prediction modelling
library(pROC)
library(glmnet) # For logistic regression and Cox
library(survival) # For survival data
library(metrica) # For posLR and negLR
library(predRupdate) # For calibration
library(doParallel) # For parallel processing

# Participant-level preprocessing used by all model runs.
deduplicate_most_complete <- function(data, id = "enterid", completeness_vars = names(data)) {
  completeness_vars <- intersect(completeness_vars, names(data))
  data %>%
    mutate(.complete_n = rowSums(!is.na(across(all_of(completeness_vars))))) %>%
    arrange(.data[[id]], desc(.complete_n)) %>%
    distinct(.data[[id]], .keep_all = TRUE) %>%
    select(-.complete_n)
}

# Load the data
ENTER_UK <- read_csv("ENTERAssessment_DATA_2025-02-13_1235.csv")
ENTER_UK <- ENTER_UK %>% mutate(enterid = as.character(enterid)) %>% subset(select = c(enterid, caarms_outcome_arms, caarms_outcome_psy)) %>% filter(!is.na(caarms_outcome_arms)) %>% deduplicate_most_complete()

ENTER_UK_check <- read_excel(file.path(ENTER_WORK_ROOT, "Papers/Published/CBD Acceptability CHR-P/ENTER Finalised CAARMS Categories.xlsx"))
ENTER_UK_check <- ENTER_UK_check %>% mutate(
  caarms_outcome_arms = case_when(
    `Baseline CAARMS` != "No Risk" & `Baseline CAARMS` != "No risk" & `Baseline CAARMS` != "Psychosis Threshold Met" & `Baseline CAARMS` != "GRD" ~ 1,
    TRUE ~ 0
  ),
  caarms_outcome_psy = case_when(
    `Baseline CAARMS` == "Psychosis Threshold Met" ~ 1,
    TRUE ~ 0
  ),
  enterid = as.character(`ENTER ID`),
  caarms_outcome_arms_check = caarms_outcome_arms,
  caarms_outcome_psy_check = caarms_outcome_psy
)
ENTER_UK_check <- ENTER_UK_check %>% subset(select = c(enterid, caarms_outcome_arms_check, caarms_outcome_psy_check))
ENTER_UK_check <- deduplicate_most_complete(ENTER_UK_check)
ENTER_UK <- merge(ENTER_UK, ENTER_UK_check, by="enterid", all.x=TRUE)
ENTER_UK <- ENTER_UK %>% mutate(
  caarms_outcome_arms = case_when(!is.na(caarms_outcome_arms_check) ~ caarms_outcome_arms_check,
                                  TRUE ~ caarms_outcome_arms),
  caarms_outcome_psy = case_when(!is.na(caarms_outcome_psy_check) ~ caarms_outcome_psy_check,
                                  TRUE ~ caarms_outcome_psy))
ENTER_UK <- ENTER_UK %>% subset(select = c(enterid, caarms_outcome_arms, caarms_outcome_psy))

ENTER_UK_web <- read_csv("ENTEROnline_DATA_2025-05-30_1704.csv")
ENTER_UK_web <- ENTER_UK_web %>% subset(select = c(
  enterid, pq16_1, pq16_1_distress, pq16_2, pq16_2_distress,
  pq16_3, pq16_3_distress, pq16_4, pq16_4_distress,
  pq16_5, pq16_5_distress, pq16_6, pq16_6_distress,
  pq16_7, pq16_7_distress, pq16_8, pq16_8_distress,
  pq16_9, pq16_9_distress, pq16_10, pq16_10_distress,
  pq16_11, pq16_11_distress, pq16_12, pq16_12_distress,
  pq16_13, pq16_13_distress, pq16_14, pq16_14_distress,
  pq16_15, pq16_15_distress, pq16_16, pq16_16_distress,
  pq16_score,
  pps_genetic_risk___1, pps_genetic_risk___2, pps_genetic_risk___3,
  pps_genetic_risk___4, pps_genetic_risk___5, pps_genetic_risk___6,
  pps_yeti_q1, pps_yeti_q2, pps_yeti_q3, pps_yeti_q4,
  pps_yeti_q5, pps_yeti_q6, pps_yeti_q7, pps_yeti_q8,
  pps_yeti_q9, pps_yeti_q10, pps_yeti_q11, pps_yeti_q12,
  pps_yeti_q13, pps_yeti_q14, pps_yeti_q15, pps_yeti_q16,
  pps_yeti_q17, pps_yeti_q18, pps_yeti_q19, pps_yeti_q20,
  pps_yeti_q21, pps_yeti_q22, pps_yeti_q23, pps_yeti_q24,
  pps_yeti_q25, pps_yeti_q26,
  dsst
))
valid <- read_csv("Validations_04.02.25.csv")
valid <- valid %>% filter(manual_validation == "Valid") %>% mutate(enter_id = as.character(enter_id))
ENTER_UK_web <- ENTER_UK_web %>% mutate(enterid = as.character(enterid)) %>% filter(enterid %in% valid$enter_id) %>% deduplicate_most_complete()

ENTER_PPS_UK <- read_csv(file.path(ENTER_WORK_ROOT, "PPS/ENTER/ENTER_PPS_Processing_UK/PPS_ENTER_clean_190626.csv"))
ENTER_PPS_UK <- ENTER_PPS_UK %>% subset(select = -c(PPS))
ENTER_PPS_UK <- ENTER_PPS_UK %>% dplyr::rename(PPS = PPS_new)
ENTER_PPS_UK <- ENTER_PPS_UK %>% mutate(enterid = as.character(enterid)) %>% deduplicate_most_complete()
ENTER_UK <- merge(ENTER_UK, ENTER_UK_web, by = "enterid", all.y = TRUE)
ENTER_UK <- merge(ENTER_UK, ENTER_PPS_UK, by = "enterid", all.x = TRUE)

ENTER_PV <- read_excel(file.path(ENTER_WORK_ROOT, "ENTER/Bipolar Prediction/ENTER_UK_Pavia_20.02.25_BPSS_SIBARS.xlsx"), sheet = "ENTER_Pavia")
ENTER_PV <- ENTER_PV %>% mutate(enterid = as.character(enterid)) %>% subset(select = c(enterid, caarms_outcome_arms, caarms_outcome_psy)) %>% deduplicate_most_complete()
ENTER_PV_web <- read_csv(file.path(ENTER_PROJECT_ROOT, "ENTERScreenerIT_DATA_2025-06-02_1322.csv"))
ENTER_PV_web <- ENTER_PV_web %>% subset(select = c(
  enterid, pq16_1, pq16_1_distress, pq16_2, pq16_2_distress,
  pq16_3, pq16_3_distress, pq16_4, pq16_4_distress,
  pq16_5, pq16_5_distress, pq16_6, pq16_6_distress,
  pq16_7, pq16_7_distress, pq16_8, pq16_8_distress,
  pq16_9, pq16_9_distress, pq16_10, pq16_10_distress,
  pq16_11, pq16_11_distress, pq16_12, pq16_12_distress,
  pq16_13, pq16_13_distress, pq16_14, pq16_14_distress,
  pq16_15, pq16_15_distress, pq16_16, pq16_16_distress,
  pq16_score,
  pps_genetic_risk___1, pps_genetic_risk___2, pps_genetic_risk___3,
  pps_genetic_risk___4, pps_genetic_risk___5, pps_genetic_risk___6,
  pps_yeti_q1, pps_yeti_q2, pps_yeti_q3, pps_yeti_q4,
  pps_yeti_q5, pps_yeti_q6, pps_yeti_q7, pps_yeti_q8,
  pps_yeti_q9, pps_yeti_q10, pps_yeti_q11, pps_yeti_q12,
  pps_yeti_q13, pps_yeti_q14, pps_yeti_q15, pps_yeti_q16,
  pps_yeti_q17, pps_yeti_q18, pps_yeti_q19, pps_yeti_q20,
  pps_yeti_q21, pps_yeti_q22, pps_yeti_q23, pps_yeti_q24,
  pps_yeti_q25, pps_yeti_q26,
  dsst
))
valid_PV <- read_csv("Italy validations 22.03.25.csv")
valid_PV <- valid_PV %>% filter(manual_validation == "Valid") %>% mutate(enter_id = as.character(enter_id))
ENTER_PV_web <- ENTER_PV_web %>% mutate(enterid = as.character(enterid)) %>% filter(enterid %in% valid_PV$enter_id) %>% deduplicate_most_complete()

ENTER_PPS_PV <- read_csv(file.path(ENTER_WORK_ROOT, "PPS/ENTER/ENTER_PPS_Processing_IT/PPS_ENTER_IT_clean_190626.csv"))
ENTER_PPS_PV <- ENTER_PPS_PV %>% mutate(enterid = as.character(enterid)) %>% deduplicate_most_complete()
ENTER_PV <- merge(ENTER_PV, ENTER_PV_web, by = "enterid", all.y = TRUE)
ENTER_PV <- merge(ENTER_PV, ENTER_PPS_PV, by = "enterid", all.x = TRUE)

ENTER <- rbind(ENTER_UK, ENTER_PV)
# ENTER <- ENTER %>% dplyr::rename(chr = caarms_outcome_arms)
ENTER <- ENTER %>% mutate(
  chr = case_when(
    caarms_outcome_arms == 1 | caarms_outcome_psy == 1 ~ 1,
    caarms_outcome_arms == 0 & caarms_outcome_psy == 0 ~ 0,
    TRUE ~ NA
  ),
  family_history = case_when(
    pps_genetic_risk___1 == 1 | pps_genetic_risk___2 == 1 | pps_genetic_risk___3 == 1 | pps_genetic_risk___4 == 1 ~ 1,
    pps_genetic_risk___5 == 1 ~ 0,
    pps_genetic_risk___6 == 1 ~ NA_real_
  )
)
# ENTER$pq_16_distress <- rowSums(ENTER[,c("pq16_1_distress","pq16_2_distress","pq16_3_distress","pq16_4_distress","pq16_5_distress","pq16_6_distress","pq16_7_distress","pq16_8_distress","pq16_9_distress","pq16_10_distress","pq16_11_distress","pq16_12_distress","pq16_13_distress","pq16_14_distress", "pq16_15_distress","pq16_16_distress")], na.rm = TRUE)
# ENTER <- ENTER %>% subset(select = c(pq16_score, pq_16_distress, dsst, chr))
ENTER <- ENTER %>% subset(select = c(
  # -pq16_1, -pq16_1_distress, -pq16_2, -pq16_2_distress,
  # -pq16_3, -pq16_3_distress, -pq16_4, -pq16_4_distress,
  # -pq16_5, -pq16_5_distress, -pq16_6, -pq16_6_distress,
  # -pq16_7, -pq16_7_distress, -pq16_8, -pq16_8_distress,
  # -pq16_9, -pq16_9_distress, -pq16_10, -pq16_10_distress,
  # -pq16_11, -pq16_11_distress, -pq16_12, -pq16_12_distress,
  # -pq16_13, -pq16_13_distress, -pq16_14, -pq16_14_distress,
  # -pq16_15, -pq16_15_distress, -pq16_16, -pq16_16_distress,
  -pps_genetic_risk___1, -pps_genetic_risk___2, -pps_genetic_risk___3,
  -pps_genetic_risk___4, -pps_genetic_risk___5, -pps_genetic_risk___6,
  -pps_yeti_q1, -pps_yeti_q2, -pps_yeti_q3, -pps_yeti_q4,
  -pps_yeti_q5, -pps_yeti_q6, -pps_yeti_q7, -pps_yeti_q8,
  -pps_yeti_q9, -pps_yeti_q10, -pps_yeti_q11, -pps_yeti_q12,
  -pps_yeti_q13, -pps_yeti_q14, -pps_yeti_q15, -pps_yeti_q16,
  -pps_yeti_q17, -pps_yeti_q18, -pps_yeti_q19, -pps_yeti_q20,
  -pps_yeti_q21, -pps_yeti_q22, -pps_yeti_q23, -pps_yeti_q24,
  -pps_yeti_q25, -pps_yeti_q26,
  -caarms_outcome_arms, -caarms_outcome_psy
))
# ENTER <- ENTER %>% mutate(chr = case_when(
#   is.na(chr) ~ 0,
#   TRUE ~ chr
# ))
# ENTER <- ENTER[complete.cases(ENTER), ]
# ENTER <- ENTER %>% filter(!is.na(chr))

# helper function
boot_summary <- function(x) {
  q <- quantile(x, c(0.025, 0.975), na.rm = TRUE)
  c(
    Mean = mean(x, na.rm = TRUE),
    CI_low = unname(q[1]),
    CI_high = unname(q[2])
  )
}

# helper to get a metric row from summary df
get_metric <- function(df, metric_name) {
  row <- df[df$Metric == metric_name, ]
  if (nrow(row) == 0) {
    return(NULL)
  }
  list(mean = row$Mean, low = row$CI_low, high = row$CI_high)
}

# helper formatters
fmt_num <- function(mean, low, high, digits = 3) {
  paste0(
    formatC(mean, format = "f", digits = digits), " (",
    formatC(low, format = "f", digits = digits), "-",
    formatC(high, format = "f", digits = digits), ")"
  )
}

fmt_pct <- function(mean, low, high, digits_mean = 1, digits_ci = 1) {
  paste0(
    formatC(mean * 100, format = "f", digits = digits_mean), "% (",
    formatC(low * 100, format = "f", digits = digits_ci), "%-",
    formatC(high * 100, format = "f", digits = digits_ci), "%)"
  )
}

fmt_num_point <- function(value, digits = 3) {
  formatC(value, format = "f", digits = digits)
}

fmt_pct_point <- function(value, digits = 1) {
  paste0(formatC(value * 100, format = "f", digits = digits), "%")
}

summarise_dca_output <- function(dca_df) {
  dca_df %>%
    dplyr::mutate(
      curve = dplyr::case_when(
        grepl("^pred$", label, ignore.case = TRUE) ~ model,
        grepl("all", label, ignore.case = TRUE) ~ "Treat all",
        grepl("none", label, ignore.case = TRUE) ~ "Treat none",
        TRUE ~ label
      )
    ) %>%
    dplyr::group_by(curve, threshold) %>%
    dplyr::summarise(net_benefit = mean(net_benefit, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(model = curve)
}

plot_decision_curve_summary <- function(decision_curve_df, title_text, subtitle_text) {
  has_multiple_learners <- any(grepl("__final_", decision_curve_df$model))
  base_model_labels <- c(
    Step_1_pq16_score = "PQ-16 Score",
    Step_1b_pq16_cutoff = "PQ-16 >= 6",
    Step_2_pq_distress = "PQ-16 Distress",
    Step_2b_pq16_score_distress = "PQ-16 Score + Distress",
    Step_3_dsst = "DSST",
    Step_4_demographic_clinical = "PPS",
    Step_5_full_model = "All"
  )

  if (has_multiple_learners) {
    default_df <- decision_curve_df %>%
      dplyr::filter(model %in% c("Treat all", "Treat none"))

    model_df <- decision_curve_df %>%
      dplyr::filter(!model %in% c("Treat all", "Treat none"))

    default_expanded <- tidyr::crossing(
      default_df,
      base_model = names(model_steps)
    )

    plot_df <- dplyr::bind_rows(
      model_df %>% dplyr::mutate(base_model = strip_final_model_suffix(model)),
      default_expanded
    ) %>%
      dplyr::mutate(
        learner = dplyr::case_when(
          model == "Treat all" ~ "Assess all",
          model == "Treat none" ~ "Assess none",
          grepl("__final_glmnet$", model) ~ "Penalised logistic regression",
          grepl("__final_rf$", model) ~ "Random forest",
          grepl("__final_xgb$", model) ~ "XGBoost",
          TRUE ~ model
        )
      )

    plot_df$base_model <- factor(
      plot_df$base_model,
      levels = names(model_steps),
      labels = unname(base_model_labels[names(model_steps)])
    )

    plot_df$learner <- factor(
      plot_df$learner,
      levels = c(
        "Penalised logistic regression",
        "Random forest",
        "XGBoost",
        "Assess all",
        "Assess none"
      )
    )

    return(
      ggplot(
        plot_df,
        aes(x = threshold, y = net_benefit, colour = learner, linetype = learner)
      ) +
        geom_line(linewidth = 0.9) +
        geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.4) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_colour_manual(
          values = c(
            `Penalised logistic regression` = "#4E79A7",
            `Random forest` = "#59A14F",
            XGBoost = "#E15759",
            `Assess all` = "#000000",
            `Assess none` = "#555555"
          ),
          drop = FALSE
        ) +
        scale_linetype_manual(
          values = c(
            `Penalised logistic regression` = "solid",
            `Random forest` = "solid",
            XGBoost = "solid",
            `Assess all` = "solid",
            `Assess none` = "solid"
          ),
          drop = FALSE
        ) +
        coord_cartesian(xlim = c(0.01, 0.50)) +
        facet_wrap(~ base_model, scales = "fixed", ncol = 2) +
        labs(
          title = title_text,
          subtitle = subtitle_text,
          x = "Threshold probability",
          y = "Net benefit",
          colour = NULL,
          linetype = NULL
        ) +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.minor = element_blank(),
          legend.position = "bottom"
        )
    )
  }

  decision_curve_df <- decision_curve_df %>%
    dplyr::mutate(
      model = dplyr::recode(
        model,
        `Treat all` = "Assess all",
        `Treat none` = "Assess none"
      )
    )

  decision_curve_df$model <- factor(
    decision_curve_df$model,
    levels = c(names(model_steps), "Assess all", "Assess none"),
    labels = c(unname(base_model_labels[names(model_steps)]), "Assess all", "Assess none")
  )

  ggplot(
    decision_curve_df,
    aes(x = threshold, y = net_benefit, colour = model, linetype = model)
  ) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.4) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_colour_manual(
      values = c(
        `PQ-16 Score` = "#4E79A7",
        `PQ-16 >= 6` = "#B07AA1",
        `PQ-16 Distress` = "#F28E2B",
        DSST = "#E15759",
        PPS = "#76B7B2",
        All = "#59A14F",
        `Assess all` = "#000000",
        `Assess none` = "#555555"
      ),
      drop = FALSE
    ) +
    scale_linetype_manual(
      values = c(
        `PQ-16 Score` = "solid",
        `PQ-16 >= 6` = "solid",
        `PQ-16 Distress` = "solid",
        DSST = "solid",
        PPS = "solid",
        All = "solid",
        `Assess all` = "solid",
        `Assess none` = "solid"
      ),
      drop = FALSE
    ) +
    coord_cartesian(xlim = c(0.01, 0.50)) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "Threshold probability",
      y = "Net benefit",
      colour = NULL,
      linetype = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

summarise_pooled_coefficients <- function(coef_df, near_zero_threshold = 1e-06) {
  coef_df %>%
    dplyr::group_by(model, Feature) %>%
    dplyr::summarise(
      pooled_coefficient = median(Coefficient, na.rm = TRUE),
      mean_coefficient = mean(Coefficient, na.rm = TRUE),
      sd_coefficient = sd(Coefficient, na.rm = TRUE),
      ci_low = quantile(Coefficient, 0.025, na.rm = TRUE),
      ci_high = quantile(Coefficient, 0.975, na.rm = TRUE),
      sign_consistency = max(
        mean(Coefficient >= 0, na.rm = TRUE),
        mean(Coefficient <= 0, na.rm = TRUE)
      ),
      proportion_near_zero = mean(abs(Coefficient) <= near_zero_threshold, na.rm = TRUE),
      n_fits = dplyr::n(),
      .groups = "drop"
    )
}

summarise_pooled_importance <- function(importance_df, near_zero_threshold = 1e-06) {
  if (nrow(importance_df) == 0) {
    return(data.frame())
  }

  importance_df %>%
    dplyr::group_by(model, final_model_family, ImportanceMetric, Feature) %>%
    dplyr::summarise(
      pooled_importance = median(ImportanceValue, na.rm = TRUE),
      mean_importance = mean(ImportanceValue, na.rm = TRUE),
      sd_importance = sd(ImportanceValue, na.rm = TRUE),
      ci_low = quantile(ImportanceValue, 0.025, na.rm = TRUE),
      ci_high = quantile(ImportanceValue, 0.975, na.rm = TRUE),
      proportion_near_zero = mean(abs(ImportanceValue) <= near_zero_threshold, na.rm = TRUE),
      n_fits = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::arrange(model, final_model_family, ImportanceMetric, dplyr::desc(pooled_importance), Feature)
}

strip_final_model_suffix <- function(model_name) {
  sub("__final_.*$", "", model_name)
}

create_harmonised_model_summary <- function(pooled_coefficients, pooled_importance) {
  coeff_table <- if (nrow(pooled_coefficients) > 0) {
    pooled_coefficients %>%
      dplyr::transmute(
        base_model = strip_final_model_suffix(model),
        Feature,
        glmnet_pooled_coefficient = pooled_coefficient,
        glmnet_ci_low = ci_low,
        glmnet_ci_high = ci_high,
        glmnet_sign_consistency = sign_consistency,
        glmnet_proportion_near_zero = proportion_near_zero,
        glmnet_n_fits = n_fits
      )
  } else {
    data.frame()
  }

  rf_table <- if (nrow(pooled_importance) > 0) {
    pooled_importance %>%
      dplyr::filter(final_model_family == "rf", ImportanceMetric == "importance") %>%
      dplyr::group_by(base_model = strip_final_model_suffix(model)) %>%
      dplyr::arrange(dplyr::desc(pooled_importance), .by_group = TRUE) %>%
      dplyr::mutate(rf_rank = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
        base_model,
        Feature,
        rf_pooled_importance = pooled_importance,
        rf_ci_low = ci_low,
        rf_ci_high = ci_high,
        rf_rank,
        rf_proportion_near_zero = proportion_near_zero,
        rf_n_fits = n_fits
      )
  } else {
    data.frame()
  }

  xgb_table <- if (nrow(pooled_importance) > 0) {
    pooled_importance %>%
      dplyr::filter(final_model_family == "xgb") %>%
      dplyr::mutate(
        base_model = strip_final_model_suffix(model),
        metric_label = tolower(ImportanceMetric)
      ) %>%
      dplyr::select(base_model, Feature, metric_label, pooled_importance, ci_low, ci_high, proportion_near_zero, n_fits) %>%
      tidyr::pivot_wider(
        names_from = metric_label,
        values_from = c(pooled_importance, ci_low, ci_high, proportion_near_zero, n_fits),
        names_glue = "xgb_{metric_label}_{.value}"
      )
  } else {
    data.frame()
  }

  tables_to_join <- list(coeff_table, rf_table, xgb_table)
  tables_to_join <- tables_to_join[vapply(tables_to_join, nrow, integer(1)) > 0]

  if (length(tables_to_join) == 0) {
    return(data.frame())
  }

  output <- tables_to_join[[1]]
  if (length(tables_to_join) > 1) {
    for (table_idx in 2:length(tables_to_join)) {
      output <- dplyr::full_join(
        output,
        tables_to_join[[table_idx]],
        by = c("base_model", "Feature")
      )
    }
  }

  if (!"glmnet_pooled_coefficient" %in% names(output)) {
    output$glmnet_pooled_coefficient <- NA_real_
  }

  if (!"rf_pooled_importance" %in% names(output)) {
    output$rf_pooled_importance <- NA_real_
  }

  output %>%
    dplyr::arrange(base_model, dplyr::desc(abs(glmnet_pooled_coefficient)), dplyr::desc(rf_pooled_importance), Feature)
}

plot_ranked_model_importance <- function(pooled_importance, top_n = 20) {
  if (nrow(pooled_importance) == 0) {
    return(NULL)
  }

  plot_df <- pooled_importance %>%
    dplyr::filter(
      (final_model_family == "rf" & ImportanceMetric == "importance") |
      (final_model_family == "xgb" & ImportanceMetric == "Gain")
    ) %>%
    dplyr::mutate(
      base_model = strip_final_model_suffix(model),
      learner = dplyr::case_when(
        final_model_family == "rf" ~ "Random forest",
        final_model_family == "xgb" ~ "XGBoost",
        TRUE ~ final_model_family
      )
    ) %>%
    dplyr::group_by(base_model, learner) %>%
    dplyr::arrange(dplyr::desc(pooled_importance), .by_group = TRUE) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::mutate(
      Feature = factor(Feature, levels = rev(unique(Feature)))
    ) %>%
    dplyr::ungroup()

  if (nrow(plot_df) == 0) {
    return(NULL)
  }

  ggplot(
    plot_df,
    aes(x = pooled_importance, y = Feature, xmin = ci_low, xmax = ci_high, colour = learner)
  ) +
    geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.4) +
    geom_errorbarh(height = 0.2, alpha = 0.7) +
    geom_point(size = 2) +
    facet_grid(base_model ~ learner, scales = "free_y", space = "free_y") +
    scale_colour_manual(values = c("Random forest" = "#2C7FB8", "XGBoost" = "#D95F0E")) +
    labs(
      title = "Ranked Variable Importance",
      subtitle = "Random forest permutation importance and XGBoost gain",
      x = "Pooled importance",
      y = NULL,
      colour = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      strip.text.y = element_text(angle = 0)
    )
}

score_pooled_implementation <- function(test_eval_data, pooled_coef_model) {
  predictor_cols <- setdiff(
    names(test_eval_data),
    c("chr", "model", "Subject_ID", "OuterRepeat", "Fold", "PI", "chr_pred", "chr_num")
  )
  scoring_x <- test_eval_data[, predictor_cols, drop = FALSE]
  scoring_mm <- model.matrix(~ ., data = scoring_x)

  coef_lookup <- pooled_coef_model$pooled_coefficient
  names(coef_lookup) <- pooled_coef_model$Feature

  missing_coef_cols <- setdiff(names(coef_lookup), colnames(scoring_mm))
  if (length(missing_coef_cols) > 0) {
    zero_block <- matrix(0, nrow = nrow(scoring_mm), ncol = length(missing_coef_cols))
    colnames(zero_block) <- missing_coef_cols
    scoring_mm <- cbind(scoring_mm, zero_block)
  }

  scoring_mm <- scoring_mm[, names(coef_lookup), drop = FALSE]
  linear_predictor <- as.numeric(scoring_mm %*% coef_lookup)
  probabilities <- plogis(linear_predictor)

  scored_df <- test_eval_data %>%
    dplyr::select(model, Subject_ID, OuterRepeat, Fold, chr) %>%
    dplyr::mutate(
      pooled_PI = probabilities,
      pooled_chr_pred = factor(
        ifelse(probabilities >= 0.5, "X1", "X0"),
        levels = c("X0", "X1")
      )
    )

  cm <- confusionMatrix(
    data = scored_df$pooled_chr_pred,
    reference = scored_df$chr,
    positive = "X1"
  )

  scored_df <- scored_df %>%
    dplyr::mutate(
      pooled_PI = dplyr::case_when(
        pooled_PI == 0 ~ 0.001,
        pooled_PI == 1 ~ 0.999,
        TRUE ~ pooled_PI
      ),
      chr_num = as.numeric(chr) - 1
    )

  cal <- pred_val_probs(
    binary_outcome = scored_df$chr_num,
    Prob = scored_df$pooled_PI,
    cal_plot = FALSE
  )

  pooled_glm <- glm(chr ~ pooled_PI, data = scored_df, family = "binomial")

  list(
    scored_df = scored_df,
    performance = data.frame(
      model = unique(test_eval_data$model),
      C = concordance(pooled_glm)$concordance,
      balanced_accuracy = unname(cm$byClass["Balanced Accuracy"]),
      sensitivity = unname(cm$byClass["Sensitivity"]),
      specificity = unname(cm$byClass["Specificity"]),
      ppv = unname(cm$byClass["Pos Pred Value"]),
      npv = unname(cm$byClass["Neg Pred Value"]),
      brier = cal$BrierScore[1],
      calibration_intercept = cal$CalInt[1],
      calibration_slope = cal$CalSlope[1],
      stringsAsFactors = FALSE
    )
  )
}


cindex_summary <- function(data, lev = NULL, model = NULL) {
  # Convert factors to numeric (0 and 1) for concordance calculation
  obs_numeric <- ifelse(data$obs == lev[1], 0, 1)
  event_level <- lev[2]
  pred_numeric <- if (!is.null(event_level) && event_level %in% names(data)) {
    as.numeric(data[[event_level]])
  } else {
    as.numeric(data$pred)
  }

  # Calculate Harrell's C-index using predicted probabilities
  cindex <- Hmisc::rcorr.cens(pred_numeric, obs_numeric)[2]
  out <- c(C = cindex)
  names(out) <- "C"
  out
}

ENTER <- ENTER %>% mutate(
  gender = as.factor(gender),
  ethnicity = as.factor(ethnicity),
  ethnic_density = as.factor(ethnic_density),
  dsst = as.numeric(dsst),
  pq_distress = rowSums(select(., starts_with("pq16_") & ends_with("_distress")), na.rm = TRUE),
  pq_cutoff = case_when(
    pq16_score > 5 ~ 1,
    TRUE ~ 0
  ),
  paternal_age_35 = case_when(
    paternal_age >= 35 ~ 1,
    TRUE ~ 0
  ),
  paternal_age_45 = case_when(
    paternal_age >= 45 ~ 1,
    TRUE ~ 0
  ),
  childhood_trauma_cutoff = case_when(
    childhood_trauma >= 56 ~ 1,
    TRUE ~ 0
  ),
  anhedonia_cutoff = case_when(
    anhedonia >= 35 ~ 1,
    TRUE ~ 0
  ),
  chr_label = case_when(
    is.na(chr) ~ "Unlabelled",
    chr == 1 ~ "CHR-P",
    chr == 0 ~ "Control"
  )
)

outerFolds <- 5
outerRepeats <- 10
innerFolds <- 5
innerRepeats <- 10
max_ssl_iterations <- 10
ssl_tail_fraction <- 0.05
ssl_min_positive_prob <- NA_real_
ssl_max_negative_prob <- 0.30
ssl_pseudolabel_weight_cap_ratio <- 3.0
ssl_class_balanced_tails <- TRUE
ssl_calibrate_pseudolabel_probs <- FALSE
ssl_base_family <- "glmnet"
ssl_tail_fraction_candidates <- c(0.05, 0.10, 0.15, 0.20)
ssl_max_negative_prob_candidates <- c(0.30, 0.40, 0.50)
ssl_pseudolabel_weight_cap_ratio_candidates <- c(1.0, 1.5, 2.0, 2.5, 3.0)
final_model_families <- c("glmnet")
seed <- 231

demographic_clinical_predictors <- c(
  "age", "gender", "ethnicity",
  "family_history", "ethnic_density", "pollution",
  "high_urbanicity", "first_gen_migrant", "second_gen_migrant",
  "north_african_first_gen_migrant", "north_african_second_gen_migrant",
  "paternal_age_35", "paternal_age_45",
  "paternal_ses", "parent_smi", "adult_life_events",
  "daily_tobacco", "heavy_cannabis_use", "heavy_alcohol_use",
  "past_month_stimulants", "past_month_mdma", "past_month_ketamine",
  "past_month_heroin", "past_month_benzos", "past_month_nitrous",
  "past_month_ghb", "past_month_synth_cannabinoids",
  "past_month_psychedelics",
  "emotional_abuse", "physical_abuse", "sexual_abuse",
  "emotional_neglect", "physical_neglect",
  "neighbourhood_cohesion", "neighbourhood_social_disorganisation",
  "anhedonia"
)

use_previous_predictor_set <- tolower(Sys.getenv("USE_PREVIOUS_PREDICTOR_SET", "false")) == "true"

if (use_previous_predictor_set) {
  expanded_pps_predictors <- c(
    "daily_tobacco", "heavy_cannabis_use", "heavy_alcohol_use",
    "past_month_stimulants", "past_month_mdma", "past_month_ketamine",
    "past_month_heroin", "past_month_benzos", "past_month_nitrous",
    "past_month_ghb", "past_month_synth_cannabinoids",
    "past_month_psychedelics",
    "emotional_abuse", "physical_abuse", "sexual_abuse",
    "emotional_neglect", "physical_neglect",
    "neighbourhood_cohesion", "neighbourhood_social_disorganisation"
  )
  demographic_clinical_predictors <- setdiff(
    demographic_clinical_predictors,
    expanded_pps_predictors
  )
}

model_steps <- list(
  Step_1_pq16_score = c("pq16_score"),
  Step_1b_pq16_cutoff = c("pq_cutoff"),
  Step_2_pq_distress = c("pq_distress"),
  Step_2b_pq16_score_distress = c("pq16_score", "pq_distress"),
  Step_3_dsst = c("dsst"),
  Step_4_demographic_clinical = demographic_clinical_predictors,
  Step_5_full_model = c(
    "pq16_score", "pq_distress", "dsst",
    demographic_clinical_predictors
  )
)

sensitivity_base_predictors <- model_steps$Step_5_full_model
sensitivity_speech_predictors <- c("Coherence", "PCSYNz")

if (!exists("results_summary")) {
  results_summary <- data.frame()
}

run_main_steps <- tolower(Sys.getenv("RUN_MAIN_STEPS", "true")) != "false"
run_sensitivity_steps <- tolower(Sys.getenv("RUN_SENSITIVITY_STEPS", "false")) != "false"
run_bootstrap <- tolower(Sys.getenv("RUN_BOOTSTRAP", "true")) != "false"
run_ssl_tuning <- tolower(Sys.getenv("RUN_SSL_TUNING", "false")) == "true"
run_near_zero_variance_filter <- tolower(Sys.getenv("RUN_NZV_FILTER", "false")) == "true"

parse_numeric_env_list <- function(env_name, default_values) {
  raw_value <- Sys.getenv(env_name, "")

  if (!nzchar(raw_value)) {
    return(default_values)
  }

  parsed_values <- suppressWarnings(as.numeric(trimws(unlist(strsplit(raw_value, ",")))))
  parsed_values <- parsed_values[!is.na(parsed_values)]

  if (length(parsed_values) == 0) {
    default_values
  } else {
    unique(parsed_values)
  }
}

parse_character_env_list <- function(env_name, default_values) {
  raw_value <- Sys.getenv(env_name, "")

  if (!nzchar(raw_value)) {
    return(default_values)
  }

  parsed_values <- trimws(unlist(strsplit(raw_value, ",")))
  parsed_values <- parsed_values[nzchar(parsed_values)]

  if (length(parsed_values) == 0) {
    default_values
  } else {
    unique(parsed_values)
  }
}

outer_repeats_env <- suppressWarnings(as.integer(Sys.getenv("OUTER_REPEATS", as.character(outerRepeats))))
inner_repeats_env <- suppressWarnings(as.integer(Sys.getenv("INNER_REPEATS", as.character(innerRepeats))))
max_ssl_iterations_env <- suppressWarnings(as.integer(Sys.getenv("MAX_SSL_ITERATIONS", as.character(max_ssl_iterations))))
ssl_max_negative_prob_env_raw <- Sys.getenv("SSL_MAX_NEGATIVE_PROB", "")
ssl_max_negative_prob_env <- if (nzchar(ssl_max_negative_prob_env_raw)) suppressWarnings(as.numeric(ssl_max_negative_prob_env_raw)) else ssl_max_negative_prob
ssl_pseudolabel_weight_cap_ratio_env <- suppressWarnings(as.numeric(Sys.getenv("SSL_PSEUDOLABEL_WEIGHT_CAP_RATIO", as.character(ssl_pseudolabel_weight_cap_ratio))))
ssl_class_balanced_tails <- tolower(Sys.getenv("SSL_CLASS_BALANCED_TAILS", as.character(ssl_class_balanced_tails))) == "true"
ssl_calibrate_pseudolabel_probs <- tolower(Sys.getenv("SSL_CALIBRATE_PSEUDOLABEL_PROBS", as.character(ssl_calibrate_pseudolabel_probs))) == "true"
ssl_base_family <- tolower(Sys.getenv("SSL_BASE_FAMILY", ssl_base_family))
ssl_tail_fraction_candidates <- parse_numeric_env_list("SSL_TAIL_FRACTION_CANDIDATES", ssl_tail_fraction_candidates)
ssl_max_negative_prob_candidates <- parse_numeric_env_list("SSL_MAX_NEGATIVE_PROB_CANDIDATES", ssl_max_negative_prob_candidates)
ssl_pseudolabel_weight_cap_ratio_candidates <- parse_numeric_env_list("SSL_CAP_RATIO_CANDIDATES", ssl_pseudolabel_weight_cap_ratio_candidates)
final_model_families <- parse_character_env_list("FINAL_MODEL_FAMILIES", final_model_families)
glmnet_alpha_values <- parse_numeric_env_list("GLMNET_ALPHA_VALUES", c(1))
rf_min_node_size_values <- parse_numeric_env_list("RF_MIN_NODE_SIZE_VALUES", c(1, 5, 10, 20))
rf_max_depth_values <- parse_numeric_env_list("RF_MAX_DEPTH_VALUES", c(0))
rf_mtry_values_override <- parse_numeric_env_list("RF_MTRY_VALUES", numeric(0))
rf_constrained_mtry <- tolower(Sys.getenv("RF_CONSTRAINED_MTRY", "false")) == "true"
use_one_se_selection <- tolower(Sys.getenv("USE_ONE_SE_SELECTION", "false")) == "true"
xgb_nrounds_values <- parse_numeric_env_list("XGB_NROUNDS_VALUES", c(50, 100, 200))
xgb_max_depth_values <- parse_numeric_env_list("XGB_MAX_DEPTH_VALUES", c(1, 2, 3))
xgb_eta_values <- parse_numeric_env_list("XGB_ETA_VALUES", c(0.01, 0.03, 0.05))
xgb_gamma_values <- parse_numeric_env_list("XGB_GAMMA_VALUES", c(0, 1, 5))
xgb_colsample_bytree_values <- parse_numeric_env_list("XGB_COLSAMPLE_BYTREE_VALUES", c(0.4, 0.6, 0.8))
xgb_min_child_weight_values <- parse_numeric_env_list("XGB_MIN_CHILD_WEIGHT_VALUES", c(5, 10, 20))
xgb_subsample_values <- parse_numeric_env_list("XGB_SUBSAMPLE_VALUES", c(0.6, 0.8))
target_models_env <- Sys.getenv("TARGET_MODELS", "")
target_models <- if (nzchar(target_models_env)) trimws(unlist(strsplit(target_models_env, ","))) else character(0)
saved_results_path <- file.path(output_dir, "results_summary.csv")
predictions_output_path <- file.path(output_dir, "test_set_predictions.csv")
decision_curve_general_output_path <- file.path(output_dir, "decision_curve_general_population.csv")
decision_curve_clinical_output_path <- file.path(output_dir, "decision_curve_clinical_sample.csv")
decision_curve_general_plot_path <- file.path(output_dir, "decision_curve_general_population.png")
decision_curve_clinical_plot_path <- file.path(output_dir, "decision_curve_clinical_sample.png")
ssl_diagnostics_output_path <- file.path(output_dir, "ssl_training_diagnostics.csv")
ssl_iterations_output_path <- file.path(output_dir, "ssl_iteration_history.csv")
coefficient_output_path <- file.path(output_dir, "semi_supervised_coefficients_by_fit.csv")
pooled_coefficient_output_path <- file.path(output_dir, "pooled_implementation_coefficients.csv")
pooled_performance_output_path <- file.path(output_dir, "pooled_implementation_performance.csv")
importance_output_path <- file.path(output_dir, "model_variable_importance_by_fit.csv")
pooled_importance_output_path <- file.path(output_dir, "pooled_model_variable_importance.csv")
tree_complexity_output_path <- file.path(output_dir, "tree_complexity_by_fit.csv")
training_performance_output_path <- file.path(output_dir, "training_performance_by_fit.csv")
harmonised_model_summary_output_path <- file.path(output_dir, "harmonised_model_summary_table.csv")
importance_plot_output_path <- file.path(output_dir, "model_importance_ranked.png")
ssl_prediction_distribution_output_path <- file.path(output_dir, "ssl_prediction_distribution.csv")
ssl_tuning_output_path <- file.path(output_dir, "ssl_parameter_search_results.csv")

if (!is.na(outer_repeats_env) && outer_repeats_env >= 1) {
  outerRepeats <- outer_repeats_env
}
if (!is.na(inner_repeats_env) && inner_repeats_env >= 1) {
  innerRepeats <- inner_repeats_env
}
if (!is.na(max_ssl_iterations_env) && max_ssl_iterations_env >= 1) {
  max_ssl_iterations <- max_ssl_iterations_env
}
if (!is.na(ssl_max_negative_prob_env)) {
  ssl_max_negative_prob <- ssl_max_negative_prob_env
}

if (!is.na(ssl_pseudolabel_weight_cap_ratio_env) && ssl_pseudolabel_weight_cap_ratio_env > 0) {
  ssl_pseudolabel_weight_cap_ratio <- ssl_pseudolabel_weight_cap_ratio_env
}

if (nrow(results_summary) == 0 && file.exists(saved_results_path)) {
  results_summary <- read.csv(saved_results_path, stringsAsFactors = FALSE)
}

log_progress <- function(...) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s\n", timestamp, paste(..., collapse = "")))
  flush.console()
}

prepare_analysis_data <- function(data, predictors) {
  analysis_df <- data %>%
    dplyr::select(all_of(c(predictors, "chr")))

  factor_vars <- intersect(
    c("ethnicity", "gender", "ethnic_density"),
    names(analysis_df)
  )

  for (var in factor_vars) {
    analysis_df[[var]] <- factor(analysis_df[[var]])
  }

  if ("dsst" %in% names(analysis_df)) {
    analysis_df$dsst <- as.numeric(analysis_df$dsst)
  }

  analysis_df
}

prepare_labelled_analysis_data <- function(data, predictors) {
  analysis_df <- prepare_analysis_data(data, predictors) %>%
    filter(!is.na(chr))

  analysis_df$chr <- factor(analysis_df$chr)
  analysis_df$chr <- factor(make.names(levels(analysis_df$chr))[analysis_df$chr])

  analysis_df
}

build_glmnet_design_matrix <- function(predictor_data) {
  design_matrix <- model.matrix(~ ., data = predictor_data)

  if ("(Intercept)" %in% colnames(design_matrix)) {
    design_matrix <- design_matrix[, colnames(design_matrix) != "(Intercept)", drop = FALSE]
  }

  design_matrix
}

select_available_glmnet_lambda <- function(model_fit, requested_lambda) {
  available_lambda <- model_fit$lambda

  if (length(requested_lambda) == 0 || is.na(requested_lambda[1])) {
    if (length(available_lambda) == 0 || all(is.na(available_lambda))) {
      return(NA_real_)
    }

    return(available_lambda[1])
  }

  if (length(available_lambda) == 0 || all(is.na(available_lambda))) {
    return(requested_lambda[1])
  }

  available_lambda[which.min(abs(available_lambda - requested_lambda[1]))]
}

fit_direct_glmnet <- function(train_data, tune_grid, effective_inner_folds, observation_weights = NULL,
                              fixed_lambda = NULL, fixed_alpha = NULL) {
  train_x <- build_glmnet_design_matrix(train_data[, setdiff(names(train_data), "chr"), drop = FALSE])
  train_y_factor <- train_data$chr
  train_y <- as.numeric(train_y_factor == "X1")
  train_weights <- if (is.null(observation_weights)) rep(1, length(train_y)) else observation_weights
  tune_grid <- tune_grid[!is.na(tune_grid$lambda) & !is.na(tune_grid$alpha), , drop = FALSE]
  lambda_grid <- sort(unique(tune_grid$lambda), decreasing = TRUE)
  alpha_grid <- sort(unique(tune_grid$alpha))

  if (length(lambda_grid) == 0 || any(is.na(lambda_grid))) {
    stop("glmnet lambda grid must contain non-missing values")
  }
  if (length(alpha_grid) == 0 || any(is.na(alpha_grid))) {
    stop("glmnet alpha grid must contain non-missing values")
  }

  has_fixed_lambda <- !is.null(fixed_lambda) && length(fixed_lambda) > 0 && !is.na(fixed_lambda[1])
  has_fixed_alpha <- !is.null(fixed_alpha) && length(fixed_alpha) > 0 && !is.na(fixed_alpha[1])

  if (!has_fixed_lambda || !has_fixed_alpha) {
    inner_folds <- createFolds(
      train_y_factor,
      k = effective_inner_folds,
      list = TRUE,
      returnTrain = FALSE
    )
    tuning_pairs <- expand.grid(alpha = alpha_grid, lambda = lambda_grid)
    fold_predictions <- matrix(NA_real_, nrow = nrow(train_x), ncol = nrow(tuning_pairs))

    for (inner_fold in seq_along(inner_folds)) {
      validation_rows <- inner_folds[[inner_fold]]
      fitting_rows <- setdiff(seq_len(nrow(train_x)), validation_rows)

      for (alpha_value in alpha_grid) {
        fold_fit <- glmnet::glmnet(
          x = train_x[fitting_rows, , drop = FALSE],
          y = train_y[fitting_rows],
          weights = train_weights[fitting_rows],
          family = "binomial",
          alpha = alpha_value,
          lambda = lambda_grid
        )

        retained_lambda_pairs <- data.frame(
          lambda = fold_fit$lambda,
          grid_idx = vapply(
            fold_fit$lambda,
            function(lambda_value) {
              which(tuning_pairs$alpha == alpha_value)[which.min(abs(tuning_pairs$lambda[tuning_pairs$alpha == alpha_value] - lambda_value))]
            },
            integer(1)
          )
        )
        retained_lambda_pairs <- retained_lambda_pairs[!duplicated(retained_lambda_pairs$grid_idx), , drop = FALSE]

        if (nrow(retained_lambda_pairs) == 0) {
          next
        }

        fold_predictions[validation_rows, retained_lambda_pairs$grid_idx] <- as.matrix(predict(
          fold_fit,
          newx = train_x[validation_rows, , drop = FALSE],
          s = retained_lambda_pairs$lambda,
          type = "response"
        ))
      }
    }

    c_values <- apply(fold_predictions, 2, function(pred) {
      if (all(is.na(pred)) || length(unique(train_y[!is.na(pred)])) < 2) {
        return(NA_real_)
      }

      as.numeric(Hmisc::rcorr.cens(pred, train_y)[2])
    })

    if (all(is.na(c_values))) {
      best_lambda <- lambda_grid[1]
      best_alpha <- alpha_grid[1]
      best_c <- NA_real_
    } else {
      best_idx <- which.max(c_values)
      best_lambda <- tuning_pairs$lambda[best_idx]
      best_alpha <- tuning_pairs$alpha[best_idx]
      best_c <- max(c_values, na.rm = TRUE)
    }
  } else {
    best_lambda <- fixed_lambda[1]
    best_alpha <- fixed_alpha[1]
    best_c <- NA_real_
  }

  final_lambda_grid <- sort(unique(c(lambda_grid, best_lambda[!is.na(best_lambda)])), decreasing = TRUE)
  final_fit <- glmnet::glmnet(
    x = train_x,
    y = train_y,
    weights = train_weights,
    family = "binomial",
    alpha = best_alpha,
    lambda = final_lambda_grid
  )
  best_lambda <- select_available_glmnet_lambda(final_fit, best_lambda)

  list(
    finalModel = final_fit,
    bestTune = data.frame(alpha = best_alpha, lambda = best_lambda),
    best_lambda = best_lambda,
    best_alpha = best_alpha,
    best_c = best_c,
    xNames = colnames(train_x),
    obsLevels = levels(train_y_factor),
    method = "glmnet"
  )
}

predict_step_prob <- function(model_fit, newdata, use_single_predictor_model) {
  if (use_single_predictor_model) {
    if (inherits(model_fit, "train")) {
      prob_pred <- predict(model_fit, newdata = newdata, type = "prob")

      if ("X1" %in% colnames(prob_pred)) {
        return(as.numeric(prob_pred[, "X1"]))
      }

      return(as.numeric(prob_pred[, ncol(prob_pred)]))
    }

    as.numeric(predict(model_fit, newdata = newdata, type = "response"))
  } else {
    if (!is.null(model_fit$finalModel) && inherits(model_fit$finalModel, "glmnet")) {
      if (nrow(newdata) == 0) {
        return(numeric(0))
      }

      non_predictor_cols <- c("chr", "model", "Subject_ID", "OuterRepeat", "Fold", "PI", "chr_pred", "chr_num")
      predictor_cols <- setdiff(names(newdata), non_predictor_cols)
      predictor_data <- newdata[, predictor_cols, drop = FALSE]
      new_x <- build_glmnet_design_matrix(predictor_data)

      required_cols <- rownames(model_fit$finalModel$beta)
      missing_cols <- setdiff(required_cols, colnames(new_x))

      if (length(missing_cols) > 0) {
        zero_block <- matrix(0, nrow = nrow(new_x), ncol = length(missing_cols))
        colnames(zero_block) <- missing_cols
        new_x <- cbind(new_x, zero_block)
      }

      new_x <- new_x[, required_cols, drop = FALSE]
      prediction_lambda <- select_available_glmnet_lambda(
        model_fit$finalModel,
        model_fit$bestTune$lambda
      )

      return(as.numeric(predict(
        model_fit$finalModel,
        newx = new_x,
        s = prediction_lambda,
        type = "response"
      )))
    }

    as.numeric(predict(model_fit, newdata = newdata, type = "prob")[, 2])
  }
}

extract_step_coefficients <- function(model_fit, use_single_predictor_model, model_name, outer_rep, outer_fold,
                                      final_model_family = "glmnet") {
  final_model_family <- canonical_final_model_family(final_model_family)

  if (final_model_family != "glmnet") {
    return(data.frame())
  }

  if (use_single_predictor_model) {
    coef_vec <- coef(model_fit)
    coef_df <- data.frame(
      Feature = names(coef_vec),
      Coefficient = as.numeric(coef_vec),
      stringsAsFactors = FALSE
    )
  } else {
    best_model <- model_fit$finalModel
    best_lambda <- select_available_glmnet_lambda(best_model, model_fit$bestTune$lambda)
    coef_matrix <- as.matrix(coef(best_model, s = best_lambda))
    coef_df <- data.frame(
      Feature = rownames(coef_matrix),
      Coefficient = coef_matrix[, 1],
      stringsAsFactors = FALSE
    )
  }

  coef_df$model <- model_name
  coef_df$final_model_family <- final_model_family
  coef_df$OuterRepeat <- outer_rep
  coef_df$Fold <- outer_fold
  coef_df
}

extract_model_importance <- function(model_fit, model_name, outer_rep, outer_fold,
                                     final_model_family = "glmnet") {
  final_model_family <- canonical_final_model_family(final_model_family)

  if (final_model_family == "glmnet") {
    return(data.frame())
  }

  if (final_model_family == "rf") {
    if (inherits(model_fit, "train")) {
      importance_obj <- tryCatch(varImp(model_fit, scale = FALSE), error = function(e) NULL)

      if (!is.null(importance_obj) && !is.null(importance_obj$importance) &&
          nrow(importance_obj$importance) > 0) {
        importance_df <- data.frame(
          Feature = rownames(importance_obj$importance),
          ImportanceValue = importance_obj$importance[, 1],
          stringsAsFactors = FALSE
        )

        importance_df$model <- model_name
        importance_df$final_model_family <- final_model_family
        importance_df$ImportanceMetric <- "importance"
        importance_df$OuterRepeat <- outer_rep
        importance_df$Fold <- outer_fold

        return(importance_df %>%
          dplyr::select(model, final_model_family, ImportanceMetric, Feature, ImportanceValue, OuterRepeat, Fold))
      }
    }

    importance_vec <- model_fit$model$variable.importance

    if (is.null(importance_vec) || length(importance_vec) == 0) {
      return(data.frame())
    }

    return(data.frame(
      model = model_name,
      final_model_family = final_model_family,
      ImportanceMetric = "importance",
      Feature = names(importance_vec),
      ImportanceValue = as.numeric(importance_vec),
      OuterRepeat = outer_rep,
      Fold = outer_fold,
      stringsAsFactors = FALSE
    ))
  }

  if (final_model_family == "xgb") {
    xgb_model <- if (inherits(model_fit, "train")) model_fit$finalModel else model_fit$model

    importance_df <- tryCatch(
      xgboost::xgb.importance(model = xgb_model),
      error = function(e1) {
        tryCatch(
          xgboost::xgb.importance(
            feature_names = model_fit$feature_names,
            model = xgb_model
          ),
          error = function(e2) {
            warning(
              "Unable to extract XGBoost importance for ", model_name,
              " at outer repeat ", outer_rep,
              ", fold ", outer_fold,
              ": ", conditionMessage(e2)
            )
            data.frame()
          }
        )
      }
    )

    if (is.null(importance_df) || nrow(importance_df) == 0) {
      return(data.frame())
    }

    importance_long <- importance_df %>%
      dplyr::select(Feature, Gain, Cover, Frequency) %>%
      tidyr::pivot_longer(
        cols = c(Gain, Cover, Frequency),
        names_to = "ImportanceMetric",
        values_to = "ImportanceValue"
      )

    importance_long$model <- model_name
    importance_long$final_model_family <- final_model_family
    importance_long$OuterRepeat <- outer_rep
    importance_long$Fold <- outer_fold

    return(importance_long %>%
      dplyr::select(model, final_model_family, ImportanceMetric, Feature, ImportanceValue, OuterRepeat, Fold))
  }

  data.frame()
}

summarise_leaf_counts <- function(leaf_assignments, outcomes, observation_weights = NULL) {
  if (is.null(leaf_assignments) || length(leaf_assignments) == 0) {
    return(data.frame(
      mean_terminal_node_n = NA_real_,
      median_terminal_node_n = NA_real_,
      min_terminal_node_n = NA_real_,
      max_terminal_node_n = NA_real_,
      mean_terminal_node_events = NA_real_,
      mean_terminal_node_event_rate = NA_real_,
      mean_terminal_node_weight = NA_real_,
      mean_terminal_node_event_weight = NA_real_
    ))
  }

  if (is.null(dim(leaf_assignments))) {
    leaf_assignments <- matrix(leaf_assignments, ncol = 1)
  }

  if (is.null(observation_weights)) {
    observation_weights <- rep(1, length(outcomes))
  }

  outcome_events <- as.integer(outcomes == "X1")
  leaf_counts <- lapply(seq_len(ncol(leaf_assignments)), function(tree_idx) {
    split(seq_along(outcomes), leaf_assignments[, tree_idx]) |>
      lapply(function(idx) {
        data.frame(
          terminal_node_n = length(idx),
          terminal_node_events = sum(outcome_events[idx], na.rm = TRUE),
          terminal_node_weight = sum(observation_weights[idx], na.rm = TRUE),
          terminal_node_event_weight = sum(observation_weights[idx] * outcome_events[idx], na.rm = TRUE)
        )
      }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows()

  if (nrow(leaf_counts) == 0) {
    return(data.frame(
      mean_terminal_node_n = NA_real_,
      median_terminal_node_n = NA_real_,
      min_terminal_node_n = NA_real_,
      max_terminal_node_n = NA_real_,
      mean_terminal_node_events = NA_real_,
      mean_terminal_node_event_rate = NA_real_,
      mean_terminal_node_weight = NA_real_,
      mean_terminal_node_event_weight = NA_real_
    ))
  }

  leaf_counts |>
    dplyr::summarise(
      mean_terminal_node_n = mean(terminal_node_n, na.rm = TRUE),
      median_terminal_node_n = median(terminal_node_n, na.rm = TRUE),
      min_terminal_node_n = min(terminal_node_n, na.rm = TRUE),
      max_terminal_node_n = max(terminal_node_n, na.rm = TRUE),
      mean_terminal_node_events = mean(terminal_node_events, na.rm = TRUE),
      mean_terminal_node_event_rate = mean(terminal_node_events / terminal_node_n, na.rm = TRUE),
      mean_terminal_node_weight = mean(terminal_node_weight, na.rm = TRUE),
      mean_terminal_node_event_weight = mean(terminal_node_event_weight, na.rm = TRUE)
    )
}

summarise_xgb_leaf_depths <- function(tree_dt) {
  if (is.null(tree_dt) || nrow(tree_dt) == 0) {
    return(data.frame(
      mean_terminal_depth = NA_real_,
      median_terminal_depth = NA_real_,
      max_terminal_depth = NA_real_,
      mean_terminal_nodes_per_tree = NA_real_,
      n_trees = NA_integer_
    ))
  }

  tree_depths <- lapply(split(tree_dt, tree_dt$Tree), function(one_tree) {
    depths <- setNames(rep(NA_integer_, nrow(one_tree)), one_tree$ID)
    root_id <- one_tree$ID[one_tree$Node == 0][1]
    depths[root_id] <- 0L

    changed <- TRUE
    while (changed && any(is.na(depths))) {
      changed <- FALSE
      split_rows <- one_tree[one_tree$Feature != "Leaf", , drop = FALSE]
      for (row_idx in seq_len(nrow(split_rows))) {
        parent_id <- split_rows$ID[row_idx]
        parent_depth <- depths[parent_id]
        if (is.na(parent_depth)) {
          next
        }
        child_ids <- stats::na.omit(c(split_rows$Yes[row_idx], split_rows$No[row_idx]))
        for (child_id in child_ids) {
          if (child_id %in% names(depths) && is.na(depths[child_id])) {
            depths[child_id] <- parent_depth + 1L
            changed <- TRUE
          }
        }
      }
    }

    leaf_depths <- depths[one_tree$ID[one_tree$Feature == "Leaf"]]
    data.frame(
      terminal_depth = as.numeric(leaf_depths),
      tree = unique(one_tree$Tree),
      stringsAsFactors = FALSE
    )
  }) |>
    dplyr::bind_rows()

  tree_depths |>
    dplyr::summarise(
      mean_terminal_depth = mean(terminal_depth, na.rm = TRUE),
      median_terminal_depth = median(terminal_depth, na.rm = TRUE),
      max_terminal_depth = max(terminal_depth, na.rm = TRUE),
      mean_terminal_nodes_per_tree = dplyr::n() / dplyr::n_distinct(tree),
      n_trees = dplyr::n_distinct(tree)
    )
}

extract_tree_complexity <- function(model_fit, train_data, observation_weights,
                                    model_name, outer_rep, outer_fold,
                                    final_model_family = "glmnet") {
  final_model_family <- canonical_final_model_family(final_model_family)

  if (!final_model_family %in% c("rf", "xgb")) {
    return(data.frame())
  }

  if (final_model_family == "rf") {
    best_tune <- if (!is.null(model_fit$bestTune)) model_fit$bestTune else data.frame()
    ranger_model <- if (inherits(model_fit, "train")) {
      ranger::ranger(
        chr ~ .,
        data = train_data,
        case.weights = observation_weights,
        probability = TRUE,
        num.trees = 500,
        mtry = if ("mtry" %in% names(best_tune)) best_tune$mtry else NULL,
        min.node.size = if ("min.node.size" %in% names(best_tune)) best_tune$min.node.size else NULL,
        splitrule = if ("splitrule" %in% names(best_tune)) as.character(best_tune$splitrule) else "gini",
        importance = "permutation"
      )
    } else {
      model_fit$model
    }
    predictor_data <- train_data[, setdiff(names(train_data), "chr"), drop = FALSE]
    terminal_nodes <- predict(ranger_model, data = predictor_data, type = "terminalNodes")$predictions

    depth_by_tree <- lapply(seq_len(ranger_model$num.trees), function(tree_idx) {
      tree_info <- ranger::treeInfo(ranger_model, tree = tree_idx)
      terminal_rows <- is.na(tree_info$leftChild) & is.na(tree_info$rightChild)
      depths <- setNames(rep(NA_integer_, nrow(tree_info)), as.character(tree_info$nodeID))
      depths["0"] <- 0L

      changed <- TRUE
      while (changed && any(is.na(depths))) {
        changed <- FALSE
        split_rows <- tree_info[!terminal_rows, , drop = FALSE]
        for (row_idx in seq_len(nrow(split_rows))) {
          parent_id <- as.character(split_rows$nodeID[row_idx])
          parent_depth <- depths[parent_id]
          if (is.na(parent_depth)) {
            next
          }
          child_ids <- as.character(stats::na.omit(c(split_rows$leftChild[row_idx], split_rows$rightChild[row_idx])))
          for (child_id in child_ids) {
            if (child_id %in% names(depths) && is.na(depths[child_id])) {
              depths[child_id] <- parent_depth + 1L
              changed <- TRUE
            }
          }
        }
      }

      data.frame(
        terminal_depth = as.numeric(depths[as.character(tree_info$nodeID[terminal_rows])]),
        tree = tree_idx,
        stringsAsFactors = FALSE
      )
    }) |>
      dplyr::bind_rows()

    depth_summary <- depth_by_tree |>
      dplyr::summarise(
        mean_terminal_depth = mean(terminal_depth, na.rm = TRUE),
        median_terminal_depth = median(terminal_depth, na.rm = TRUE),
        max_terminal_depth = max(terminal_depth, na.rm = TRUE),
        mean_terminal_nodes_per_tree = dplyr::n() / dplyr::n_distinct(tree),
        n_trees = dplyr::n_distinct(tree)
      )

    leaf_summary <- summarise_leaf_counts(
      leaf_assignments = terminal_nodes,
      outcomes = train_data$chr,
      observation_weights = observation_weights
    )

    return(cbind(
      data.frame(
        model = model_name,
        final_model_family = final_model_family,
        OuterRepeat = outer_rep,
        Fold = outer_fold,
        selected_mtry = if ("mtry" %in% names(best_tune)) best_tune$mtry else NA_real_,
        selected_min_node_size = if ("min.node.size" %in% names(best_tune)) best_tune$min.node.size else NA_real_,
        selected_splitrule = if ("splitrule" %in% names(best_tune)) as.character(best_tune$splitrule) else NA_character_,
        selected_nrounds = NA_real_,
        selected_max_depth = if ("max.depth" %in% names(best_tune)) best_tune$max.depth else NA_real_,
        selected_eta = NA_real_,
        selected_gamma = NA_real_,
        selected_colsample_bytree = NA_real_,
        selected_min_child_weight = NA_real_,
        selected_subsample = NA_real_,
        stringsAsFactors = FALSE
      ),
      depth_summary,
      leaf_summary
    ))
  }

  xgb_model <- if (inherits(model_fit, "train")) model_fit$finalModel else model_fit$model
  predictor_cols <- setdiff(names(train_data), "chr")
  train_x <- model.matrix(~ . - 1, data = train_data[, predictor_cols, drop = FALSE])

  missing_cols <- setdiff(model_fit$feature_names, colnames(train_x))
  if (length(missing_cols) > 0) {
    zero_block <- matrix(0, nrow = nrow(train_x), ncol = length(missing_cols))
    colnames(zero_block) <- missing_cols
    train_x <- cbind(train_x, zero_block)
  }
  train_x <- train_x[, model_fit$feature_names, drop = FALSE]

  terminal_nodes <- predict(
    xgb_model,
    newdata = xgboost::xgb.DMatrix(train_x),
    predleaf = TRUE
  )
  tree_dt <- xgboost::xgb.model.dt.tree(
    feature_names = model_fit$feature_names,
    model = xgb_model
  )

  depth_summary <- summarise_xgb_leaf_depths(tree_dt)
  leaf_summary <- summarise_leaf_counts(
    leaf_assignments = terminal_nodes,
    outcomes = train_data$chr,
    observation_weights = observation_weights
  )
  best_tune <- model_fit$bestTune

  cbind(
    data.frame(
      model = model_name,
      final_model_family = final_model_family,
      OuterRepeat = outer_rep,
      Fold = outer_fold,
      selected_mtry = NA_real_,
      selected_min_node_size = NA_real_,
      selected_splitrule = NA_character_,
      selected_nrounds = if ("nrounds" %in% names(best_tune)) best_tune$nrounds else NA_real_,
      selected_max_depth = if ("max_depth" %in% names(best_tune)) best_tune$max_depth else NA_real_,
      selected_eta = if ("eta" %in% names(best_tune)) best_tune$eta else NA_real_,
      selected_gamma = if ("gamma" %in% names(best_tune)) best_tune$gamma else NA_real_,
      selected_colsample_bytree = if ("colsample_bytree" %in% names(best_tune)) best_tune$colsample_bytree else NA_real_,
      selected_min_child_weight = if ("min_child_weight" %in% names(best_tune)) best_tune$min_child_weight else NA_real_,
      selected_subsample = if ("subsample" %in% names(best_tune)) best_tune$subsample else NA_real_,
      stringsAsFactors = FALSE
    ),
    depth_summary,
    leaf_summary
  )
}

fit_step_model <- function(train_data, use_single_predictor_model, tune_grid, effective_inner_folds,
                           model_name, outer_rep, outer_fold, inner_rep = NULL, fixed_lambda = NULL,
                           fixed_alpha = NULL,
                           observation_weights = NULL) {
  if (use_single_predictor_model) {
    if (is.null(fixed_lambda)) {
      control <- trainControl(
        method = "cv",
        number = effective_inner_folds,
        summaryFunction = cindex_summary,
        classProbs = TRUE
      )

      model_fit <- train(
        chr ~ .,
        data = train_data,
        weights = observation_weights,
        method = "glm",
        family = "binomial",
        trControl = control,
        metric = "C"
      )

      return(list(
        model_fit = model_fit,
        best_alpha = NA_real_,
        best_lambda = NA_real_,
        best_c = max(model_fit$results$C, na.rm = TRUE)
      ))
    }

    model_fit <- glm(chr ~ ., data = train_data, family = "binomial", weights = observation_weights)
    return(list(
      model_fit = model_fit,
      best_alpha = NA_real_,
      best_lambda = NA_real_,
      best_c = NA_real_
    ))
  }

  model_fit <- fit_direct_glmnet(
    train_data = train_data,
    tune_grid = tune_grid,
    effective_inner_folds = effective_inner_folds,
    observation_weights = observation_weights,
    fixed_lambda = fixed_lambda,
    fixed_alpha = fixed_alpha
  )

  list(
    model_fit = model_fit,
    best_lambda = model_fit$best_lambda,
    best_alpha = model_fit$best_alpha,
    best_c = model_fit$best_c
  )
}

canonical_final_model_family <- function(family_name) {
  family_key <- tolower(family_name)

  dplyr::case_when(
    family_key %in% c("glmnet", "lasso", "logistic", "lr") ~ "glmnet",
    family_key %in% c("rf", "randomforest", "random_forest", "ranger") ~ "rf",
    family_key %in% c("xgb", "xgboost", "xgbtree") ~ "xgb",
    TRUE ~ family_key
  )
}

format_result_model_name <- function(model_name, final_model_family) {
  final_model_family <- canonical_final_model_family(final_model_family)

  if (length(final_model_families) == 1 && identical(final_model_family, "glmnet")) {
    return(model_name)
  }

  paste0(model_name, "__final_", final_model_family)
}

get_final_model_tune_grid <- function(final_model_family, train_data) {
  final_model_family <- canonical_final_model_family(final_model_family)
  predictor_count <- max(ncol(train_data) - 1, 1)

  if (final_model_family == "glmnet") {
    return(expand.grid(alpha = glmnet_alpha_values, lambda = seq(0.001, 0.1, length = 50)))
  }

  if (final_model_family == "rf") {
    rf_mtry_candidates <- if (length(rf_mtry_values_override) > 0) {
      rf_mtry_values_override
    } else if (isTRUE(rf_constrained_mtry)) {
      c(
        floor(sqrt(predictor_count) / 2),
        floor(sqrt(predictor_count)),
        floor(predictor_count / 4)
      )
    } else {
      c(
        floor(sqrt(predictor_count) / 2),
        floor(sqrt(predictor_count)),
        floor(predictor_count / 4),
        floor(predictor_count / 3),
        floor(predictor_count / 2)
      )
    }

    mtry_values <- unique(pmax(1, pmin(
      predictor_count,
      rf_mtry_candidates
    )))

    return(expand.grid(
      mtry = mtry_values,
      splitrule = "gini",
      min.node.size = as.integer(rf_min_node_size_values),
      max.depth = as.integer(rf_max_depth_values)
    ))
  }

  if (final_model_family == "xgb") {
    return(expand.grid(
      nrounds = as.integer(xgb_nrounds_values),
      max_depth = as.integer(xgb_max_depth_values),
      eta = xgb_eta_values,
      gamma = xgb_gamma_values,
      colsample_bytree = xgb_colsample_bytree_values,
      min_child_weight = xgb_min_child_weight_values,
      subsample = xgb_subsample_values
    ))
  }

  stop("Unsupported final model family: ", final_model_family)
}

select_tune_configuration <- function(tuning_results, final_model_family,
                                      use_one_se = FALSE) {
  best_idx <- which.max(tuning_results$C)

  if (!isTRUE(use_one_se) || !"C_se" %in% names(tuning_results)) {
    return(best_idx)
  }

  best_c <- tuning_results$C[best_idx]
  best_se <- tuning_results$C_se[best_idx]
  if (!is.finite(best_se)) {
    return(best_idx)
  }

  eligible <- tuning_results[tuning_results$C >= (best_c - best_se), , drop = FALSE]
  if (nrow(eligible) == 0) {
    return(best_idx)
  }
  eligible$.tune_index <- which(tuning_results$C >= (best_c - best_se))

  final_model_family <- canonical_final_model_family(final_model_family)
  eligible <- if (final_model_family == "rf") {
    eligible |>
      dplyr::arrange(dplyr::desc(min.node.size), max.depth, mtry)
  } else if (final_model_family == "xgb") {
    eligible |>
      dplyr::arrange(
        dplyr::desc(min_child_weight),
        dplyr::desc(gamma),
        max_depth,
        nrounds,
        eta,
        colsample_bytree,
        subsample
      )
  } else {
    eligible
  }

  eligible$.tune_index[1]
}

fit_tuned_rf_model <- function(train_data, tune_grid, effective_inner_folds,
                               observation_weights = NULL) {
  if (is.null(observation_weights)) {
    observation_weights <- rep(1, nrow(train_data))
  }

  inner_folds <- caret::createFolds(
    train_data$chr,
    k = effective_inner_folds,
    returnTrain = FALSE
  )

  grid_results <- lapply(seq_len(nrow(tune_grid)), function(grid_idx) {
    params_row <- tune_grid[grid_idx, , drop = FALSE]

    fold_c <- vapply(inner_folds, function(validation_idx) {
      training_idx <- setdiff(seq_len(nrow(train_data)), validation_idx)

      model <- ranger::ranger(
        chr ~ .,
        data = train_data[training_idx, , drop = FALSE],
        probability = TRUE,
        num.trees = 500,
        mtry = params_row$mtry,
        min.node.size = params_row$min.node.size,
        max.depth = params_row$max.depth,
        splitrule = as.character(params_row$splitrule),
        case.weights = observation_weights[training_idx]
      )

      validation_prob <- predict(
        model,
        # Retain the outcome column: ranger formula fits record it among the
        # required variables in some package versions.
        data = train_data[validation_idx, , drop = FALSE]
      )$predictions[, "X1"]

      as.numeric(Hmisc::rcorr.cens(
        validation_prob,
        as.numeric(train_data$chr[validation_idx] == "X1")
      )[2])
    }, numeric(1))

    cbind(
      params_row,
      C = mean(fold_c, na.rm = TRUE),
      C_se = stats::sd(fold_c, na.rm = TRUE) / sqrt(sum(is.finite(fold_c)))
    )
  })

  tuning_results <- dplyr::bind_rows(grid_results)
  best_idx <- select_tune_configuration(
    tuning_results,
    final_model_family = "rf",
    use_one_se = use_one_se_selection
  )
  best_tune <- tuning_results[best_idx, setdiff(names(tuning_results), c("C", "C_se")), drop = FALSE]

  final_model <- ranger::ranger(
    chr ~ .,
    data = train_data,
    probability = TRUE,
    num.trees = 500,
    mtry = best_tune$mtry,
    min.node.size = best_tune$min.node.size,
    max.depth = best_tune$max.depth,
    splitrule = as.character(best_tune$splitrule),
    case.weights = observation_weights,
    importance = "permutation"
  )

  list(
    model = final_model,
    predictor_names = setdiff(names(train_data), "chr"),
    bestTune = best_tune,
    results = tuning_results
  )
}

fit_tuned_xgb_model <- function(train_data, tune_grid, effective_inner_folds,
                                observation_weights = NULL) {
  train_x <- model.matrix(chr ~ . - 1, data = train_data)
  train_y <- as.numeric(train_data$chr == "X1")

  if (is.null(observation_weights)) {
    observation_weights <- rep(1, nrow(train_data))
  }

  inner_folds <- caret::createFolds(train_data$chr, k = effective_inner_folds, returnTrain = FALSE)

  grid_results <- lapply(seq_len(nrow(tune_grid)), function(grid_idx) {
    params_row <- tune_grid[grid_idx, , drop = FALSE]

    fold_c <- vapply(inner_folds, function(validation_idx) {
      training_idx <- setdiff(seq_len(nrow(train_x)), validation_idx)

      dtrain <- xgboost::xgb.DMatrix(
        data = train_x[training_idx, , drop = FALSE],
        label = train_y[training_idx],
        weight = observation_weights[training_idx]
      )

      model <- xgboost::xgb.train(
        params = list(
          objective = "binary:logistic",
          eval_metric = "auc",
          eta = params_row$eta,
          max_depth = params_row$max_depth,
          gamma = params_row$gamma,
          subsample = params_row$subsample,
          colsample_bytree = params_row$colsample_bytree,
          min_child_weight = params_row$min_child_weight
        ),
        data = dtrain,
        nrounds = params_row$nrounds,
        verbose = 0
      )

      validation_prob <- predict(
        model,
        newdata = xgboost::xgb.DMatrix(train_x[validation_idx, , drop = FALSE])
      )

      as.numeric(Hmisc::rcorr.cens(validation_prob, train_y[validation_idx])[2])
    }, numeric(1))

    cbind(
      params_row,
      C = mean(fold_c, na.rm = TRUE),
      C_se = stats::sd(fold_c, na.rm = TRUE) / sqrt(sum(is.finite(fold_c)))
    )
  })

  tuning_results <- dplyr::bind_rows(grid_results)
  best_idx <- select_tune_configuration(
    tuning_results,
    final_model_family = "xgb",
    use_one_se = use_one_se_selection
  )
  best_tune <- tuning_results[best_idx, setdiff(names(tuning_results), c("C", "C_se")), drop = FALSE]

  final_dtrain <- xgboost::xgb.DMatrix(
    data = train_x,
    label = train_y,
    weight = observation_weights
  )

  final_model <- xgboost::xgb.train(
    params = list(
      objective = "binary:logistic",
      eval_metric = "auc",
      eta = best_tune$eta,
      max_depth = best_tune$max_depth,
      gamma = best_tune$gamma,
      subsample = best_tune$subsample,
      colsample_bytree = best_tune$colsample_bytree,
      min_child_weight = best_tune$min_child_weight
    ),
    data = final_dtrain,
    nrounds = best_tune$nrounds,
    verbose = 0
  )

  list(
    model = final_model,
    feature_names = colnames(train_x),
    bestTune = best_tune,
    results = tuning_results
  )
}

fit_final_model <- function(train_data, final_model_family, effective_inner_folds,
                            model_name, outer_rep, outer_fold, use_single_predictor_model,
                            observation_weights = NULL, fixed_lambda = NULL, fixed_alpha = NULL) {
  final_model_family <- canonical_final_model_family(final_model_family)

  if (final_model_family == "glmnet") {
    return(fit_step_model(
      train_data = train_data,
      use_single_predictor_model = use_single_predictor_model,
      tune_grid = get_final_model_tune_grid("glmnet", train_data),
      effective_inner_folds = effective_inner_folds,
      model_name = model_name,
      outer_rep = outer_rep,
      outer_fold = outer_fold,
      fixed_lambda = fixed_lambda,
      fixed_alpha = fixed_alpha,
      observation_weights = observation_weights
    ))
  }

  control <- trainControl(
    method = "cv",
    number = effective_inner_folds,
    summaryFunction = cindex_summary,
    classProbs = TRUE
  )

  if (final_model_family == "rf") {
    model_fit <- fit_tuned_rf_model(
      train_data = train_data,
      tune_grid = get_final_model_tune_grid("rf", train_data),
      effective_inner_folds = effective_inner_folds,
      observation_weights = observation_weights
    )

    return(list(
      model_fit = model_fit,
      best_lambda = NA_real_,
      best_c = max(model_fit$results$C, na.rm = TRUE)
    ))
  }

  if (final_model_family == "xgb") {
    model_fit <- fit_tuned_xgb_model(
      train_data = train_data,
      tune_grid = get_final_model_tune_grid("xgb", train_data),
      effective_inner_folds = effective_inner_folds,
      observation_weights = observation_weights
    )

    return(list(
      model_fit = model_fit,
      best_lambda = NA_real_,
      best_c = max(model_fit$results$C, na.rm = TRUE)
    ))
  }

  stop("Unsupported final model family: ", final_model_family)
}

predict_final_model_prob <- function(model_fit, newdata, final_model_family, use_single_predictor_model) {
  final_model_family <- canonical_final_model_family(final_model_family)

  if (final_model_family == "glmnet") {
    return(predict_step_prob(model_fit, newdata, use_single_predictor_model))
  }

  if (final_model_family == "rf") {
    if (inherits(model_fit, "train")) {
      return(as.numeric(predict(model_fit, newdata = newdata, type = "prob")[, "X1"]))
    }

    required_columns <- unique(c("chr", model_fit$predictor_names))
    required_columns <- required_columns[required_columns %in% names(newdata)]
    predictor_data <- newdata[, required_columns, drop = FALSE]
    prob_pred <- predict(model_fit$model, data = predictor_data)$predictions

    if (is.matrix(prob_pred) && "X1" %in% colnames(prob_pred)) {
      return(as.numeric(prob_pred[, "X1"]))
    }

    if (is.matrix(prob_pred)) {
      return(as.numeric(prob_pred[, ncol(prob_pred)]))
    }

    return(as.numeric(prob_pred))
  }

  if (final_model_family == "xgb") {
    if (inherits(model_fit, "train")) {
      return(as.numeric(predict(model_fit, newdata = newdata, type = "prob")[, "X1"]))
    }

    predictor_cols <- setdiff(
      names(newdata),
      c("chr", "model", "Subject_ID", "OuterRepeat", "Fold", "PI", "chr_pred", "chr_num")
    )
    new_x <- model.matrix(~ . - 1, data = newdata[, predictor_cols, drop = FALSE])
    missing_cols <- setdiff(model_fit$feature_names, colnames(new_x))

    if (length(missing_cols) > 0) {
      zero_block <- matrix(0, nrow = nrow(new_x), ncol = length(missing_cols))
      colnames(zero_block) <- missing_cols
      new_x <- cbind(new_x, zero_block)
    }

    new_x <- new_x[, model_fit$feature_names, drop = FALSE]
    return(as.numeric(predict(model_fit$model, newdata = new_x)))
  }

  stop("Unsupported final model family for prediction: ", final_model_family)
}

fit_ssl_base_model <- function(train_data, use_single_predictor_model, tune_grid,
                               effective_inner_folds, model_name, outer_rep, outer_fold,
                               fixed_lambda = NULL, fixed_alpha = NULL, observation_weights = NULL,
                               ssl_base_family = "glmnet") {
  ssl_base_family <- canonical_final_model_family(ssl_base_family)

  if (ssl_base_family == "glmnet" || use_single_predictor_model) {
    fit_result <- fit_step_model(
      train_data = train_data,
      use_single_predictor_model = use_single_predictor_model,
      tune_grid = tune_grid,
      effective_inner_folds = effective_inner_folds,
      model_name = model_name,
      outer_rep = outer_rep,
      outer_fold = outer_fold,
      fixed_lambda = fixed_lambda,
      fixed_alpha = fixed_alpha,
      observation_weights = observation_weights
    )

    return(list(
      model_fit = fit_result$model_fit,
      best_lambda = fit_result$best_lambda,
      best_alpha = fit_result$best_alpha,
      best_c = fit_result$best_c,
      family = "glmnet"
    ))
  }

  predictor_count <- max(ncol(train_data) - 1, 1)

  if (ssl_base_family == "rf") {
    model_fit <- list(
      model = ranger::ranger(
        chr ~ .,
        data = train_data,
        probability = TRUE,
        num.trees = 500,
        mtry = max(1, floor(sqrt(predictor_count))),
        min.node.size = 5,
        splitrule = "gini",
        case.weights = observation_weights
      ),
      predictor_names = setdiff(names(train_data), "chr")
    )

    return(list(
      model_fit = model_fit,
      best_lambda = NA_real_,
      best_c = NA_real_,
      family = "rf"
    ))
  }

  if (ssl_base_family == "xgb") {
    train_x <- model.matrix(chr ~ . - 1, data = train_data)
    train_y <- as.numeric(train_data$chr == "X1")
    dtrain <- xgboost::xgb.DMatrix(
      data = train_x,
      label = train_y,
      weight = observation_weights
    )
    model_fit <- list(
      model = xgboost::xgb.train(
        params = list(
          objective = "binary:logistic",
          eval_metric = "auc",
          eta = 0.05,
          max_depth = 4,
          subsample = 0.8,
          colsample_bytree = 0.8,
          min_child_weight = 1
        ),
        data = dtrain,
        nrounds = 100,
        verbose = 0
      ),
      feature_names = colnames(train_x)
    )

    return(list(
      model_fit = model_fit,
      best_lambda = NA_real_,
      best_c = NA_real_,
      family = "xgb"
    ))
  }

  stop("Unsupported SSL base family: ", ssl_base_family)
}

predict_ssl_base_prob <- function(model_fit, newdata, use_single_predictor_model,
                                  ssl_base_family = "glmnet") {
  ssl_base_family <- canonical_final_model_family(ssl_base_family)

  if (ssl_base_family == "glmnet" || use_single_predictor_model) {
    return(predict_step_prob(model_fit, newdata, use_single_predictor_model))
  }

  predict_final_model_prob(
    model_fit = model_fit,
    newdata = newdata,
    final_model_family = ssl_base_family,
    use_single_predictor_model = use_single_predictor_model
  )
}

clip_prob <- function(prob, eps = 1e-6) {
  pmin(pmax(prob, eps), 1 - eps)
}

calibrate_probabilities_for_pseudolabelling <- function(probabilities, labelled_probabilities, labelled_outcome) {
  if (length(probabilities) == 0 || length(labelled_probabilities) == 0) {
    return(probabilities)
  }

  y <- as.integer(labelled_outcome == "X1")
  labelled_probabilities <- clip_prob(labelled_probabilities)
  probabilities <- clip_prob(probabilities)

  calibration_data <- data.frame(
    y = y,
    logit_p = qlogis(labelled_probabilities)
  )

  calibration_data <- calibration_data[is.finite(calibration_data$logit_p), , drop = FALSE]

  if (nrow(calibration_data) < 10 || length(unique(calibration_data$y)) < 2 ||
      length(unique(round(calibration_data$logit_p, 8))) < 2) {
    return(probabilities)
  }

  calibration_fit <- tryCatch(
    glm(y ~ logit_p, data = calibration_data, family = binomial()),
    error = function(e) NULL,
    warning = function(w) suppressWarnings(glm(y ~ logit_p, data = calibration_data, family = binomial()))
  )

  if (is.null(calibration_fit) || any(!is.finite(coef(calibration_fit)))) {
    return(probabilities)
  }

  as.numeric(predict(calibration_fit, newdata = data.frame(logit_p = qlogis(probabilities)), type = "response"))
}

select_weighted_pseudolabels <- function(unlabelled_scored, tail_fraction = 0.05,
                                         min_positive_prob = NA_real_, max_negative_prob = NA_real_,
                                         remaining_weight_budget = Inf,
                                         class_balanced_tails = FALSE) {
  if (nrow(unlabelled_scored) == 0) {
    return(list(
      selected = unlabelled_scored,
      remaining = unlabelled_scored,
      diagnostics = data.frame(
        positive_selection_threshold = NA_real_,
        negative_selection_threshold = NA_real_,
        n_positive_selected = 0,
        n_negative_selected = 0,
        pseudolabel_weight_raw = 0,
        pseudolabel_weight_scaled = 0,
        pseudolabel_weight_scale_factor = NA_real_,
        stringsAsFactors = FALSE
      )
    ))
  }

  q_low <- as.numeric(quantile(unlabelled_scored$PI, probs = tail_fraction, na.rm = TRUE, names = FALSE))
  q_high <- as.numeric(quantile(unlabelled_scored$PI, probs = 1 - tail_fraction, na.rm = TRUE, names = FALSE))
  positive_selection_threshold <- if (is.na(min_positive_prob)) q_high else max(q_high, min_positive_prob)
  negative_selection_threshold <- if (is.na(max_negative_prob)) q_low else min(q_low, max_negative_prob)

  positive_selected <- unlabelled_scored$PI >= positive_selection_threshold
  negative_selected <- unlabelled_scored$PI <= negative_selection_threshold

  if (isTRUE(class_balanced_tails)) {
    positive_idx <- which(positive_selected)
    negative_idx <- which(negative_selected)
    balanced_n <- min(length(positive_idx), length(negative_idx))

    positive_selected <- rep(FALSE, nrow(unlabelled_scored))
    negative_selected <- rep(FALSE, nrow(unlabelled_scored))

    if (balanced_n > 0) {
      positive_idx <- positive_idx[order(unlabelled_scored$PI[positive_idx], decreasing = TRUE)]
      negative_idx <- negative_idx[order(unlabelled_scored$PI[negative_idx], decreasing = FALSE)]
      positive_selected[positive_idx[seq_len(balanced_n)]] <- TRUE
      negative_selected[negative_idx[seq_len(balanced_n)]] <- TRUE
    }
  }

  selected_idx <- positive_selected | negative_selected

  selected <- unlabelled_scored[selected_idx, , drop = FALSE]
  remaining <- unlabelled_scored[!selected_idx, , drop = FALSE]

  if (nrow(selected) > 0) {
    selected$chr <- ifelse(selected$PI >= positive_selection_threshold, "X1", "X0")
    selected$pseudolabel_weight_raw <- 2 * abs(selected$PI - 0.5)
    total_raw_weight <- sum(selected$pseudolabel_weight_raw, na.rm = TRUE)
    if (is.finite(remaining_weight_budget) && remaining_weight_budget <= 0) {
      selected <- selected[0, , drop = FALSE]
      remaining <- unlabelled_scored
      scale_factor <- 0
      total_scaled_weight <- 0
    } else {
      scale_factor <- if (is.finite(remaining_weight_budget) && total_raw_weight > remaining_weight_budget &&
        total_raw_weight > 0) remaining_weight_budget / total_raw_weight else 1
      selected$pseudolabel_weight <- selected$pseudolabel_weight_raw * scale_factor
      total_scaled_weight <- sum(selected$pseudolabel_weight, na.rm = TRUE)
    }
  } else {
    selected$pseudolabel_weight_raw <- numeric(0)
    selected$pseudolabel_weight <- numeric(0)
    total_raw_weight <- 0
    total_scaled_weight <- 0
    scale_factor <- NA_real_
  }

  list(
    selected = selected,
    remaining = remaining,
    diagnostics = data.frame(
      positive_selection_threshold = positive_selection_threshold,
      negative_selection_threshold = negative_selection_threshold,
      n_positive_selected = sum(positive_selected, na.rm = TRUE),
      n_negative_selected = sum(negative_selected, na.rm = TRUE),
      pseudolabel_weight_raw = total_raw_weight,
      pseudolabel_weight_scaled = total_scaled_weight,
      pseudolabel_weight_scale_factor = scale_factor,
      stringsAsFactors = FALSE
    )
  )
}

strip_ssl_helper_columns <- function(df) {
  helper_cols <- intersect(
    c("PI", "PI_uncalibrated", "pseudolabel_weight_raw", "pseudolabel_weight"),
    names(df)
  )

  if (length(helper_cols) == 0) {
    return(df)
  }

  df[, setdiff(names(df), helper_cols), drop = FALSE]
}

run_ssl_training_loop <- function(train_imp, unlabelled_imp, use_single_predictor_model, tune_grid,
                                  effective_inner_folds, model_name, outer_rep, outer_fold,
                                  fixed_lambda = NULL, fixed_alpha = NULL, max_ssl_iterations = 20,
                                  tail_fraction = 0.05, min_positive_prob = NA_real_,
                                  max_negative_prob = NA_real_, min_confident_pseudolabels = 10,
                                  pseudolabel_weight_cap_ratio = 1.0,
                                  class_balanced_tails = FALSE,
                                  calibrate_pseudolabel_probs = FALSE,
                                  ssl_base_family = "glmnet") {
  ssl_base_family <- canonical_final_model_family(ssl_base_family)
  ssl_train_data <- train_imp
  ssl_train_weights <- rep(1, nrow(train_imp))
  current_unlabelled <- unlabelled_imp
  ssl_iteration_log <- data.frame()
  ssl_prediction_distribution <- data.frame()
  last_fit <- NULL
  successful_ssl_iterations <- 0L
  labelled_weight_total <- sum(ssl_train_weights)

  for (ssl_iter in seq_len(max_ssl_iterations)) {
    log_progress("      Semi-supervised iteration ", ssl_iter, "/", max_ssl_iterations,
                 " for ", model_name, ", outer repeat ", outer_rep, ", outer fold ", outer_fold)

    fit_result <- fit_ssl_base_model(
      train_data = ssl_train_data,
      use_single_predictor_model = use_single_predictor_model,
      tune_grid = tune_grid,
      effective_inner_folds = effective_inner_folds,
      model_name = model_name,
      outer_rep = outer_rep,
      outer_fold = outer_fold,
      fixed_lambda = fixed_lambda,
      fixed_alpha = fixed_alpha,
      observation_weights = ssl_train_weights,
      ssl_base_family = ssl_base_family
    )

    last_fit <- fit_result$model_fit

    if (nrow(current_unlabelled) == 0) {
      ssl_iteration_log <- dplyr::bind_rows(ssl_iteration_log, data.frame(
        ssl_iteration = ssl_iter,
        newly_pseudolabelled = 0,
        newly_positive_pseudolabels = 0,
        newly_negative_pseudolabels = 0,
        positive_selection_threshold = NA_real_,
        negative_selection_threshold = NA_real_,
        pseudolabel_weight_raw = 0,
        pseudolabel_weight_scaled = 0,
        pseudolabel_weight_scale_factor = NA_real_,
        unlabelled_remaining = 0,
        stopping_reason = "no_unlabelled_remaining",
        stringsAsFactors = FALSE
      ))
      break
    }

    current_unlabelled <- strip_ssl_helper_columns(current_unlabelled)
    current_unlabelled$PI <- predict_ssl_base_prob(
      last_fit,
      current_unlabelled,
      use_single_predictor_model,
      ssl_base_family
    )
    if (isTRUE(calibrate_pseudolabel_probs)) {
      current_unlabelled$PI_uncalibrated <- current_unlabelled$PI
      labelled_probabilities <- predict_ssl_base_prob(
        last_fit,
        train_imp,
        use_single_predictor_model,
        ssl_base_family
      )
      current_unlabelled$PI <- calibrate_probabilities_for_pseudolabelling(
        probabilities = current_unlabelled$PI,
        labelled_probabilities = labelled_probabilities,
        labelled_outcome = train_imp$chr
      )
    }
    pi_summary <- quantile(
      current_unlabelled$PI,
      probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99),
      na.rm = TRUE,
      names = FALSE
    )
    current_pseudolabel_weight <- sum(ssl_train_weights) - labelled_weight_total
    remaining_weight_budget <- max((labelled_weight_total * pseudolabel_weight_cap_ratio) - current_pseudolabel_weight, 0)
    pseudolabel_selection <- select_weighted_pseudolabels(
      unlabelled_scored = current_unlabelled,
      tail_fraction = tail_fraction,
      min_positive_prob = min_positive_prob,
      max_negative_prob = max_negative_prob,
      remaining_weight_budget = remaining_weight_budget,
      class_balanced_tails = class_balanced_tails
    )

    confident_pseudolabels <- pseudolabel_selection$selected
    current_unlabelled <- strip_ssl_helper_columns(pseudolabel_selection$remaining)
    ssl_prediction_distribution <- dplyr::bind_rows(ssl_prediction_distribution, data.frame(
      model = model_name,
      OuterRepeat = outer_rep,
      Fold = outer_fold,
      ssl_iteration = ssl_iter,
      n_unlabelled_scored = nrow(current_unlabelled) + nrow(confident_pseudolabels),
      mean_pi = mean(c(current_unlabelled$PI, confident_pseudolabels$PI), na.rm = TRUE),
      sd_pi = sd(c(current_unlabelled$PI, confident_pseudolabels$PI), na.rm = TRUE),
      min_pi = min(c(current_unlabelled$PI, confident_pseudolabels$PI), na.rm = TRUE),
      q01_pi = pi_summary[1],
      q05_pi = pi_summary[2],
      q10_pi = pi_summary[3],
      q25_pi = pi_summary[4],
      q50_pi = pi_summary[5],
      q75_pi = pi_summary[6],
      q90_pi = pi_summary[7],
      q95_pi = pi_summary[8],
      q99_pi = pi_summary[9],
      max_pi = max(c(current_unlabelled$PI, confident_pseudolabels$PI), na.rm = TRUE),
      tail_fraction = tail_fraction,
      min_positive_prob = min_positive_prob,
      max_negative_prob = max_negative_prob,
      positive_selection_threshold = pseudolabel_selection$diagnostics$positive_selection_threshold,
      negative_selection_threshold = pseudolabel_selection$diagnostics$negative_selection_threshold,
      n_positive_selected = pseudolabel_selection$diagnostics$n_positive_selected,
      n_negative_selected = pseudolabel_selection$diagnostics$n_negative_selected,
      pseudolabel_weight_raw = pseudolabel_selection$diagnostics$pseudolabel_weight_raw,
      pseudolabel_weight_scaled = pseudolabel_selection$diagnostics$pseudolabel_weight_scaled,
      pseudolabel_weight_scale_factor = pseudolabel_selection$diagnostics$pseudolabel_weight_scale_factor,
      class_balanced_tails = class_balanced_tails,
      calibrated_pseudolabel_probabilities = calibrate_pseudolabel_probs,
      n_below_05 = sum(c(current_unlabelled$PI, confident_pseudolabels$PI) < 0.05, na.rm = TRUE),
      n_above_95 = sum(c(current_unlabelled$PI, confident_pseudolabels$PI) > 0.95, na.rm = TRUE),
      n_below_10 = sum(c(current_unlabelled$PI, confident_pseudolabels$PI) < 0.10, na.rm = TRUE),
      n_above_90 = sum(c(current_unlabelled$PI, confident_pseudolabels$PI) > 0.90, na.rm = TRUE),
      stringsAsFactors = FALSE
    ))

    ssl_iteration_log <- dplyr::bind_rows(ssl_iteration_log, data.frame(
      ssl_iteration = ssl_iter,
      newly_pseudolabelled = nrow(confident_pseudolabels),
      newly_positive_pseudolabels = sum(confident_pseudolabels$chr == "X1", na.rm = TRUE),
      newly_negative_pseudolabels = sum(confident_pseudolabels$chr == "X0", na.rm = TRUE),
      positive_selection_threshold = pseudolabel_selection$diagnostics$positive_selection_threshold,
      negative_selection_threshold = pseudolabel_selection$diagnostics$negative_selection_threshold,
      pseudolabel_weight_raw = pseudolabel_selection$diagnostics$pseudolabel_weight_raw,
      pseudolabel_weight_scaled = pseudolabel_selection$diagnostics$pseudolabel_weight_scaled,
      pseudolabel_weight_scale_factor = pseudolabel_selection$diagnostics$pseudolabel_weight_scale_factor,
      unlabelled_remaining = nrow(current_unlabelled),
      stopping_reason = ifelse(
        nrow(confident_pseudolabels) < min_confident_pseudolabels,
        "minimum_confident_not_met",
        "continue"
      ),
      stringsAsFactors = FALSE
    ))

    if (nrow(confident_pseudolabels) < min_confident_pseudolabels) {
      break
    }

    confident_weights <- confident_pseudolabels$pseudolabel_weight
    confident_pseudolabels$chr <- factor(
      confident_pseudolabels$chr,
      levels = levels(train_imp$chr)
    )
    confident_pseudolabels <- strip_ssl_helper_columns(confident_pseudolabels)
    ssl_train_data <- dplyr::bind_rows(ssl_train_data, confident_pseudolabels)
    ssl_train_weights <- c(ssl_train_weights, confident_weights)
    successful_ssl_iterations <- ssl_iter
  }

  final_fit <- fit_ssl_base_model(
    train_data = ssl_train_data,
    use_single_predictor_model = use_single_predictor_model,
    tune_grid = tune_grid,
    effective_inner_folds = effective_inner_folds,
    model_name = model_name,
    outer_rep = outer_rep,
    outer_fold = outer_fold,
    fixed_lambda = fixed_lambda,
    fixed_alpha = fixed_alpha,
    observation_weights = ssl_train_weights,
    ssl_base_family = ssl_base_family
  )

  pseudolabelled_rows <- nrow(ssl_train_data) - nrow(train_imp)
  pseudolabelled_events <- sum(ssl_train_data$chr == "X1") - sum(train_imp$chr == "X1")
  pseudolabelled_weight <- sum(ssl_train_weights) - labelled_weight_total

  list(
    final_training_data = ssl_train_data,
    final_training_weights = ssl_train_weights,
    final_model = final_fit$model_fit,
    ssl_iteration_log = ssl_iteration_log,
    ssl_prediction_distribution = ssl_prediction_distribution,
    successful_ssl_iterations = successful_ssl_iterations,
    pseudolabelled_rows = pseudolabelled_rows,
    pseudolabelled_events = pseudolabelled_events,
    pseudolabelled_weight = pseudolabelled_weight,
    unlabelled_remaining = nrow(current_unlabelled)
  )
}

build_ssl_candidate_grid <- function() {
  expand.grid(
    tail_fraction = ssl_tail_fraction_candidates,
    max_negative_prob = ssl_max_negative_prob_candidates,
    pseudolabel_weight_cap_ratio = ssl_pseudolabel_weight_cap_ratio_candidates,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::arrange(tail_fraction, max_negative_prob, pseudolabel_weight_cap_ratio)
}

tune_ssl_configuration <- function(train_imp, unlabelled_imp, use_single_predictor_model, tune_grid,
                                   effective_inner_folds, model_name, outer_rep, outer_fold, seeds,
                                   ssl_base_family = "glmnet") {
  ssl_base_family <- canonical_final_model_family(ssl_base_family)
  candidate_grid <- build_ssl_candidate_grid()

  if (nrow(candidate_grid) == 1) {
    return(list(
      best_tail_fraction = candidate_grid$tail_fraction[1],
      best_max_negative_prob = candidate_grid$max_negative_prob[1],
      best_pseudolabel_weight_cap_ratio = candidate_grid$pseudolabel_weight_cap_ratio[1],
      best_alpha = if (use_single_predictor_model) NA_real_ else tune_grid$alpha[1],
      best_lambda = if (use_single_predictor_model) NA_real_ else tune_grid$lambda[1],
      search_results = data.frame(
        model = model_name,
        OuterRepeat = outer_rep,
        Fold = outer_fold,
        tail_fraction = candidate_grid$tail_fraction[1],
        max_negative_prob = candidate_grid$max_negative_prob[1],
        pseudolabel_weight_cap_ratio = candidate_grid$pseudolabel_weight_cap_ratio[1],
        best_alpha = if (use_single_predictor_model) NA_real_ else tune_grid$alpha[1],
        best_lambda = if (use_single_predictor_model) NA_real_ else tune_grid$lambda[1],
        avg_C = NA_real_,
        mean_ssl_iterations = NA_real_,
        stringsAsFactors = FALSE
      )
    ))
  }

  search_results <- data.frame()

  for (candidate_idx in seq_len(nrow(candidate_grid))) {
    candidate <- candidate_grid[candidate_idx, , drop = FALSE]
    log_progress(
      "    SSL candidate ", candidate_idx, "/", nrow(candidate_grid), " for ", model_name,
      ": tail=", candidate$tail_fraction,
      ", neg_gate=", candidate$max_negative_prob,
      ", cap=", candidate$pseudolabel_weight_cap_ratio
    )

    inner_results <- data.frame()
    ssl_iterations_per_repeat <- numeric(innerRepeats)

    for (inner_rep in seq_len(innerRepeats)) {
      log_progress("      Inner Repeat ", inner_rep, "/", innerRepeats, " for ", model_name,
                   ", outer repeat ", outer_rep, ", outer fold ", outer_fold)
      set.seed(seeds[outer_rep] + inner_rep)

      ssl_train_data <- train_imp
      ssl_train_weights <- rep(1, nrow(train_imp))
      ssl_unlabelled_data <- unlabelled_imp
      labelled_weight_total <- sum(ssl_train_weights)
      completed_iterations <- 0L

      for (ssl_iter in seq_len(max_ssl_iterations)) {
        log_progress("        Inner SSL iteration ", ssl_iter, "/", max_ssl_iterations, " for ", model_name,
                     ", candidate ", candidate_idx, ", inner repeat ", inner_rep)

        inner_fit <- fit_ssl_base_model(
          train_data = ssl_train_data,
          use_single_predictor_model = use_single_predictor_model,
          tune_grid = tune_grid,
          effective_inner_folds = effective_inner_folds,
          model_name = model_name,
          outer_rep = outer_rep,
          outer_fold = outer_fold,
          observation_weights = ssl_train_weights,
          ssl_base_family = ssl_base_family
        )

        inner_results <- rbind(inner_results, data.frame(
          tail_fraction = candidate$tail_fraction,
          max_negative_prob = candidate$max_negative_prob,
          pseudolabel_weight_cap_ratio = candidate$pseudolabel_weight_cap_ratio,
          alpha = inner_fit$best_alpha,
          lambda = inner_fit$best_lambda,
          C = inner_fit$best_c,
          repeat_number = inner_rep,
          ssl_iteration = ssl_iter,
          stringsAsFactors = FALSE
        ))

        if (nrow(ssl_unlabelled_data) == 0) {
          break
        }

        ssl_unlabelled_data <- strip_ssl_helper_columns(ssl_unlabelled_data)
        ssl_unlabelled_data$PI <- predict_ssl_base_prob(
          inner_fit$model_fit,
          ssl_unlabelled_data,
          use_single_predictor_model,
          ssl_base_family
        )
        if (isTRUE(ssl_calibrate_pseudolabel_probs)) {
          ssl_unlabelled_data$PI_uncalibrated <- ssl_unlabelled_data$PI
          labelled_probabilities <- predict_ssl_base_prob(
            inner_fit$model_fit,
            train_imp,
            use_single_predictor_model,
            ssl_base_family
          )
          ssl_unlabelled_data$PI <- calibrate_probabilities_for_pseudolabelling(
            probabilities = ssl_unlabelled_data$PI,
            labelled_probabilities = labelled_probabilities,
            labelled_outcome = train_imp$chr
          )
        }
        current_pseudolabel_weight <- sum(ssl_train_weights) - labelled_weight_total
        remaining_weight_budget <- max(
          (labelled_weight_total * candidate$pseudolabel_weight_cap_ratio) - current_pseudolabel_weight,
          0
        )
        pseudolabel_selection <- select_weighted_pseudolabels(
          unlabelled_scored = ssl_unlabelled_data,
          tail_fraction = candidate$tail_fraction,
          min_positive_prob = ssl_min_positive_prob,
          max_negative_prob = candidate$max_negative_prob,
          remaining_weight_budget = remaining_weight_budget,
          class_balanced_tails = ssl_class_balanced_tails
        )

        confident_pseudolabels <- pseudolabel_selection$selected
        ssl_unlabelled_data <- strip_ssl_helper_columns(pseudolabel_selection$remaining)

        if (nrow(confident_pseudolabels) < 10) {
          break
        }

        confident_weights <- confident_pseudolabels$pseudolabel_weight
        confident_pseudolabels$chr <- factor(
          confident_pseudolabels$chr,
          levels = levels(train_imp$chr)
        )
        confident_pseudolabels <- strip_ssl_helper_columns(confident_pseudolabels)
        ssl_train_data <- dplyr::bind_rows(ssl_train_data, confident_pseudolabels)
        ssl_train_weights <- c(ssl_train_weights, confident_weights)
        completed_iterations <- ssl_iter
      }

      ssl_iterations_per_repeat[inner_rep] <- completed_iterations
    }

    lambda_summary <- inner_results %>%
      dplyr::filter(!is.na(C)) %>%
      dplyr::group_by(alpha, lambda) %>%
      dplyr::summarise(avg_C = mean(C, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(desc(avg_C))

    candidate_best_alpha <- if (use_single_predictor_model) {
      NA_real_
    } else if (nrow(lambda_summary) == 0) {
      tune_grid$alpha[1]
    } else {
      lambda_summary$alpha[1]
    }

    candidate_best_lambda <- if (use_single_predictor_model) {
      NA_real_
    } else if (nrow(lambda_summary) == 0) {
      tune_grid$lambda[1]
    } else {
      lambda_summary$lambda[1]
    }

    candidate_best_c <- if (nrow(lambda_summary) == 0) {
      mean(inner_results$C, na.rm = TRUE)
    } else {
      lambda_summary$avg_C[1]
    }

    search_results <- rbind(search_results, data.frame(
      model = model_name,
      OuterRepeat = outer_rep,
      Fold = outer_fold,
      tail_fraction = candidate$tail_fraction,
      max_negative_prob = candidate$max_negative_prob,
      pseudolabel_weight_cap_ratio = candidate$pseudolabel_weight_cap_ratio,
      best_alpha = candidate_best_alpha,
      best_lambda = candidate_best_lambda,
      avg_C = candidate_best_c,
      mean_ssl_iterations = mean(ssl_iterations_per_repeat, na.rm = TRUE),
      stringsAsFactors = FALSE
    ))
  }

  search_results <- search_results %>%
    dplyr::arrange(desc(avg_C), tail_fraction, max_negative_prob, pseudolabel_weight_cap_ratio)

  best_candidate <- search_results[1, , drop = FALSE]

  list(
    best_tail_fraction = best_candidate$tail_fraction,
    best_max_negative_prob = best_candidate$max_negative_prob,
    best_pseudolabel_weight_cap_ratio = best_candidate$pseudolabel_weight_cap_ratio,
    best_alpha = best_candidate$best_alpha,
    best_lambda = best_candidate$best_lambda,
    search_results = search_results
  )
}

impute_predictors <- function(df_x) {
  if (ncol(df_x) == 1) {
    log_progress("Single predictor detected; using direct fallback imputation")
    column_name <- names(df_x)[1]
    column_data <- df_x[[1]]

    if (is.numeric(column_data)) {
      fill_value <- median(column_data, na.rm = TRUE)
      if (is.na(fill_value)) {
        fill_value <- 0
      }
    } else {
      observed_values <- column_data[!is.na(column_data)]
      if (length(observed_values) == 0) {
        fill_value <- if (is.factor(column_data)) levels(column_data)[1] else ""
      } else {
        fill_value <- names(sort(table(observed_values), decreasing = TRUE))[1]
      }
    }

    df_x[[1]][is.na(df_x[[1]])] <- fill_value

    if (is.factor(column_data)) {
      df_x[[1]] <- factor(df_x[[1]], levels = levels(column_data))
    }

    names(df_x)[1] <- column_name
    return(df_x)
  }

  vars_with_na <- names(which(colSums(is.na(df_x)) > 0))
  method <- make.method(df_x)
  method[vars_with_na] <- "rf"

  pred_matrix <- make.predictorMatrix(df_x)
  diag(pred_matrix) <- 0

  too_sparse <- names(which(colSums(is.na(df_x)) > 10))
  pred_matrix[, too_sparse] <- 0

  imp <- mice(
    df_x,
    method = method,
    predictorMatrix = pred_matrix,
    m = 1,
    maxit = 5,
    seed = 123,
    print = FALSE
  )

  df_imp <- complete(imp)

  if (sum(is.na(df_x)) > 0) {
    cat("Imputing remaining missing predictor values with kNN\n")
    df_imp <- kNN(df_imp, imp_var = FALSE)
  } else {
    cat("No missing data in predictor matrix\n")
  }

  df_imp
}

apply_near_zero_variance_filter <- function(train_imp, test_imp, unlabelled_imp,
                                            model_name, outer_rep, outer_fold) {
  predictor_names <- setdiff(names(train_imp), "chr")

  filter_summary <- list(
    train_imp = train_imp,
    test_imp = test_imp,
    unlabelled_imp = unlabelled_imp,
    predictors_before = length(predictor_names),
    predictors_after = length(predictor_names),
    excluded_predictors = character(0)
  )

  if (length(predictor_names) <= 1) {
    return(filter_summary)
  }

  nzv_metrics <- caret::nearZeroVar(
    train_imp[, predictor_names, drop = FALSE],
    saveMetrics = TRUE
  )

  excluded_predictors <- rownames(nzv_metrics)[nzv_metrics$nzv]

  if (length(excluded_predictors) == 0) {
    return(filter_summary)
  }

  retained_predictors <- setdiff(predictor_names, excluded_predictors)

  if (length(retained_predictors) == 0) {
    log_progress(
      "Near-zero variance filtering would remove all predictors for ", model_name,
      ", outer repeat ", outer_rep, ", outer fold ", outer_fold,
      "; retaining original predictor set"
    )
    return(filter_summary)
  }

  log_progress(
    "Near-zero variance filtering removed ", length(excluded_predictors),
    " predictor(s) for ", model_name, ", outer repeat ", outer_rep,
    ", outer fold ", outer_fold, ": ", paste(excluded_predictors, collapse = ", ")
  )

  filter_summary$train_imp <- train_imp[, c(retained_predictors, "chr"), drop = FALSE]
  filter_summary$test_imp <- test_imp[, c(retained_predictors, "chr"), drop = FALSE]
  filter_summary$unlabelled_imp <- unlabelled_imp[, retained_predictors, drop = FALSE]
  filter_summary$predictors_after <- length(retained_predictors)
  filter_summary$excluded_predictors <- excluded_predictors

  filter_summary
}

summarise_boot_metrics <- function(boot_df, model_name, B = 2000) {
  log_progress("Starting bootstrap summary for ", model_name, " with B=", B)
  boot_df$obs <- factor(boot_df$obs)
  n <- nrow(boot_df)

  C_boot <- numeric(B)
  BA_boot <- numeric(B)
  SEN_boot <- numeric(B)
  SPEC_boot <- numeric(B)
  PPV_boot <- numeric(B)
  NPV_boot <- numeric(B)
  Intercept_boot <- numeric(B)
  Slope_boot <- numeric(B)
  Brier_boot <- numeric(B)

  for (b in seq_len(B)) {
    if (b == 1 || b %% 250 == 0 || b == B) {
      log_progress("Bootstrap ", model_name, ": iteration ", b, "/", B)
    }
    attempt <- 0

    repeat {
      attempt <- attempt + 1
      idx <- sample(seq_len(n), size = n, replace = TRUE)
      d <- boot_df[idx, ]

      if (length(unique(d$obs)) > 1 || attempt >= 100) {
        break
      }
    }

    if (length(unique(d$obs)) < 2) {
      C_boot[b] <- NA_real_
      BA_boot[b] <- NA_real_
      SEN_boot[b] <- NA_real_
      SPEC_boot[b] <- NA_real_
      PPV_boot[b] <- NA_real_
      NPV_boot[b] <- NA_real_
      Intercept_boot[b] <- NA_real_
      Slope_boot[b] <- NA_real_
      Brier_boot[b] <- NA_real_
      next
    }

    C_boot[b] <- concordance(glm(obs ~ pred, data = d, family = "binomial"))$concordance

    pred_class <- ifelse(d$chr_pred >= 0.5, 1, 0)
    cm <- caret::confusionMatrix(
      factor(pred_class, levels = c(0, 1)),
      d$obs,
      positive = "1"
    )

    BA_boot[b] <- cm$byClass["Balanced Accuracy"]
    SEN_boot[b] <- cm$byClass["Sensitivity"]
    SPEC_boot[b] <- cm$byClass["Specificity"]
    PPV_boot[b] <- cm$byClass["Pos Pred Value"]
    NPV_boot[b] <- cm$byClass["Neg Pred Value"]

    logistic_calibration <- predRupdate::pred_val_probs(
      binary_outcome = as.numeric(d$obs) - 1,
      Prob = d$pred,
      cal_plot = FALSE
    )

    Intercept_boot[b] <- logistic_calibration$CalInt[1]
    Slope_boot[b] <- logistic_calibration$CalSlope[1]
    Brier_boot[b] <- logistic_calibration$BrierScore[1]
  }

  class_boot_summary <- rbind(
    C = boot_summary(C_boot),
    BalancedAccuracy = boot_summary(BA_boot),
    Sensitivity = boot_summary(SEN_boot),
    Specificity = boot_summary(SPEC_boot),
    PPV = boot_summary(PPV_boot),
    NPV = boot_summary(NPV_boot)
  ) %>%
    as.data.frame()

  colnames(class_boot_summary) <- c("Mean", "CI_low", "CI_high")
  class_boot_summary$Metric <- rownames(class_boot_summary)
  rownames(class_boot_summary) <- NULL
  class_boot_summary <- class_boot_summary %>%
    select(Metric, Mean, CI_low, CI_high)

  calib_boot_summary <- rbind(
    Intercept = boot_summary(Intercept_boot),
    Slope = boot_summary(Slope_boot),
    Brier = boot_summary(Brier_boot)
  ) %>%
    as.data.frame()

  colnames(calib_boot_summary) <- c("Mean", "CI_low", "CI_high")
  calib_boot_summary$Metric <- rownames(calib_boot_summary)
  rownames(calib_boot_summary) <- NULL
  calib_boot_summary <- calib_boot_summary %>%
    select(Metric, Mean, CI_low, CI_high)

  C_m <- get_metric(class_boot_summary, "C")
  BA_m <- get_metric(class_boot_summary, "BalancedAccuracy")
  SEN_m <- get_metric(class_boot_summary, "Sensitivity")
  SPEC_m <- get_metric(class_boot_summary, "Specificity")
  PPV_m <- get_metric(class_boot_summary, "PPV")
  NPV_m <- get_metric(class_boot_summary, "NPV")

  Intercept_m <- get_metric(calib_boot_summary, "Intercept")
  Slope_m <- get_metric(calib_boot_summary, "Slope")
  Brier_m <- get_metric(calib_boot_summary, "Brier")

  data.frame(
    model = model_name,
    C = if (!is.null(C_m)) fmt_num(C_m$mean, C_m$low, C_m$high, digits = 3) else NA_character_,
    balanced_accuracy = if (!is.null(BA_m)) fmt_pct(BA_m$mean, BA_m$low, BA_m$high, digits_mean = 1, digits_ci = 1) else NA_character_,
    sensitivity = if (!is.null(SEN_m)) fmt_pct(SEN_m$mean, SEN_m$low, SEN_m$high, digits_mean = 1, digits_ci = 1) else NA_character_,
    specificity = if (!is.null(SPEC_m)) fmt_pct(SPEC_m$mean, SPEC_m$low, SPEC_m$high, digits_mean = 1, digits_ci = 1) else NA_character_,
    ppv = if (!is.null(PPV_m)) fmt_pct(PPV_m$mean, PPV_m$low, PPV_m$high, digits_mean = 1, digits_ci = 1) else NA_character_,
    npv = if (!is.null(NPV_m)) fmt_pct(NPV_m$mean, NPV_m$low, NPV_m$high, digits_mean = 1, digits_ci = 1) else NA_character_,
    brier = if (!is.null(Brier_m)) paste0(
      formatC(Brier_m$mean, format = "f", digits = 2), " (",
      formatC(Brier_m$low, format = "f", digits = 2), "-",
      formatC(Brier_m$high, format = "f", digits = 2), ")"
    ) else NA_character_,
    calibration_intercept = if (!is.null(Intercept_m)) paste0(
      formatC(Intercept_m$mean, format = "f", digits = 2), " (",
      formatC(Intercept_m$low, format = "f", digits = 2), "-",
      formatC(Intercept_m$high, format = "f", digits = 2), ")"
    ) else NA_character_,
    calibration_slope = if (!is.null(Slope_m)) paste0(
      formatC(Slope_m$mean, format = "f", digits = 2), " (",
      formatC(Slope_m$low, format = "f", digits = 2), "-",
      formatC(Slope_m$high, format = "f", digits = 2), ")"
    ) else NA_character_,
    stringsAsFactors = FALSE
  )
}

summarise_point_metrics <- function(c_stat_nested, calibration_slopes, model_name) {
  c_summary <- c_stat_nested %>%
    dplyr::summarise(
      C = mean(C_test, na.rm = TRUE),
      balanced_accuracy = mean(balanced_accuracy, na.rm = TRUE),
      sensitivity = mean(sensitivity, na.rm = TRUE),
      specificity = mean(specificity, na.rm = TRUE),
      ppv = mean(ppv, na.rm = TRUE),
      npv = mean(npv, na.rm = TRUE)
    )

  cal_summary <- calibration_slopes %>%
    dplyr::summarise(
      brier = mean(brier, na.rm = TRUE),
      calibration_intercept = mean(intercept, na.rm = TRUE),
      calibration_slope = mean(slope, na.rm = TRUE)
    )

  data.frame(
    model = model_name,
    C = fmt_num_point(c_summary$C, digits = 3),
    balanced_accuracy = fmt_pct_point(c_summary$balanced_accuracy, digits = 1),
    sensitivity = fmt_pct_point(c_summary$sensitivity, digits = 1),
    specificity = fmt_pct_point(c_summary$specificity, digits = 1),
    ppv = fmt_pct_point(c_summary$ppv, digits = 1),
    npv = fmt_pct_point(c_summary$npv, digits = 1),
    brier = fmt_num_point(cal_summary$brier, digits = 2),
    calibration_intercept = fmt_num_point(cal_summary$calibration_intercept, digits = 2),
    calibration_slope = fmt_num_point(cal_summary$calibration_slope, digits = 2),
    stringsAsFactors = FALSE
  )
}



run_validation_step <- function(data, predictors, model_name) {
  log_progress("==============================")
  log_progress("Running ", model_name)
  log_progress("Predictors: ", paste(predictors, collapse = ", "))
  log_progress("==============================")

  analysis_all <- prepare_analysis_data(data, predictors)
  labelled_data <- prepare_labelled_analysis_data(data, predictors)
  unlabelled_data <- analysis_all %>% filter(is.na(chr))

  set.seed(seed)
  seeds <- sample(1:10000, outerRepeats)

  c_stat_nested <- data.frame()
  calibration_slopes <- data.frame()
  dca_clinical <- data.frame()
  dca_general <- data.frame()
  preds <- data.frame()
  coef_df <- data.frame()
  importance_df <- data.frame()
  tree_complexity_df <- data.frame()
  ssl_diagnostics <- data.frame()
  ssl_iteration_history <- data.frame()
  ssl_prediction_distribution <- data.frame()
  ssl_parameter_search_results <- data.frame()
  test_eval_data <- data.frame()
  training_performance_df <- data.frame()
  use_single_predictor_model <- length(predictors) == 1
  class_counts <- table(labelled_data$chr)
  effective_outer_folds <- min(outerFolds, min(class_counts))

  if (effective_outer_folds < 2) {
    stop("At least two observations are required in each outcome class for ", model_name)
  }

  if (effective_outer_folds < outerFolds) {
    log_progress("Reducing outer folds to ", effective_outer_folds, " for ", model_name,
                 " due to limited class counts in this subset")
  }

  total_outer_iterations <- outerRepeats * effective_outer_folds

  for (outer_rep in seq_len(outerRepeats)) {
    log_progress("Outer Repeat ", outer_rep, "/", outerRepeats, " for ", model_name)
    set.seed(seeds[outer_rep])
    outer_folds <- createFolds(labelled_data$chr, k = effective_outer_folds, list = TRUE, returnTrain = FALSE)

    for (outer_fold in seq_along(outer_folds)) {
      outer_iteration <- ((outer_rep - 1) * effective_outer_folds) + outer_fold
      log_progress("  Outer Fold ", outer_fold, "/", effective_outer_folds, " for ", model_name,
                   " (overall ", outer_iteration, "/", total_outer_iterations, ")")

      test_indices <- outer_folds[[outer_fold]]
      train_indices <- setdiff(seq_len(nrow(labelled_data)), test_indices)

      train <- labelled_data[train_indices, ]
      test <- labelled_data[test_indices, ]

      test_y <- test %>% subset(select = c(chr))
      test_x <- test %>% subset(select = c(-chr))
      test_imp <- cbind(impute_predictors(test_x), test_y)

      train_y <- train %>% subset(select = c(chr))
      train_x <- train %>% subset(select = c(-chr))
      train_imp <- cbind(impute_predictors(train_x), train_y)

      train_class_counts <- table(train_imp$chr)
      effective_inner_folds <- min(innerFolds, min(train_class_counts))

      if (effective_inner_folds < 2) {
        stop("At least two observations are required in each training outcome class for ", model_name,
             ", outer repeat ", outer_rep, ", outer fold ", outer_fold)
      }

      if (nrow(unlabelled_data) > 0) {
        unlabelled_x <- unlabelled_data %>% subset(select = c(-chr))
        unlabelled_imp <- impute_predictors(unlabelled_x)
      } else {
        unlabelled_imp <- data.frame(matrix(nrow = 0, ncol = ncol(train_imp) - 1))
        colnames(unlabelled_imp) <- names(train_imp)[names(train_imp) != "chr"]
      }

      nzv_filter <- if (run_near_zero_variance_filter) {
        apply_near_zero_variance_filter(
          train_imp = train_imp,
          test_imp = test_imp,
          unlabelled_imp = unlabelled_imp,
          model_name = model_name,
          outer_rep = outer_rep,
          outer_fold = outer_fold
        )
      } else {
        predictor_names <- setdiff(names(train_imp), "chr")
        list(
          train_imp = train_imp,
          test_imp = test_imp,
          unlabelled_imp = unlabelled_imp,
          predictors_before = length(predictor_names),
          predictors_after = length(predictor_names),
          excluded_predictors = character(0)
        )
      }

      train_imp <- nzv_filter$train_imp
      test_imp <- nzv_filter$test_imp
      unlabelled_imp <- nzv_filter$unlabelled_imp

      tune_grid <- expand.grid(
        alpha = glmnet_alpha_values,
        lambda = seq(0.001, 0.1, length = 100)
      )

      if (run_ssl_tuning) {
        ssl_tuning <- tune_ssl_configuration(
          train_imp = train_imp,
          unlabelled_imp = unlabelled_imp,
          use_single_predictor_model = use_single_predictor_model,
          tune_grid = tune_grid,
          effective_inner_folds = effective_inner_folds,
          model_name = model_name,
          outer_rep = outer_rep,
          outer_fold = outer_fold,
          seeds = seeds,
          ssl_base_family = ssl_base_family
        )
      } else {
        ssl_tuning <- list(
          best_tail_fraction = ssl_tail_fraction,
          best_max_negative_prob = ssl_max_negative_prob,
          best_pseudolabel_weight_cap_ratio = ssl_pseudolabel_weight_cap_ratio,
          best_alpha = NA_real_,
          best_lambda = NA_real_,
          search_results = data.frame()
        )
      }
      
      ssl_parameter_search_results <- dplyr::bind_rows(
        ssl_parameter_search_results,
        ssl_tuning$search_results
      )

      ssl_training <- run_ssl_training_loop(
        train_imp = train_imp,
        unlabelled_imp = unlabelled_imp,
        use_single_predictor_model = use_single_predictor_model,
        tune_grid = tune_grid,
        effective_inner_folds = effective_inner_folds,
        model_name = model_name,
        outer_rep = outer_rep,
        outer_fold = outer_fold,
        fixed_lambda = ssl_tuning$best_lambda,
        fixed_alpha = ssl_tuning$best_alpha,
        max_ssl_iterations = max_ssl_iterations,
        tail_fraction = ssl_tuning$best_tail_fraction,
        min_positive_prob = ssl_min_positive_prob,
        max_negative_prob = ssl_tuning$best_max_negative_prob,
        pseudolabel_weight_cap_ratio = ssl_tuning$best_pseudolabel_weight_cap_ratio,
        class_balanced_tails = ssl_class_balanced_tails,
        calibrate_pseudolabel_probs = ssl_calibrate_pseudolabel_probs,
        ssl_base_family = ssl_base_family
      )
      final_train_imp <- ssl_training$final_training_data

      ssl_diagnostics <- dplyr::bind_rows(ssl_diagnostics, data.frame(
        model = model_name,
        Fold = outer_fold,
        OuterRepeat = outer_rep,
        ssl_iterations_completed = ssl_training$successful_ssl_iterations,
        n_labelled_train = nrow(train_imp),
        events_labelled_train = sum(train_imp$chr == "X1"),
        tail_fraction = ssl_tuning$best_tail_fraction,
        min_positive_prob = ssl_min_positive_prob,
        max_negative_prob = ssl_tuning$best_max_negative_prob,
        pseudolabel_weight_cap_ratio = ssl_tuning$best_pseudolabel_weight_cap_ratio,
        class_balanced_tails = ssl_class_balanced_tails,
        calibrated_pseudolabel_probabilities = ssl_calibrate_pseudolabel_probs,
        ssl_base_family = ssl_base_family,
        ssl_base_best_alpha = ssl_tuning$best_alpha,
        ssl_base_best_lambda = ssl_tuning$best_lambda,
        predictors_before_nzv_filter = nzv_filter$predictors_before,
        predictors_after_nzv_filter = nzv_filter$predictors_after,
        nzv_excluded_predictors = paste(nzv_filter$excluded_predictors, collapse = "; "),
        n_pseudolabelled_train = ssl_training$pseudolabelled_rows,
        events_pseudolabelled_train = ssl_training$pseudolabelled_events,
        pseudolabelled_weight_train = ssl_training$pseudolabelled_weight,
        total_training_weight = sum(ssl_training$final_training_weights),
        n_final_train = nrow(final_train_imp),
        events_final_train = sum(final_train_imp$chr == "X1"),
        n_unlabelled_remaining = ssl_training$unlabelled_remaining,
        stringsAsFactors = FALSE
      ))

      if (nrow(ssl_training$ssl_iteration_log) > 0) {
        ssl_iteration_log <- ssl_training$ssl_iteration_log
        ssl_iteration_log$model <- model_name
        ssl_iteration_log$Fold <- outer_fold
        ssl_iteration_log$OuterRepeat <- outer_rep
        ssl_iteration_history <- dplyr::bind_rows(ssl_iteration_history, ssl_iteration_log)
      }

      if (nrow(ssl_training$ssl_prediction_distribution) > 0) {
        ssl_prediction_distribution <- dplyr::bind_rows(
          ssl_prediction_distribution,
          ssl_training$ssl_prediction_distribution
        )
      }

      log_progress(sum(is.na(test)), " missing data points in test set for ", model_name,
                   ", outer repeat ", outer_rep, ", outer fold ", outer_fold)

      for (final_family in final_model_families) {
        result_model_name <- format_result_model_name(model_name, final_family)
        log_progress("    Final learner ", canonical_final_model_family(final_family), " for ", model_name,
                     ", outer repeat ", outer_rep, ", outer fold ", outer_fold)

        final_fit <- fit_final_model(
          train_data = final_train_imp,
          final_model_family = final_family,
          effective_inner_folds = effective_inner_folds,
          model_name = result_model_name,
          outer_rep = outer_rep,
          outer_fold = outer_fold,
          use_single_predictor_model = use_single_predictor_model,
          observation_weights = ssl_training$final_training_weights,
          fixed_lambda = ssl_tuning$best_lambda,
          fixed_alpha = ssl_tuning$best_alpha
        )

        labelled_train_eval <- train_imp
        labelled_train_eval$PI <- predict_final_model_prob(
          final_fit$model_fit,
          labelled_train_eval,
          final_family,
          use_single_predictor_model
        )

        augmented_train_eval <- final_train_imp
        augmented_train_eval$PI <- predict_final_model_prob(
          final_fit$model_fit,
          augmented_train_eval,
          final_family,
          use_single_predictor_model
        )

        labelled_train_c <- tryCatch(
          concordance(glm(chr ~ PI, data = labelled_train_eval, family = "binomial"))$concordance,
          error = function(e) NA_real_
        )
        augmented_train_c <- tryCatch(
          concordance(glm(chr ~ PI, data = augmented_train_eval, family = "binomial"))$concordance,
          error = function(e) NA_real_
        )

        training_performance_df <- dplyr::bind_rows(training_performance_df, data.frame(
          model = result_model_name,
          base_model = model_name,
          final_model_family = canonical_final_model_family(final_family),
          OuterRepeat = outer_rep,
          Fold = outer_fold,
          labelled_train_c = labelled_train_c,
          augmented_train_c = augmented_train_c,
          n_labelled_train = nrow(train_imp),
          events_labelled_train = sum(train_imp$chr == "X1"),
          n_augmented_train = nrow(final_train_imp),
          events_augmented_train = sum(final_train_imp$chr == "X1"),
          stringsAsFactors = FALSE
        ))

        coef_df <- dplyr::bind_rows(
          coef_df,
          extract_step_coefficients(
            final_fit$model_fit,
            use_single_predictor_model,
            result_model_name,
            outer_rep,
            outer_fold,
            final_model_family = final_family
          )
        )

        importance_df <- dplyr::bind_rows(
          importance_df,
          extract_model_importance(
            final_fit$model_fit,
            result_model_name,
            outer_rep,
            outer_fold,
            final_model_family = final_family
          )
        )

        tree_complexity_df <- dplyr::bind_rows(
          tree_complexity_df,
          extract_tree_complexity(
            model_fit = final_fit$model_fit,
            train_data = final_train_imp,
            observation_weights = ssl_training$final_training_weights,
            model_name = result_model_name,
            outer_rep = outer_rep,
            outer_fold = outer_fold,
            final_model_family = final_family
          )
        )

        test_eval_block <- test_imp
        test_eval_block$model <- result_model_name
        test_eval_block$Subject_ID <- test_indices
        test_eval_block$OuterRepeat <- outer_rep
        test_eval_block$Fold <- outer_fold

        test_eval_block$PI <- predict_final_model_prob(
          final_fit$model_fit,
          test_eval_block,
          final_family,
          use_single_predictor_model
        )

        cutoff_selection <- data.frame(
          cutoff = seq(0, 1, by = 0.01),
          balanced_accuracy = NA_real_
        )

        for (cutoff_idx in seq_len(nrow(cutoff_selection))) {
          test_eval_block$chr_pred <- factor(
            ifelse(test_eval_block$PI >= cutoff_selection$cutoff[cutoff_idx], "X1", "X0"),
            levels = c("X0", "X1")
          )
          cm <- confusionMatrix(data = test_eval_block$chr_pred, reference = test_eval_block$chr, positive = "X1")
          cutoff_selection$balanced_accuracy[cutoff_idx] <- unname(cm$byClass["Balanced Accuracy"])
        }

        cutoff_selection <- cutoff_selection %>%
          filter(balanced_accuracy == max(balanced_accuracy, na.rm = TRUE)) %>%
          slice(1)

        if (nrow(cutoff_selection) == 0 || all(is.na(cutoff_selection$balanced_accuracy))) {
          log_progress("No valid balanced-accuracy cutoff for ", result_model_name,
                       ", outer repeat ", outer_rep, ", outer fold ", outer_fold,
                       "; falling back to 0.5")
          cutoff_selection <- data.frame(cutoff = 0.5, balanced_accuracy = NA_real_)
        }

        test_eval_block$chr_pred <- factor(
          ifelse(test_eval_block$PI >= cutoff_selection$cutoff, "X1", "X0"),
          levels = c("X0", "X1")
        )

        cm <- confusionMatrix(data = test_eval_block$chr_pred, reference = test_eval_block$chr, positive = "X1")
        test_eval_block <- test_eval_block %>%
          mutate(PI = case_when(PI == 0 ~ 0.001, PI == 1 ~ 0.999, TRUE ~ PI))
        test_eval_block$chr_num <- as.numeric(test_eval_block$chr) - 1
        test_eval_data <- dplyr::bind_rows(test_eval_data, test_eval_block)

        model_test <- glm(chr ~ PI, data = test_eval_block, family = "binomial")

        c_stat_nested <- dplyr::bind_rows(c_stat_nested, data.frame(
          model = result_model_name,
          base_model = model_name,
          final_model_family = canonical_final_model_family(final_family),
          C_test = concordance(model_test)$concordance,
          SE_test = concordance(model_test)$cvar,
          Fold = outer_fold,
          OuterRepeat = outer_rep,
          n_train = nrow(final_train_imp),
          events_train = sum(final_train_imp$chr == "X1"),
          n_test = nrow(test),
          events_test = sum(test$chr == "X1"),
          balanced_accuracy = unname(cm$byClass["Balanced Accuracy"]),
          sensitivity = unname(cm$byClass["Sensitivity"]),
          specificity = unname(cm$byClass["Specificity"]),
          ppv = unname(cm$byClass["Pos Pred Value"]),
          npv = unname(cm$byClass["Neg Pred Value"]),
          precision = unname(cm$byClass["Precision"]),
          recall = unname(cm$byClass["Recall"]),
          f1 = unname(cm$byClass["F1"])
        ))

        cal <- pred_val_probs(
          binary_outcome = test_eval_block$chr_num,
          Prob = test_eval_block$PI,
          cal_plot = FALSE
        )

        calibration_slopes <- dplyr::bind_rows(calibration_slopes, data.frame(
          model = result_model_name,
          base_model = model_name,
          final_model_family = canonical_final_model_family(final_family),
          fold = outer_fold,
          OuterRepeat = outer_rep,
          intercept = cal$CalInt,
          slope = cal$CalSlope,
          brier = cal$BrierScore
        ))

        dca_input <- data.frame(
          model = result_model_name,
          Subject_ID = test_indices,
          obs = as.numeric(test_eval_block$chr) - 1,
          pred = as.numeric(test_eval_block$PI),
          chr_pred = as.numeric(test_eval_block$chr_pred) - 1
        )

        dca_assessment <- dcurves::dca(obs ~ pred,
          data = dca_input,
          prevalence = 0.192,
          thresholds = seq(0, 0.5, 0.01)
        ) %>% as_tibble()

        dca_clinical <- dplyr::bind_rows(dca_clinical, data.frame(
          model = result_model_name,
          label = dca_assessment$label,
          n = dca_assessment$n,
          outer_repeat = rep(outer_rep, nrow(dca_assessment)),
          outer_fold = rep(outer_fold, nrow(dca_assessment)),
          threshold = dca_assessment$threshold,
          pos_rate = dca_assessment$pos_rate,
          net_benefit = dca_assessment$net_benefit
        ))

        dca_assessment <- dcurves::dca(obs ~ pred,
          data = dca_input,
          prevalence = 0.017,
          thresholds = seq(0, 0.5, 0.001)
        ) %>% as_tibble()

        dca_general <- dplyr::bind_rows(dca_general, data.frame(
          model = result_model_name,
          label = dca_assessment$label,
          n = dca_assessment$n,
          outer_repeat = rep(outer_rep, nrow(dca_assessment)),
          outer_fold = rep(outer_fold, nrow(dca_assessment)),
          threshold = dca_assessment$threshold,
          pos_rate = dca_assessment$pos_rate,
          net_benefit = dca_assessment$net_benefit
        ))

        preds <- dplyr::bind_rows(preds, dca_input)
      }
    }
  }

  boot_df <- preds %>%
    dplyr::group_by(model, Subject_ID) %>%
    dplyr::summarise(
      obs = first(obs),
      pred = mean(pred),
      chr_pred = mean(chr_pred),
      .groups = "drop"
    )

  if (run_bootstrap) {
    results_new <- bind_rows(lapply(unique(boot_df$model), function(result_model_name) {
      summarise_boot_metrics(
        boot_df = boot_df %>% dplyr::filter(model == result_model_name) %>% dplyr::select(-model),
        model_name = result_model_name,
        B = 2000
      )
    }))
  } else {
    results_new <- bind_rows(lapply(unique(c_stat_nested$model), function(result_model_name) {
      summarise_point_metrics(
        c_stat_nested = c_stat_nested %>% dplyr::filter(model == result_model_name),
        calibration_slopes = calibration_slopes %>% dplyr::filter(model == result_model_name),
        model_name = result_model_name
      )
    }))
  }

  log_progress("Completed ", model_name)

  list(
    results_new = results_new,
    c_stat_nested = c_stat_nested,
    calibration_slopes = calibration_slopes,
    dca_clinical = dca_clinical,
    dca_general = dca_general,
    preds = preds,
    boot_df = boot_df,
    coef_df = coef_df,
    importance_df = importance_df,
    tree_complexity_df = tree_complexity_df,
    training_performance_df = training_performance_df,
    ssl_diagnostics = ssl_diagnostics,
    ssl_iteration_history = ssl_iteration_history,
    ssl_prediction_distribution = ssl_prediction_distribution,
    ssl_parameter_search_results = ssl_parameter_search_results,
    test_eval_data = test_eval_data
  )
}

step_results <- list()
selected_model_steps <- model_steps

if (length(target_models) > 0) {
  selected_model_steps <- model_steps[names(model_steps) %in% target_models]

  if (length(selected_model_steps) == 0) {
    stop("No model names from TARGET_MODELS matched model_steps")
  }
}

if (run_main_steps) {
  if ("model" %in% names(results_summary)) {
    step_patterns <- paste0("^(", paste(names(selected_model_steps), collapse = "|"), ")(?:__final_.*)?$")
    results_summary <- results_summary %>%
      dplyr::filter(!grepl(step_patterns, model))
  }

  for (step_idx in seq_along(selected_model_steps)) {
    step_name <- names(selected_model_steps)[step_idx]
    log_progress("Starting step ", step_idx, "/", length(selected_model_steps), ": ", step_name)
    step_fit <- run_validation_step(
      data = ENTER,
      predictors = selected_model_steps[[step_name]],
      model_name = step_name
    )

    step_results[[step_name]] <- step_fit
    if (!is.null(step_fit$results_new)) {
      results_summary <- rbind(results_summary, step_fit$results_new)
      write.csv(results_summary, file.path(output_dir, "results_summary_partial.csv"), row.names = FALSE)
    }
    log_progress("Finished step ", step_idx, "/", length(selected_model_steps), ": ", step_name)
  }
}

sensitivity_step_results <- list()

if (run_sensitivity_steps) {
  speech_trimmed <- read_csv("/Users/domoliver/Library/CloudStorage/Dropbox/Work/ENTER/CAARMS Prediction/speech.csv", show_col_types = FALSE) %>%
    dplyr::rename(enterid = `ENTER ID`) %>%
    dplyr::mutate(enterid = as.character(enterid)) %>%
    dplyr::select(enterid, Coherence, PCSYNz) %>%
    dplyr::distinct(enterid, .keep_all = TRUE)

  ENTER <- ENTER %>%
    dplyr::mutate(enterid = as.character(enterid))

  ENTER_speech_subset <- ENTER %>%
    dplyr::left_join(speech_trimmed, by = "enterid") %>%
    dplyr::filter(!is.na(Coherence) & !is.na(PCSYNz))

  speech_chr_summary <- ENTER_speech_subset %>%
    dplyr::mutate(
      chr_label = dplyr::case_when(
        chr == 1 ~ "CHR-P+",
        chr == 0 ~ "CHR-P-",
        TRUE ~ "Unlabelled"
      )
    ) %>%
    dplyr::count(chr_label, name = "n") %>%
    dplyr::arrange(chr_label)

  write.csv(
    speech_chr_summary,
    file.path(output_dir, "speech_chr_summary.csv"),
    row.names = FALSE
  )

  log_progress("Speech sensitivity subset size: ", nrow(ENTER_speech_subset), " participants")
  log_progress("Saved speech chr summary to ", file.path(output_dir, "speech_chr_summary.csv"))

  sensitivity_steps <- list(
    Step_5_full_model_speech_subset = sensitivity_base_predictors,
    Step_6_full_model_plus_speech = c(sensitivity_base_predictors, sensitivity_speech_predictors)
  )

  if ("model" %in% names(results_summary)) {
    results_summary <- results_summary %>%
      dplyr::filter(
        !grepl("^(Step_5_full_model_speech_subset|Step_6_full_model_plus_speech)(?:__final_.*)?$", model)
      )
  }

  for (step_idx in seq_along(sensitivity_steps)) {
    step_name <- names(sensitivity_steps)[step_idx]
    log_progress("Starting sensitivity step ", step_idx, "/", length(sensitivity_steps), ": ", step_name)
    step_fit <- run_validation_step(
      data = ENTER_speech_subset,
      predictors = sensitivity_steps[[step_name]],
      model_name = step_name
    )

    sensitivity_step_results[[step_name]] <- step_fit
    if (!is.null(step_fit$results_new)) {
      results_summary <- rbind(results_summary, step_fit$results_new)
      write.csv(results_summary, file.path(output_dir, "results_summary_partial.csv"), row.names = FALSE)
    }
    log_progress("Finished sensitivity step ", step_idx, "/", length(sensitivity_steps), ": ", step_name)
  }
} else if ("model" %in% names(results_summary)) {
  results_summary <- results_summary %>%
    dplyr::filter(
      !grepl("^(Step_5_full_model_speech_subset|Step_6_full_model_plus_speech)(?:__final_.*)?$", model)
    )
}

export_step_results <- c(step_results, sensitivity_step_results)

if (length(export_step_results) > 0) {
  log_progress("Preparing pooled test-set predictions for decision curve analysis")

  pooled_predictions <- bind_rows(lapply(names(export_step_results), function(step_name) {
    export_step_results[[step_name]]$boot_df
  })) %>%
    dplyr::select(model, Subject_ID, obs, pred, chr_pred)

  write.csv(pooled_predictions, predictions_output_path, row.names = FALSE)
  log_progress("Saved pooled test-set predictions to ", predictions_output_path)

  decision_curve_general <- bind_rows(lapply(names(export_step_results), function(step_name) {
    export_step_results[[step_name]]$dca_general
  })) %>%
    summarise_dca_output()

  write.csv(decision_curve_general, decision_curve_general_output_path, row.names = FALSE)
  log_progress("Saved general-population decision curve summary to ", decision_curve_general_output_path)

  decision_curve_general_plot <- plot_decision_curve_summary(
    decision_curve_general,
    title_text = "Decision Curve Analysis",
    subtitle_text = "General population prevalence 1.7%"
  )
  ggsave(
    filename = decision_curve_general_plot_path,
    plot = decision_curve_general_plot,
    width = 11,
    height = 7,
    dpi = 300
  )
  log_progress("Saved general-population decision curve plot to ", decision_curve_general_plot_path)

  decision_curve_clinical <- bind_rows(lapply(names(export_step_results), function(step_name) {
    export_step_results[[step_name]]$dca_clinical
  })) %>%
    summarise_dca_output()

  write.csv(decision_curve_clinical, decision_curve_clinical_output_path, row.names = FALSE)
  log_progress("Saved clinical-sample decision curve summary to ", decision_curve_clinical_output_path)

  decision_curve_clinical_plot <- plot_decision_curve_summary(
    decision_curve_clinical,
    title_text = "Decision Curve Analysis",
    subtitle_text = "Clinical sample prevalence 19.2%"
  )
  ggsave(
    filename = decision_curve_clinical_plot_path,
    plot = decision_curve_clinical_plot,
    width = 11,
    height = 7,
    dpi = 300
  )
  log_progress("Saved clinical-sample decision curve plot to ", decision_curve_clinical_plot_path)

  ssl_diagnostics_all <- bind_rows(lapply(names(export_step_results), function(step_name) {
    export_step_results[[step_name]]$ssl_diagnostics
  }))
  write.csv(ssl_diagnostics_all, ssl_diagnostics_output_path, row.names = FALSE)
  log_progress("Saved SSL training diagnostics to ", ssl_diagnostics_output_path)

  ssl_iterations_all <- bind_rows(lapply(names(export_step_results), function(step_name) {
    export_step_results[[step_name]]$ssl_iteration_history
  }))
  write.csv(ssl_iterations_all, ssl_iterations_output_path, row.names = FALSE)
  log_progress("Saved SSL iteration history to ", ssl_iterations_output_path)

  ssl_prediction_distribution_all <- bind_rows(lapply(names(export_step_results), function(step_name) {
    export_step_results[[step_name]]$ssl_prediction_distribution
  }))
  write.csv(ssl_prediction_distribution_all, ssl_prediction_distribution_output_path, row.names = FALSE)
  log_progress("Saved SSL prediction distribution to ", ssl_prediction_distribution_output_path)

  ssl_tuning_all <- bind_rows(lapply(names(export_step_results), function(step_name) {
    export_step_results[[step_name]]$ssl_parameter_search_results
  }))
  write.csv(ssl_tuning_all, ssl_tuning_output_path, row.names = FALSE)
  log_progress("Saved SSL parameter search results to ", ssl_tuning_output_path)

  tree_complexity_all <- bind_rows(lapply(names(export_step_results), function(step_name) {
    export_step_results[[step_name]]$tree_complexity_df
  }))
  if (nrow(tree_complexity_all) > 0) {
    write.csv(tree_complexity_all, tree_complexity_output_path, row.names = FALSE)
    log_progress("Saved tree complexity diagnostics to ", tree_complexity_output_path)
  } else {
    log_progress("No tree complexity diagnostics available for export")
  }

  training_performance_all <- bind_rows(lapply(names(export_step_results), function(step_name) {
    export_step_results[[step_name]]$training_performance_df
  }))
  if (nrow(training_performance_all) > 0) {
    write.csv(training_performance_all, training_performance_output_path, row.names = FALSE)
    log_progress("Saved training performance diagnostics to ", training_performance_output_path)
  } else {
    log_progress("No training performance diagnostics available for export")
  }

  importance_all <- bind_rows(lapply(names(export_step_results), function(step_name) {
    export_step_results[[step_name]]$importance_df
  }))
  if (nrow(importance_all) > 0) {
    write.csv(importance_all, importance_output_path, row.names = FALSE)
    log_progress("Saved per-fit model variable importance to ", importance_output_path)

    pooled_importance <- summarise_pooled_importance(importance_all)
    write.csv(pooled_importance, pooled_importance_output_path, row.names = FALSE)
    log_progress("Saved pooled model variable importance to ", pooled_importance_output_path)

    importance_plot <- plot_ranked_model_importance(pooled_importance)
    if (!is.null(importance_plot)) {
      ggsave(
        filename = importance_plot_output_path,
        plot = importance_plot,
        width = 14,
        height = 10,
        dpi = 300
      )
      log_progress("Saved ranked model importance plot to ", importance_plot_output_path)
    }
  } else {
    log_progress("No non-glmnet model importance available for export")
  }

  coefficients_all <- bind_rows(lapply(names(export_step_results), function(step_name) {
    export_step_results[[step_name]]$coef_df
  }))
  if (nrow(coefficients_all) > 0) {
    write.csv(coefficients_all, coefficient_output_path, row.names = FALSE)
    log_progress("Saved per-fit coefficients to ", coefficient_output_path)

    pooled_coefficients <- summarise_pooled_coefficients(coefficients_all)
    write.csv(pooled_coefficients, pooled_coefficient_output_path, row.names = FALSE)
    log_progress("Saved pooled implementation coefficients to ", pooled_coefficient_output_path)

    harmonised_summary_table <- create_harmonised_model_summary(
      pooled_coefficients = pooled_coefficients,
      pooled_importance = if (exists("pooled_importance")) pooled_importance else data.frame()
    )
    if (nrow(harmonised_summary_table) > 0) {
      write.csv(harmonised_summary_table, harmonised_model_summary_output_path, row.names = FALSE)
      log_progress("Saved harmonised model summary table to ", harmonised_model_summary_output_path)
    }

    all_test_eval_data <- bind_rows(lapply(names(export_step_results), function(step_name) {
      export_step_results[[step_name]]$test_eval_data
    }))

    pooled_performance <- bind_rows(lapply(unique(pooled_coefficients$model), function(result_model_name) {
      pooled_coef_model <- pooled_coefficients %>%
        filter(model == result_model_name)
      test_eval_subset <- all_test_eval_data %>%
        dplyr::filter(model == result_model_name)

      if (nrow(pooled_coef_model) == 0 || nrow(test_eval_subset) == 0) {
        return(data.frame())
      }

      pooled_eval <- tryCatch(
        score_pooled_implementation(
          test_eval_data = test_eval_subset,
          pooled_coef_model = pooled_coef_model
        ),
        error = function(e) {
          warning(
            "Skipping pooled implementation scoring for ", result_model_name,
            ": ", conditionMessage(e)
          )
          NULL
        }
      )

      if (is.null(pooled_eval) || is.null(pooled_eval$performance)) {
        return(data.frame())
      }

      pooled_eval$performance
    }))

    if (nrow(pooled_performance) > 0) {
      write.csv(pooled_performance, pooled_performance_output_path, row.names = FALSE)
      log_progress("Saved pooled implementation performance to ", pooled_performance_output_path)
    } else {
      log_progress("No pooled implementation performance available for export")
    }
  } else {
    log_progress("No coefficient-based models available for pooled implementation export")
  }
}

if (!run_bootstrap && nrow(results_summary) == 0 && file.exists(file.path(output_dir, "results_summary_partial.csv"))) {
  results_summary <- read.csv(file.path(output_dir, "results_summary_partial.csv"), stringsAsFactors = FALSE)

  if (!run_sensitivity_steps && "model" %in% names(results_summary)) {
    results_summary <- results_summary %>%
      dplyr::filter(
        !model %in% c(
          "Step_5_full_model_speech_subset",
          "Step_6_full_model_plus_speech"
        )
      )
  }
}

write.csv(results_summary, file.path(output_dir, "results_summary.csv"), row.names = FALSE)
log_progress("Saved results_summary to ", file.path(output_dir, "results_summary.csv"))

results_summary
