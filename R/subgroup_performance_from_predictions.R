library(tidyverse)
library(readxl)
library(predRupdate)
library(survival)

work_root <- "/Users/domoliver/Library/CloudStorage/Dropbox/Work"
project_root <- file.path(work_root, "ENTER/CAARMS Prediction")
analyses_dir <- file.path(project_root, "Analyses")

output_dir_env <- Sys.getenv("SUBGROUP_OUTPUT_DIR", unset = "")
output_dir <- if (nzchar(output_dir_env)) {
  if (grepl("^/", output_dir_env)) output_dir_env else file.path(analyses_dir, output_dir_env)
} else {
  file.path(analyses_dir, "fixed_common_ssl_step5_all_learners")
}

target_model_pattern <- Sys.getenv("SUBGROUP_MODEL_PATTERN", unset = "^Step_5_full_model")

bootstrap_B <- suppressWarnings(as.integer(Sys.getenv("SUBGROUP_BOOTSTRAP_B", unset = "2000")))
if (is.na(bootstrap_B) || bootstrap_B <= 0) {
  bootstrap_B <- 2000
}

bootstrap_seed <- suppressWarnings(as.integer(Sys.getenv("SUBGROUP_BOOTSTRAP_SEED", unset = "123")))
if (is.na(bootstrap_seed)) {
  bootstrap_seed <- 123
}

metric_names <- c(
  "C",
  "balanced_accuracy",
  "sensitivity",
  "specificity",
  "ppv",
  "npv",
  "brier",
  "calibration_intercept",
  "calibration_slope"
)

percent_metrics <- c("balanced_accuracy", "sensitivity", "specificity", "ppv", "npv")

load_labelled_enter_subgroups <- function() {
  source(file.path("R", "regenerate_sociodemographics_table.R"))
  revised <- load_enter_data()
  return(
    revised %>%
      filter(!is.na(chr)) %>%
      mutate(
        Subject_ID = row_number(),
        age = suppressWarnings(as.numeric(age)),
        age_group = case_when(
          age < 18 ~ "Under 18",
          age >= 18 ~ "18 and over",
          TRUE ~ NA_character_
        ),
        gender = as.character(gender),
        ethnicity = as.character(ethnicity),
        country = as.character(country)
      ) %>%
      select(Subject_ID, age, gender, ethnicity, age_group, country, chr)
  )

  enter_uk <- read_excel(
    file.path(work_root, "Papers/Published/CBD Acceptability CHR-P/ENTER Finalised CAARMS Categories.xlsx")
  ) %>%
    mutate(
      caarms_outcome_arms = case_when(
        `Baseline CAARMS` != "No Risk" &
          `Baseline CAARMS` != "No risk" &
          `Baseline CAARMS` != "GRD" &
          `Baseline CAARMS` != "Psychosis Threshold Met" ~ 1,
        TRUE ~ 0
      ),
      caarms_outcome_psy = case_when(
        `Baseline CAARMS` != "Psychosis Threshold Met" ~ 1,
        TRUE ~ 0
      ),
      enterid = as.character(`ENTER ID`)
    ) %>%
    dplyr::select(enterid, caarms_outcome_arms, caarms_outcome_psy)

  enter_uk_web <- readr::read_csv(
    file.path(project_root, "ENTEROnline_DATA_2025-05-30_1704.csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::select(enterid)
  valid_uk <- readr::read_csv(
    file.path(project_root, "Validations_04.02.25.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(enter_id = as.character(enter_id)) %>%
    filter(manual_validation == "Valid")
  enter_uk_web <- enter_uk_web %>%
    mutate(enterid = as.character(enterid)) %>%
    filter(enterid %in% valid_uk$enter_id)

  enter_pps_uk <- readr::read_csv(
    file.path(project_root, "PPS_ENTER_all_clean_020625.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(enterid = as.character(enterid)) %>%
    dplyr::select(enterid, age, gender, ethnicity)

  enter_uk <- merge(enter_uk, enter_pps_uk, by = "enterid", all = TRUE)
  enter_uk <- merge(enter_uk, enter_uk_web, by = "enterid", all.y = TRUE) %>%
    mutate(country = "UK")

  enter_pv <- read_excel(
    file.path(work_root, "ENTER/Bipolar Prediction/ENTER_UK_Pavia_20.02.25_BPSS_SIBARS.xlsx"),
    sheet = "ENTER_Pavia"
  ) %>%
    mutate(enterid = as.character(enterid)) %>%
    dplyr::select(enterid, caarms_outcome_arms, caarms_outcome_psy)

  enter_pv_web <- readr::read_csv(
    file.path(project_root, "ENTERScreenerIT_DATA_2025-06-02_1322.csv"),
    show_col_types = FALSE
  ) %>%
    dplyr::select(enterid)
  valid_pv <- readr::read_csv(
    file.path(project_root, "Italy validations 22.03.25.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(enter_id = as.character(enter_id)) %>%
    filter(manual_validation == "Valid")
  enter_pv_web <- enter_pv_web %>%
    mutate(enterid = as.character(enterid)) %>%
    filter(enterid %in% valid_pv$enter_id)

  enter_pps_pv <- readr::read_csv(
    file.path(work_root, "PPS/ENTER/ENTER_PPS_Processing_IT/PPS_ENTER_IT_clean_180225.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(enterid = as.character(enterid)) %>%
    dplyr::select(enterid, age, gender, ethnicity)

  enter_pv <- merge(enter_pv, enter_pps_pv, by = "enterid", all = TRUE)
  enter_pv <- merge(enter_pv, enter_pv_web, by = "enterid", all.y = TRUE) %>%
    mutate(country = "Italy (PV)")

  bind_rows(enter_uk, enter_pv) %>%
    mutate(
      chr = case_when(
        caarms_outcome_arms == 1 | caarms_outcome_psy == 1 ~ 1,
        caarms_outcome_arms == 0 & caarms_outcome_psy == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select(age, gender, ethnicity, country, chr) %>%
    filter(!is.na(chr)) %>%
    mutate(
      Subject_ID = row_number(),
      age = suppressWarnings(as.numeric(as.character(age))),
      age_group = case_when(
        age < 18 ~ "Under 18",
        age >= 18 ~ "18 and over",
        TRUE ~ NA_character_
      ),
      gender = as.character(gender),
      ethnicity = as.character(ethnicity),
      country = as.character(country)
    )
}

boot_summary <- function(x) {
  valid <- x[!is.na(x) & is.finite(x)]

  if (length(valid) == 0) {
    return(c(Mean = NA_real_, CI_low = NA_real_, CI_high = NA_real_))
  }

  q <- quantile(valid, c(0.025, 0.975), na.rm = TRUE, names = FALSE)
  c(
    Mean = mean(valid, na.rm = TRUE),
    CI_low = unname(q[1]),
    CI_high = unname(q[2])
  )
}

fmt_num <- function(mean, low, high, digits = 3) {
  values <- c(mean, low, high)
  if (any(is.na(values)) || any(!is.finite(values))) {
    return(NA_character_)
  }

  paste0(
    formatC(mean, format = "f", digits = digits), " (",
    formatC(low, format = "f", digits = digits), "-",
    formatC(high, format = "f", digits = digits), ")"
  )
}

fmt_pct <- function(mean, low, high, digits_mean = 1, digits_ci = 1) {
  values <- c(mean, low, high)
  if (any(is.na(values)) || any(!is.finite(values))) {
    return(NA_character_)
  }

  paste0(
    formatC(mean * 100, format = "f", digits = digits_mean), "% (",
    formatC(low * 100, format = "f", digits = digits_ci), "%-",
    formatC(high * 100, format = "f", digits = digits_ci), "%)"
  )
}

safe_calibration <- function(obs, pred) {
  valid <- !is.na(obs) & !is.na(pred)
  obs <- obs[valid]
  pred <- pred[valid]
  pred <- pmin(pmax(pred, 0.001), 0.999)

  if (length(unique(obs)) < 2) {
    return(list(
      C = NA_real_,
      brier = NA_real_,
      calibration_intercept = NA_real_,
      calibration_slope = NA_real_
    ))
  }

  cal <- tryCatch(
    suppressWarnings(pred_val_probs(binary_outcome = obs, Prob = pred, cal_plot = FALSE)),
    error = function(e) NULL
  )

  c_fit <- tryCatch(
    {
      glm_fit <- suppressWarnings(glm(obs ~ pred, family = "binomial"))
      survival::concordance(glm_fit)$concordance
    },
    error = function(e) NA_real_
  )

  list(
    C = c_fit,
    brier = if (is.null(cal)) NA_real_ else cal$BrierScore[1],
    calibration_intercept = if (is.null(cal)) NA_real_ else cal$CalInt[1],
    calibration_slope = if (is.null(cal)) NA_real_ else cal$CalSlope[1]
  )
}

classification_metrics <- function(obs, chr_pred) {
  valid <- !is.na(obs) & !is.na(chr_pred)
  obs <- obs[valid]
  chr_pred <- chr_pred[valid]

  if (length(obs) == 0) {
    return(list(
      balanced_accuracy = NA_real_,
      sensitivity = NA_real_,
      specificity = NA_real_,
      ppv = NA_real_,
      npv = NA_real_
    ))
  }

  pred_class <- ifelse(chr_pred >= 0.5, 1, 0)
  tp <- sum(pred_class == 1 & obs == 1, na.rm = TRUE)
  tn <- sum(pred_class == 0 & obs == 0, na.rm = TRUE)
  fp <- sum(pred_class == 1 & obs == 0, na.rm = TRUE)
  fn <- sum(pred_class == 0 & obs == 1, na.rm = TRUE)

  sensitivity <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  specificity <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
  ppv <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
  npv <- if ((tn + fn) > 0) tn / (tn + fn) else NA_real_
  balanced_accuracy <- if (all(!is.na(c(sensitivity, specificity)))) {
    mean(c(sensitivity, specificity))
  } else {
    NA_real_
  }

  list(
    balanced_accuracy = balanced_accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    ppv = ppv,
    npv = npv
  )
}

calculate_metrics <- function(df) {
  obs <- as.numeric(as.character(df$obs))
  pred <- as.numeric(df$pred)
  chr_pred <- as.numeric(df$chr_pred)

  class_summary <- classification_metrics(obs, chr_pred)
  calibration_summary <- safe_calibration(obs, pred)

  list(
    C = calibration_summary$C,
    balanced_accuracy = class_summary$balanced_accuracy,
    sensitivity = class_summary$sensitivity,
    specificity = class_summary$specificity,
    ppv = class_summary$ppv,
    npv = class_summary$npv,
    brier = calibration_summary$brier,
    calibration_intercept = calibration_summary$calibration_intercept,
    calibration_slope = calibration_summary$calibration_slope
  )
}

summarise_metric_matrix <- function(metric_matrix) {
  bind_rows(lapply(seq_along(metric_names), function(i) {
    values <- boot_summary(metric_matrix[, i])
    tibble(
      Metric = metric_names[i],
      Mean = values[["Mean"]],
      CI_low = values[["CI_low"]],
      CI_high = values[["CI_high"]]
    )
  }))
}

bootstrap_subgroup_metrics <- function(df, B = 2000) {
  n <- nrow(df)
  obs <- as.numeric(as.character(df$obs))
  metric_matrix <- matrix(
    NA_real_,
    nrow = B,
    ncol = length(metric_names),
    dimnames = list(NULL, metric_names)
  )

  if (n == 0 || length(unique(obs[!is.na(obs)])) < 2) {
    return(summarise_metric_matrix(metric_matrix))
  }

  for (b in seq_len(B)) {
    attempt <- 0

    repeat {
      attempt <- attempt + 1
      idx <- sample(seq_len(n), size = n, replace = TRUE)
      sampled_obs <- obs[idx]

      if (length(unique(sampled_obs[!is.na(sampled_obs)])) > 1 || attempt >= 100) {
        break
      }
    }

    if (length(unique(sampled_obs[!is.na(sampled_obs)])) < 2) {
      next
    }

    metrics <- calculate_metrics(df[idx, , drop = FALSE])
    metric_matrix[b, ] <- unlist(metrics[metric_names], use.names = FALSE)
  }

  summarise_metric_matrix(metric_matrix)
}

format_bootstrap_metrics <- function(summary_df) {
  formatted <- list()
  raw <- list()

  for (metric in metric_names) {
    metric_row <- summary_df %>% filter(Metric == metric)
    metric_mean <- metric_row$Mean
    metric_low <- metric_row$CI_low
    metric_high <- metric_row$CI_high

    formatted[[metric]] <- if (metric %in% percent_metrics) {
      fmt_pct(metric_mean, metric_low, metric_high, digits_mean = 1, digits_ci = 1)
    } else if (metric == "C") {
      fmt_num(metric_mean, metric_low, metric_high, digits = 3)
    } else {
      fmt_num(metric_mean, metric_low, metric_high, digits = 2)
    }

    raw[[paste0(metric, "_mean")]] <- metric_mean
    raw[[paste0(metric, "_ci_low")]] <- metric_low
    raw[[paste0(metric, "_ci_high")]] <- metric_high
  }

  bind_cols(as_tibble(formatted), as_tibble(raw))
}

summarise_subgroups <- function(preds, subgroup_data, subgroup_var) {
  preds %>%
    left_join(subgroup_data, by = "Subject_ID") %>%
    mutate(subgroup = .data[[subgroup_var]]) %>%
    group_by(model, subgroup) %>%
    group_modify(~ {
      obs <- as.numeric(as.character(.x$obs))
      events <- sum(obs == 1, na.rm = TRUE)
      non_events <- sum(obs == 0, na.rm = TRUE)
      subgroup_label <- ifelse(is.na(.y$subgroup), "NA", as.character(.y$subgroup))

      message(
        "Bootstrapping ", subgroup_var, "=", subgroup_label,
        " for ", .y$model, " (n=", nrow(.x), ", B=", bootstrap_B, ")"
      )

      bootstrap_summary <- bootstrap_subgroup_metrics(.x, B = bootstrap_B)

      bind_cols(tibble(
        subgroup_type = subgroup_var,
        n = nrow(.x),
        events = events,
        non_events = non_events,
        event_rate = mean(obs == 1, na.rm = TRUE),
        estimable = length(unique(obs)) > 1,
        sparse_stratum = events < 10 | non_events < 10
      ), format_bootstrap_metrics(bootstrap_summary))
    }) %>%
    ungroup() %>%
    arrange(model, subgroup)
}

set.seed(bootstrap_seed)

labelled_subgroups <- load_labelled_enter_subgroups()
# regenerate_sociodemographics_table.R defines its own output_dir; restore the
# subgroup output directory before reading predictions and writing results.
output_dir_env <- Sys.getenv("SUBGROUP_OUTPUT_DIR", unset = "")
output_dir <- if (nzchar(output_dir_env)) {
  if (grepl("^/", output_dir_env)) output_dir_env else file.path(analyses_dir, output_dir_env)
} else {
  file.path(analyses_dir, "fixed_common_ssl_step5_all_learners")
}
preds <- read.csv(file.path(output_dir, "test_set_predictions.csv"), stringsAsFactors = FALSE) %>%
  mutate(
    Subject_ID = as.integer(Subject_ID),
    obs = as.numeric(as.character(obs)),
    pred = as.numeric(pred),
    chr_pred = as.numeric(chr_pred)
  )

if (nzchar(target_model_pattern)) {
  preds <- preds %>%
    filter(grepl(target_model_pattern, model))
}

if (nrow(preds) == 0) {
  stop("No predictions matched SUBGROUP_MODEL_PATTERN in ", output_dir)
}

preds <- preds %>%
  group_by(model, Subject_ID) %>%
  summarise(
    obs = first(obs),
    pred = mean(pred, na.rm = TRUE),
    chr_pred = mean(chr_pred, na.rm = TRUE),
    .groups = "drop"
  )

message(
  "Bootstrapping subgroup performance for ", n_distinct(preds$model),
  " model(s) from ", output_dir,
  " with B=", bootstrap_B,
  " and seed=", bootstrap_seed
)

gender_results <- summarise_subgroups(preds, labelled_subgroups, "gender")
ethnicity_results <- summarise_subgroups(preds, labelled_subgroups, "ethnicity")
age_results <- summarise_subgroups(preds, labelled_subgroups, "age_group")
country_results <- summarise_subgroups(preds, labelled_subgroups, "country")

prediction_subgroups <- preds %>%
  distinct(Subject_ID, obs) %>%
  left_join(
    labelled_subgroups %>% dplyr::select(Subject_ID, gender, ethnicity, age_group, country),
    by = "Subject_ID"
  )

subgroup_counts <- bind_rows(
  prediction_subgroups %>%
    count(gender, obs, name = "n") %>%
    mutate(subgroup_type = "gender", subgroup = gender) %>%
    dplyr::select(subgroup_type, subgroup, obs, n),
  prediction_subgroups %>%
    count(ethnicity, obs, name = "n") %>%
    mutate(subgroup_type = "ethnicity", subgroup = ethnicity) %>%
    dplyr::select(subgroup_type, subgroup, obs, n),
  prediction_subgroups %>%
    count(age_group, obs, name = "n") %>%
    mutate(subgroup_type = "age_group", subgroup = age_group) %>%
    dplyr::select(subgroup_type, subgroup, obs, n),
  prediction_subgroups %>%
    count(country, obs, name = "n") %>%
    mutate(subgroup_type = "country", subgroup = country) %>%
    dplyr::select(subgroup_type, subgroup, obs, n)
) %>%
  mutate(outcome = ifelse(obs == 1, "event", "non_event")) %>%
  dplyr::select(subgroup_type, subgroup, outcome, n)

combined_results <- bind_rows(gender_results, ethnicity_results, age_results, country_results)

write.csv(gender_results, file.path(output_dir, "subgroup_performance_gender.csv"), row.names = FALSE)
write.csv(ethnicity_results, file.path(output_dir, "subgroup_performance_ethnicity.csv"), row.names = FALSE)
write.csv(age_results, file.path(output_dir, "subgroup_performance_age.csv"), row.names = FALSE)
write.csv(country_results, file.path(output_dir, "subgroup_performance_country.csv"), row.names = FALSE)
write.csv(combined_results, file.path(output_dir, "subgroup_performance_all.csv"), row.names = FALSE)
write.csv(subgroup_counts, file.path(output_dir, "subgroup_counts.csv"), row.names = FALSE)

message("Saved subgroup performance outputs to ", output_dir)
