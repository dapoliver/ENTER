library(dplyr)
library(readr)
library(readxl)
library(tidyr)

project_root <- Sys.getenv("ENTER_PROJECT_ROOT", unset = getwd())
work_root <- Sys.getenv("ENTER_WORK_ROOT", unset = dirname(dirname(project_root)))
repo_root <- Sys.getenv("ENTER_REPO_ROOT", unset = file.path(project_root, "Analyses"))
output_dir <- file.path(repo_root, "paper_outputs_final_one_se")

needed_web_vars <- c(
  "enterid",
  paste0("pq16_", 1:16),
  paste0("pq16_", 1:16, "_distress"),
  "pq16_score",
  "dsst",
  paste0("pps_genetic_risk___", 1:6)
)

needed_pps_vars <- c(
  "enterid", "age", "gender", "ethnicity", "ethnic_density", "pollution",
  "high_urbanicity", "first_gen_migrant", "second_gen_migrant",
  "north_african_first_gen_migrant", "north_african_second_gen_migrant",
  "paternal_age", "paternal_ses", "parent_smi", "adult_life_events",
  "childhood_trauma", "daily_tobacco", "heavy_cannabis_use", "anhedonia", "PPS"
)

select_available <- function(data, vars) {
  data %>% dplyr::select(any_of(vars))
}

deduplicate_most_complete <- function(data, id = "enterid", completeness_vars = names(data)) {
  completeness_vars <- intersect(completeness_vars, names(data))
  data %>%
    mutate(.complete_n = rowSums(!is.na(across(all_of(completeness_vars))))) %>%
    arrange(.data[[id]], desc(.complete_n)) %>%
    distinct(.data[[id]], .keep_all = TRUE) %>%
    select(-.complete_n)
}

load_enter_data <- function() {
  finalised_uk <- read_excel(
    file.path(work_root, "Papers/Published/CBD Acceptability CHR-P/ENTER Finalised CAARMS Categories.xlsx")
  ) %>%
    mutate(
      caarms_outcome_arms = case_when(
        `Baseline CAARMS` != "No Risk" &
          `Baseline CAARMS` != "No risk" &
          `Baseline CAARMS` != "Psychosis Threshold Met" &
          `Baseline CAARMS` != "GRD" ~ 1,
        TRUE ~ 0
      ),
      caarms_outcome_psy = case_when(
        `Baseline CAARMS` == "Psychosis Threshold Met" ~ 1,
        TRUE ~ 0
      ),
      enterid = as.character(`ENTER ID`),
      finalised_chr = if_else(caarms_outcome_arms == 1 | caarms_outcome_psy == 1, 1, 0)
    ) %>%
    dplyr::select(enterid, finalised_chr)

  raw_uk <- read_csv(file.path(project_root, "ENTERAssessment_DATA_2025-02-13_1235.csv"), show_col_types = FALSE) %>%
    mutate(enterid = as.character(enterid)) %>%
    filter(!is.na(caarms_outcome_arms)) %>%
    transmute(enterid, raw_chr = if_else(caarms_outcome_arms == 1 | caarms_outcome_psy == 1, 1, 0)) %>%
    distinct(enterid, .keep_all = TRUE)

  enter_uk_web <- read_csv(file.path(project_root, "ENTEROnline_DATA_2025-05-30_1704.csv"), show_col_types = FALSE) %>%
    select_available(needed_web_vars)
  valid_uk <- read_csv(file.path(project_root, "Validations_04.02.25.csv"), show_col_types = FALSE) %>%
    filter(manual_validation == "Valid")
  enter_uk_web <- enter_uk_web %>% filter(enterid %in% valid_uk$enter_id)
  enter_uk_web <- deduplicate_most_complete(enter_uk_web, completeness_vars = needed_web_vars)

  enter_pps_uk <- read_csv(
    file.path(work_root, "PPS/ENTER/ENTER_PPS_Processing_UK/PPS_ENTER_clean_190626.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(enterid = as.character(enterid)) %>%
    dplyr::select(-PPS) %>%
    dplyr::rename(PPS = PPS_new) %>%
    select_available(needed_pps_vars)

  enter_uk_outcome <- full_join(finalised_uk, raw_uk, by = "enterid") %>%
    mutate(
      caarms_outcome_arms = coalesce(finalised_chr, raw_chr),
      # The finalised category is already collapsed to CHR-P status; retain
      # a zero psychosis flag so CHR-P-negative participants are classified.
      caarms_outcome_psy = if_else(!is.na(caarms_outcome_arms), 0, NA_real_)
    ) %>%
    select(enterid, caarms_outcome_arms, caarms_outcome_psy)
  enter_uk <- merge(enter_uk_outcome, enter_uk_web, by = "enterid", all.y = TRUE)
  enter_uk <- merge(enter_uk, enter_pps_uk, by = "enterid", all.x = TRUE)

  enter_it_outcome <- read_excel(
    file.path(work_root, "ENTER/Bipolar Prediction/ENTER_UK_Pavia_20.02.25_BPSS_SIBARS.xlsx"),
    sheet = "ENTER_Pavia"
  ) %>%
    mutate(enterid = as.character(enterid)) %>%
    dplyr::select(enterid, caarms_outcome_arms, caarms_outcome_psy)

  enter_it_web <- read_csv(file.path(project_root, "ENTERScreenerIT_DATA_2025-06-02_1322.csv"), show_col_types = FALSE) %>%
    select_available(needed_web_vars)
  valid_it <- read_csv(file.path(project_root, "Italy validations 22.03.25.csv"), show_col_types = FALSE) %>%
    filter(manual_validation == "Valid")
  enter_it_web <- enter_it_web %>% filter(enterid %in% valid_it$enter_id)
  enter_it_web <- deduplicate_most_complete(enter_it_web, completeness_vars = needed_web_vars)

  enter_pps_it <- read_csv(
    file.path(work_root, "PPS/ENTER/ENTER_PPS_Processing_IT/PPS_ENTER_IT_clean_190626.csv"),
    show_col_types = FALSE
  ) %>%
    mutate(enterid = as.character(enterid)) %>%
    select_available(needed_pps_vars)

  enter_it <- merge(enter_it_outcome, enter_it_web, by = "enterid", all.y = TRUE)
  enter_it <- merge(enter_it, enter_pps_it, by = "enterid", all.x = TRUE)

  combined <- bind_rows(
    enter_uk %>% mutate(country = "UK", across(everything(), as.character)),
    enter_it %>% mutate(country = "Italy (PV)", across(everything(), as.character))
  ) %>%
    mutate(
      across(
        -any_of(c("enterid", "gender", "ethnicity", "ethnic_density", "country")),
        ~ suppressWarnings(as.numeric(.x))
      ),
      chr = case_when(
        caarms_outcome_arms == 1 | caarms_outcome_psy == 1 ~ 1,
        caarms_outcome_arms == 0 & caarms_outcome_psy == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      group = case_when(
        chr == 1 ~ "CHR-P+",
        chr == 0 ~ "CHR-P-",
        TRUE ~ "Unlabelled"
      ),
      family_history = case_when(
        pps_genetic_risk___1 == 1 | pps_genetic_risk___2 == 1 |
          pps_genetic_risk___3 == 1 | pps_genetic_risk___4 == 1 ~ 1,
        pps_genetic_risk___5 == 1 ~ 0,
        pps_genetic_risk___6 == 1 ~ NA_real_
      ),
      above_pq16_cutoff = ifelse(!is.na(pq16_score), pq16_score >= 6, NA),
      paternal_age_35 = ifelse(!is.na(paternal_age), paternal_age >= 35, NA),
      paternal_age_45 = ifelse(!is.na(paternal_age), paternal_age >= 45, NA)
    )

  distress_cols <- intersect(paste0("pq16_", 1:16, "_distress"), names(combined))
  distress_matrix <- combined[, distress_cols, drop = FALSE]
  combined$pq_distress <- rowSums(distress_matrix, na.rm = TRUE)
  combined$pq_distress[rowSums(!is.na(distress_matrix)) == 0] <- NA_real_
  combined
}

fmt_n_pct <- function(n, denom) {
  paste0(n, " (", formatC(100 * n / denom, format = "f", digits = 1), "%)")
}

fmt_mean_sd <- function(x) {
  valid <- x[!is.na(x)]
  if (length(valid) == 0) return(NA_character_)
  paste0(
    formatC(mean(valid), format = "f", digits = 1),
    " (",
    formatC(stats::sd(valid), format = "f", digits = 1),
    ")"
  )
}

group_levels <- c("CHR-P+", "CHR-P-", "Unlabelled")

make_row <- function(characteristic, values) {
  tibble(
    Characteristic = characteristic,
    `CHR-P+` = values[["CHR-P+"]],
    `CHR-P-` = values[["CHR-P-"]],
    Unlabelled = values[["Unlabelled"]]
  )
}

continuous_row <- function(data, label, variable) {
  values <- setNames(lapply(group_levels, function(g) {
    fmt_mean_sd(data[[variable]][data$group == g])
  }), group_levels)
  make_row(label, values)
}

unknown_row <- function(data, variable) {
  values <- setNames(lapply(group_levels, function(g) {
    as.character(sum(is.na(data[[variable]][data$group == g])))
  }), group_levels)
  make_row("Unknown", values)
}

categorical_rows <- function(data, label, variable, levels) {
  denominator <- setNames(lapply(group_levels, function(g) {
    sum(!is.na(data[[variable]][data$group == g]))
  }), group_levels)

  rows <- lapply(names(levels), function(level_name) {
    level_value <- levels[[level_name]]
    values <- setNames(lapply(group_levels, function(g) {
      n <- sum(data[[variable]][data$group == g] == level_value, na.rm = TRUE)
      denom <- denominator[[g]]
      if (denom == 0) return("0 (NA)")
      fmt_n_pct(n, denom)
    }), group_levels)
    make_row(level_name, values)
  })

  bind_rows(make_row(label, setNames(as.list(rep("", 3)), group_levels)), bind_rows(rows), unknown_row(data, variable))
}

binary_positive_row <- function(data, label, variable, positive_value = 1, include_unknown = TRUE) {
  denominator <- setNames(lapply(group_levels, function(g) {
    sum(!is.na(data[[variable]][data$group == g]))
  }), group_levels)
  values <- setNames(lapply(group_levels, function(g) {
    n <- sum(data[[variable]][data$group == g] == positive_value, na.rm = TRUE)
    denom <- denominator[[g]]
    if (denom == 0) return("0 (NA)")
    fmt_n_pct(n, denom)
  }), group_levels)
  out <- make_row(label, values)
  if (include_unknown) out <- bind_rows(out, unknown_row(data, variable))
  out
}

data <- load_enter_data()

group_counts <- table(factor(data$group, levels = group_levels))
names_with_n <- c(
  `CHR-P+` = paste0("CHR-P+ N = ", group_counts[["CHR-P+"]]),
  `CHR-P-` = paste0("CHR-P- N = ", group_counts[["CHR-P-"]]),
  Unlabelled = paste0("Unlabelled N = ", group_counts[["Unlabelled"]])
)

table_1 <- bind_rows(
  continuous_row(data, "Age", "age"),
  unknown_row(data, "age"),
  categorical_rows(data, "Gender", "gender", c(Female = "Female", Male = "Male", `Non-binary` = "Non-binary", Other = "Other")),
  categorical_rows(data, "Ethnicity", "ethnicity", c(Asian = "Asian", Black = "Black", Mixed = "Mixed", Other = "Other", White = "White")),
  binary_positive_row(data, "Above PQ-16 cut-off", "above_pq16_cutoff", TRUE),
  continuous_row(data, "PQ-16 Distress", "pq_distress"),
  unknown_row(data, "pq_distress"),
  continuous_row(data, "DSST", "dsst"),
  unknown_row(data, "dsst"),
  binary_positive_row(data, "Family history of psychosis", "family_history"),
  categorical_rows(data, "Ethnic density", "ethnic_density", c(High = "High", Low = "Low", Medium = "Medium")),
  continuous_row(data, "Pollution (PM2.5)", "pollution"),
  unknown_row(data, "pollution"),
  binary_positive_row(data, "High urbanicity", "high_urbanicity"),
  binary_positive_row(data, "First-generation migrant", "first_gen_migrant", include_unknown = FALSE),
  binary_positive_row(data, "Second-generation migrant", "second_gen_migrant", include_unknown = FALSE),
  binary_positive_row(data, "First-generation migrant from North Africa", "north_african_first_gen_migrant", include_unknown = FALSE),
  binary_positive_row(data, "Second-generation migrant from North Africa", "north_african_second_gen_migrant", include_unknown = FALSE),
  unknown_row(data, "first_gen_migrant"),
  binary_positive_row(data, "Paternal age 35", "paternal_age_35", TRUE, include_unknown = FALSE),
  binary_positive_row(data, "Paternal age 45", "paternal_age_45", TRUE, include_unknown = FALSE),
  binary_positive_row(data, "Paternal socioeconomic status", "paternal_ses"),
  binary_positive_row(data, "Parental severe mental illness", "parent_smi", include_unknown = FALSE),
  continuous_row(data, "Stressful life events (number in past six months)", "adult_life_events"),
  unknown_row(data, "adult_life_events"),
  continuous_row(data, "Childhood trauma", "childhood_trauma"),
  unknown_row(data, "childhood_trauma"),
  binary_positive_row(data, "Daily tobacco use", "daily_tobacco", include_unknown = FALSE),
  binary_positive_row(data, "Heavy cannabis use", "heavy_cannabis_use", include_unknown = FALSE),
  continuous_row(data, "Trait anhedonia", "anhedonia"),
  unknown_row(data, "anhedonia")
) %>%
  rename(
    !!names_with_n[["CHR-P+"]] := `CHR-P+`,
    !!names_with_n[["CHR-P-"]] := `CHR-P-`,
    !!names_with_n[["Unlabelled"]] := Unlabelled
  )

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(table_1, file.path(output_dir, "table1_sociodemographics.csv"))

message("Saved Table 1 to ", file.path(output_dir, "table1_sociodemographics.csv"))
