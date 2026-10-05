# Bootstrap comparisons of subgroup performance within each learner.
# For each subgroup pair, the difference is subgroup A minus subgroup B.
# Subgroups are independent, so each bootstrap sample is drawn separately.

suppressPackageStartupMessages(library(tidyverse))

input_dir <- Sys.getenv(
  "WITHIN_MODEL_SUBGROUP_INPUT_DIR",
  unset = "/Users/domoliver/Library/CloudStorage/Dropbox/Work/ENTER/CAARMS Prediction/Analyses/revised_enter_step5_unbalanced_one_se_regularised_trees"
)
paired_output_dir <- Sys.getenv(
  "WITHIN_MODEL_SUBGROUP_OUTPUT_DIR",
  unset = file.path(input_dir, "within_model_subgroup_comparisons")
)
B <- suppressWarnings(as.integer(Sys.getenv("WITHIN_MODEL_SUBGROUP_B", "2000")))
if (is.na(B) || B < 100) B <- 2000
seed <- suppressWarnings(as.integer(Sys.getenv("WITHIN_MODEL_SUBGROUP_SEED", "123")))
if (is.na(seed)) seed <- 123

source(file.path("R", "regenerate_sociodemographics_table.R"))
metadata <- load_enter_data() %>%
  filter(!is.na(chr)) %>%
  mutate(
    Subject_ID = as.character(row_number()),
    age = suppressWarnings(as.numeric(age)),
    age_group = case_when(age < 18 ~ "Under 18", age >= 18 ~ "18 and over", TRUE ~ NA_character_),
    gender = as.character(gender), ethnicity = as.character(ethnicity), country = as.character(country),
    obs_meta = as.numeric(chr)
  ) %>% select(Subject_ID, age_group, gender, ethnicity, country, obs_meta)

predictions <- read_csv(file.path(input_dir, "test_set_predictions.csv"), show_col_types = FALSE) %>%
  mutate(Subject_ID = as.character(Subject_ID), obs = as.numeric(obs), pred = as.numeric(pred), chr_pred = as.numeric(chr_pred)) %>%
  group_by(model, Subject_ID) %>%
  summarise(obs = first(obs), pred = mean(pred, na.rm = TRUE), chr_pred = mean(chr_pred, na.rm = TRUE), .groups = "drop") %>%
  filter(grepl("^Step_5_full_model", model))

check <- predictions %>% distinct(Subject_ID, obs) %>% left_join(metadata, by = "Subject_ID")
if (any(is.na(check$obs_meta)) || any(check$obs != check$obs_meta, na.rm = TRUE)) stop("Prediction IDs/outcomes could not be reconciled to subgroup metadata.")

auc <- function(obs, pred) {
  keep <- !is.na(obs) & !is.na(pred); obs <- obs[keep]; pred <- pred[keep]
  if (length(unique(obs)) < 2) return(NA_real_)
  n1 <- sum(obs == 1); n0 <- sum(obs == 0)
  (sum(rank(pred)[obs == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
metric_values <- function(obs, pred, chr_pred) {
  keep <- !is.na(obs) & !is.na(pred) & !is.na(chr_pred); obs <- obs[keep]; pred <- pred[keep]; chr_pred <- chr_pred[keep]
  if (!length(obs)) return(c(C=NA, balanced_accuracy=NA, brier=NA, calibration_intercept=NA, calibration_slope=NA))
  pc <- as.integer(chr_pred >= 0.5); tp <- sum(pc==1 & obs==1); tn <- sum(pc==0 & obs==0); fp <- sum(pc==1 & obs==0); fn <- sum(pc==0 & obs==1)
  sens <- if (tp+fn > 0) tp/(tp+fn) else NA; spec <- if (tn+fp > 0) tn/(tn+fp) else NA
  cal <- tryCatch({ co <- coef(suppressWarnings(glm(obs ~ pmin(pmax(pred,0.001),0.999), family=binomial()))); c(intercept=co[1], slope=co[2]) }, error=function(e) c(intercept=NA, slope=NA))
  c(C=auc(obs,pred), balanced_accuracy=if(all(is.finite(c(sens,spec)))) mean(c(sens,spec)) else NA,
    brier=mean((obs-pred)^2), calibration_intercept=unname(cal[1]), calibration_slope=unname(cal[2]))
}

metric_names <- c("C", "balanced_accuracy", "brier", "calibration_intercept", "calibration_slope")
set.seed(seed)
results <- list()
for (subgroup_type in c("age_group", "gender", "ethnicity", "country")) {
  lev <- metadata %>% distinct(.data[[subgroup_type]]) %>% pull(.data[[subgroup_type]]) %>% na.omit() %>% as.character()
  if (length(lev) < 2) next
  for (pair in combn(lev, 2, simplify=FALSE)) {
    a_name <- pair[1]; b_name <- pair[2]
    ids_a <- metadata %>% filter(.data[[subgroup_type]] == a_name) %>% pull(Subject_ID)
    ids_b <- metadata %>% filter(.data[[subgroup_type]] == b_name) %>% pull(Subject_ID)
    for (model in sort(unique(predictions$model))) {
      pa <- predictions %>% filter(Subject_ID %in% ids_a, model == !!model) %>% select(Subject_ID, obs, pred, chr_pred) %>% distinct()
      pb <- predictions %>% filter(Subject_ID %in% ids_b, model == !!model) %>% select(Subject_ID, obs, pred, chr_pred) %>% distinct()
      if (nrow(pa) < 2 || nrow(pb) < 2 || length(unique(pa$obs)) < 2 || length(unique(pb$obs)) < 2) next
      boot <- matrix(NA_real_, nrow=B, ncol=length(metric_names), dimnames=list(NULL,metric_names))
      for (i in seq_len(B)) {
        sa <- pa[sample.int(nrow(pa), nrow(pa), replace=TRUE),]
        sb <- pb[sample.int(nrow(pb), nrow(pb), replace=TRUE),]
        boot[i,] <- metric_values(sa$obs,sa$pred,sa$chr_pred)[metric_names] - metric_values(sb$obs,sb$pred,sb$chr_pred)[metric_names]
      }
      results[[length(results)+1]] <- tibble(
        subgroup_type=subgroup_type, subgroup_a=a_name, subgroup_b=b_name,
        n_a=nrow(pa), events_a=sum(pa$obs==1), n_b=nrow(pb), events_b=sum(pb$obs==1), model=model,
        metric=metric_names, difference=apply(boot,2,mean,na.rm=TRUE), ci_low=apply(boot,2,quantile,probs=.025,na.rm=TRUE),
        ci_high=apply(boot,2,quantile,probs=.975,na.rm=TRUE), p_value=apply(boot,2,function(x){x<-x[is.finite(x)]; if(!length(x)) return(NA_real_); min(1,2*min(mean(x<=0),mean(x>=0)))})
      )
    }
  }
}
dir.create(paired_output_dir, recursive=TRUE, showWarnings=FALSE)
write_csv(bind_rows(results), file.path(paired_output_dir,"within_model_subgroup_performance_differences.csv"))
write_csv(tibble(bootstrap_B=B, seed=seed), file.path(paired_output_dir,"within_model_subgroup_comparison_metadata.csv"))
message("Saved within-model subgroup comparisons to ", paired_output_dir)
