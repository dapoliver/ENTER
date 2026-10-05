# Generate supplementary tables, subgroup analyses and paired comparisons.
rm(list = ls())
repo_root <- normalizePath(".", mustWork = FALSE)
setwd(repo_root)
source("config/project_config.R")

source("R/regenerate_sociodemographics_table.R")
source("R/subgroup_performance_from_predictions.R")
source("R/paired_model_performance_comparison.R")
source("R/paired_within_model_subgroup_comparisons.R")
source("R/regenerate_subgroup_performance_table.R")
source("R/regenerate_coefficient_importance_table.R")
source("R/pmsampsize_main_models.R")

message("Supplementary analyses completed. Results are in: ", SUPPLEMENT_OUTPUT)
