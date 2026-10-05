# Generate core main-text and supplementary figures from saved results.
rm(list = ls())
repo_root <- normalizePath(".", mustWork = FALSE)
setwd(repo_root)
source("config/project_config.R")

source("R/regenerate_final_figures.R")
source("R/regenerate_steps1to4_summary_plots.R")
source("R/regenerate_classification_plot.R")
source("R/regenerate_roc_plot.R")
source("R/regenerate_dca_enter_step5.R")
source("R/regenerate_dca_pq16_cutoff.R")

message("Figure generation completed. Figures are in: ", FIGURE_OUTPUT)
