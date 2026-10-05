# Reproduce the main-text model analyses from start to finish.
rm(list = ls())
repo_root <- normalizePath(".", mustWork = FALSE)
setwd(repo_root)
source("config/project_config.R")

run_model <- function(target_models, output_name, use_previous = FALSE) {
  Sys.setenv(
    OUTPUT_DIR = file.path(MAIN_OUTPUT, output_name),
    TARGET_MODELS = target_models,
    RUN_SSL_TUNING = "false", RUN_MAIN_STEPS = "true", RUN_SENSITIVITY_STEPS = "false",
    RUN_BOOTSTRAP = "true", FINAL_MODEL_FAMILIES = "glmnet,rf,xgboost",
    MAX_SSL_ITERATIONS = "20", SSL_MAX_NEGATIVE_PROB = "0.30",
    SSL_PSEUDOLABEL_WEIGHT_CAP_RATIO = "3.0", SSL_CLASS_BALANCED_TAILS = "false",
    USE_ONE_SE_SELECTION = "true"
  )
  if (use_previous) Sys.setenv(USE_PREVIOUS_PREDICTOR_SET = "true") else Sys.unsetenv("USE_PREVIOUS_PREDICTOR_SET")
  old <- getwd(); setwd(PROJECT_ROOT); on.exit(setwd(old), add = TRUE)
  source(file.path(repo_root, "R/ENTER_main.R"), local = .GlobalEnv)
  setwd(old)
}

run_model("Step_1b_pq16_cutoff", "pq16_cutoff", FALSE)
run_model(paste(c("Step_1_pq16_score", "Step_2_pq_distress", "Step_3_dsst", "Step_4_demographic_clinical"), collapse = ","), "unimodal", FALSE)
run_model("Step_5_full_model", "enter", TRUE)

message("Main analyses completed. Results are in: ", MAIN_OUTPUT)
