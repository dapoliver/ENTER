setwd("/Users/domoliver/Library/CloudStorage/Dropbox/Work/ENTER/CAARMS Prediction/Analyses")

Sys.setenv(
  OUTPUT_DIR = file.path(getwd(), "revised_enter_step5_unbalanced_one_se_regularised_trees"),
  TARGET_MODELS = "Step_5_full_model",
  RUN_SSL_TUNING = "false",
  RUN_MAIN_STEPS = "true",
  RUN_SENSITIVITY_STEPS = "false",
  RUN_BOOTSTRAP = "true",
  FINAL_MODEL_FAMILIES = "glmnet,rf,xgboost",
  GLMNET_ALPHA_VALUES = "0.25,0.5,0.75",
  MAX_SSL_ITERATIONS = "20",
  SSL_MAX_NEGATIVE_PROB = "0.30",
  SSL_PSEUDOLABEL_WEIGHT_CAP_RATIO = "3.0",
  SSL_CLASS_BALANCED_TAILS = "false",
  USE_PREVIOUS_PREDICTOR_SET = "true",
  RF_MTRY_VALUES = "3,6,9,12",
  RF_MIN_NODE_SIZE_VALUES = "20,50,100,150",
  RF_MAX_DEPTH_VALUES = "4,6,8",
  XGB_NROUNDS_VALUES = "200,400,800",
  XGB_MAX_DEPTH_VALUES = "2,3,4",
  XGB_ETA_VALUES = "0.01,0.03,0.05",
  XGB_GAMMA_VALUES = "0,1,5",
  XGB_COLSAMPLE_BYTREE_VALUES = "0.4,0.6,0.8",
  XGB_MIN_CHILD_WEIGHT_VALUES = "1,2,5",
  XGB_SUBSAMPLE_VALUES = "0.6,0.8,0.9",
  USE_ONE_SE_SELECTION = "true"
)

source(file.path("ENTER", "R", "ENTER_main.R"))
