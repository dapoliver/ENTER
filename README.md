# ENTER prediction analysis

This repository contains the reproducible R analysis for the ENTER community CHR-P screening study. Each entry-point is a standalone script and can be run independently from the repository root after the data paths and R packages have been configured:

1. `scripts/03_run_main_analysis.R` runs the PQ-16 cut-off, unimodal and ENTER model analyses.
2. `scripts/04_run_supplementary_analysis.R` generates supplementary performance tables, subgroup analyses, paired comparisons, coefficients and sample-size assessments.
3. `scripts/05_generate_figures.R` generates the core main-text and supplementary figures from the saved results.

## Setup

The raw participant-level files are intentionally excluded from GitHub. Set the project location before running:

```r
Sys.setenv(ENTER_PROJECT_ROOT = "/path/to/ENTER/CAARMS Prediction")
Sys.setenv(ENTER_REPO_ROOT = "/path/to/ENTER_GitHub_Repository")
```

`ENTER_PROJECT_ROOT` must contain the source datasets and linked PPS/CAARMS files expected by `R/ENTER_main.R`. Install the R packages used by the analysis before running the scripts, including `tidyverse`, `readxl`, `caret`, `pROC`, `glmnet`, `predRupdate`, `missForest`, `mice`, `VIM`, `doParallel`, `writexl`, `survival`, and `metrica`.

## Running

From the repository root:

```bash
Rscript scripts/03_run_main_analysis.R
Rscript scripts/04_run_supplementary_analysis.R
Rscript scripts/05_generate_figures.R
```

The supplementary analysis and figure scripts require the corresponding saved model outputs and predictions; run the main analysis first when reproducing the project from raw data. Outputs are written to `results/` and `figures/`, both of which are excluded from version control.
