# Edit these paths on a new machine before running the pipeline.
PROJECT_ROOT <- Sys.getenv("ENTER_PROJECT_ROOT", unset = normalizePath("..", mustWork = FALSE))
WORK_ROOT <- Sys.getenv("ENTER_WORK_ROOT", unset = normalizePath(file.path(PROJECT_ROOT, "../.."), mustWork = FALSE))
REPO_ROOT <- Sys.getenv("ENTER_REPO_ROOT", unset = normalizePath(".", mustWork = FALSE))
Sys.setenv(ENTER_PROJECT_ROOT = PROJECT_ROOT, ENTER_WORK_ROOT = WORK_ROOT, ENTER_REPO_ROOT = REPO_ROOT)

MAIN_OUTPUT <- file.path(REPO_ROOT, "results", "main")
SUPPLEMENT_OUTPUT <- file.path(REPO_ROOT, "results", "supplement")
FIGURE_OUTPUT <- file.path(REPO_ROOT, "figures")

dir.create(MAIN_OUTPUT, recursive = TRUE, showWarnings = FALSE)
dir.create(SUPPLEMENT_OUTPUT, recursive = TRUE, showWarnings = FALSE)
dir.create(FIGURE_OUTPUT, recursive = TRUE, showWarnings = FALSE)
