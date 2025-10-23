# =============================================================================
# global.R — Application bootstrap
# =============================================================================
#
# OVERVIEW
# -----------------------------------------------------------------------------
# This file is the **first code that runs** when the Shiny app starts.
# It sets up the runtime environment so that all modules and logic files
# have consistent access to:
#   • the application root directory (APP_ROOT)
#   • reproducible dependencies via renv
#   • the Quarto CLI (for report rendering)
#   • the {soils} package (domain-specific calculations)
#   • the app configuration (from config/config.yml)
#
# DESIGN GOALS
# -----------------------------------------------------------------------------
# - Reproducibility: renv ensures all dependencies are pinned.
# - Portability: paths are resolved relative to APP_ROOT (repo root).
# - Safety: fail fast if Quarto is missing (required for reports).
# - Flexibility: allows devs to use a local checkout of {soils} when iterating.
#
# FLOW
# -----------------------------------------------------------------------------
# 1. Identify APP_ROOT (based on presence of renv.lock).
# 2. Activate renv if present (ensures correct R library paths).
# 3. Load app dependencies (shiny, dplyr, yaml, etc.).
# 4. Verify that Quarto CLI is installed and log its version.
# 5. Load the {soils} package:
#      - Dev mode: USE_LOCAL_SOILS=1 + ~/projects/soils → devtools::load_all()
#      - Prod mode: library(soils) from renv.lock
# 6. Load and normalize YAML configuration via config.R helpers.
# 7. Store configuration in global options(app.config), with helper get_cfg().
#
# =============================================================================


## --- 1) Determine application root ------------------------------------------
suppressPackageStartupMessages({
  library(rprojroot)  # helps find the repo root based on marker files
  library(fs)         # robust filesystem utilities
})

# APP_ROOT is the repo root (identified by presence of renv.lock)
APP_ROOT <- rprojroot::find_root(rprojroot::has_file("renv.lock"))

# Make APP_ROOT the working directory.
# This ensures all relative paths (e.g., "config/config.yml") resolve consistently.
setwd(APP_ROOT)


## --- 2) Activate renv if present ---------------------------------------------
# renv provides reproducible package libraries. Activating it ensures that
# the app uses the pinned versions defined in renv.lock.
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}


## --- 3) Load core app dependencies -------------------------------------------
# These packages are needed everywhere in the app.
suppressPackageStartupMessages({
  library(shiny)    # web application framework
  library(dplyr)    # data manipulation
  library(tidyr)    # tidy data reshaping
  library(yaml)     # config parsing
  library(fs)       # filesystem paths
  library(readxl)   # reading Excel templates
  library(writexl)  # writing Excel outputs
})


## --- 4) Verify Quarto CLI availability ---------------------------------------
# The app cannot generate reports without Quarto.
quarto_bin <- Sys.which("quarto")
if (!nzchar(quarto_bin)) {
  stop(
    "Quarto CLI is required but was not found.\n",
    "Install from https://quarto.org/ and ensure 'quarto' is on the system PATH."
  )
}

# Log version (helpful in debug logs)
invisible(system2(quarto_bin, args = "--version", stdout = TRUE, stderr = TRUE))


## --- 5) Load casoils package ---------------------------------------------------
# Two possible modes:
#   • Development: USE_LOCAL_CASOILS=1 + ~/projects/ca-soils exists
#       - loads local package source with devtools::load_all()
#   • Production (default): library(casoils) loads the version pinned by renv

use_local_casoils <- identical(Sys.getenv("USE_LOCAL_CASOILS", "0"), "1")
local_casoils_dir <- path_expand("~/projects/UCANR/ca-soil-health-reports-clean/casoils_minimal")

if (use_local_casoils && dir_exists(local_casoils_dir)) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("devtools is required to load local 'casoils' in development mode. Please install it in this renv.")
  }
  message("🔧 Using local 'casoils' from: ", local_casoils_dir)
  devtools::load_all(local_casoils_dir, quiet = TRUE)
  library(casoils)  # make it available like a normal package
} else {
  library(casoils)  # standard load (uses version from renv.lock)
}


## --- 6) Load minimal configuration ------------------------------------------
# config.R defines load_config() and resolve_paths(), which:
#   - read YAML (config.yml → list)
#   - expand relative paths into absolute paths (based on APP_ROOT)
#   - validate required keys exist
source(file.path(APP_ROOT, "R/logic/config.R"))

CFG_PATH <- Sys.getenv("APP_CONFIG", unset = file.path(APP_ROOT, "config", "config.yml"))
CFG <- load_config(CFG_PATH)                   # read config.yml into list
CFG <- resolve_paths(CFG, app_root = APP_ROOT) # normalize all paths


## --- 7) Expose configuration globally ----------------------------------------
# Store config in R’s global options so it can be accessed anywhere.
options(app.config = CFG)

# Convenience accessor used throughout app modules.
get_cfg <- function() getOption("app.config")