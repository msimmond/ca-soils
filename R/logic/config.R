# =============================================================================
# config.R — Config load + path normalization helpers
#
# Purpose:
#   - Read YAML config (config/config.yml by default, or APP_CONFIG env override).
#   - Validate the schema (required keys, allowed values).
#   - Normalize all file paths so they work no matter where the app is launched
#     (RStudio button, repo root, app subfolder, CI).
#
# Public API:
#   load_config(path)        -> list
#   resolve_paths(cfg, app_root = NULL) -> cfg with absolute, validated paths
#
# Typical usage (in global.R):
#   source("R/logic/config.R")
#   CFG_PATH <- Sys.getenv("APP_CONFIG", unset = file.path(APP_ROOT, "config", "config.yml"))
#   CFG <- resolve_paths(load_config(CFG_PATH), app_root = APP_ROOT)
#   options(app.config = CFG)
#
# Notes:
#   - `app_root` should be the detected repo/app root (e.g., via rprojroot).
#   - This module does NOT read/validate the data dictionary schema; that’s done
#     later in your data/validate logic. It only checks that files exist.
# =============================================================================

#' Load configuration from YAML file
#' @param path character; path to YAML (e.g., "config/config.yml")
#' @return list config
load_config <- function(path) {
  if (!file.exists(path)) {
    stop("Config file not found: ", path, call. = FALSE)
  }
  cfg <- yaml::read_yaml(path)

  if (!is.list(cfg)) {
    stop("Config file did not parse to a list. Check YAML syntax: ", path, call. = FALSE)
  }
  cfg
}

#' Normalize and validate paths + top-level config
#'
#' @param cfg list; returned from `load_config()`
#' @param app_root character; repo/app root. If given, relative paths are resolved from here.
#' @return list; config with normalized paths and validated core fields
resolve_paths <- function(cfg, app_root = NULL) {
  stopifnot(is.list(cfg))

  # --- Validate top-level keys (minimal config) --------------------------
  # No additional validation needed for minimal config

  # --- Validate .paths section -------------------------------------------------
  if (!"paths" %in% names(cfg) || !is.list(cfg$paths)) {
    stop("`paths` list missing from config.yml.", call. = FALSE)
  }
  p <- cfg$paths

  must <- c("template", "output_dir")
  miss <- setdiff(must, names(p))
  if (length(miss)) {
    stop("Missing required path(s) in config.yml -> paths: ",
         paste(miss, collapse = ", "), call. = FALSE)
  }

  # Helper: make a path absolute (relative to app_root if provided)
  make_abs <- function(x) {
    if (is.null(x) || !nzchar(x)) return(x)
    if (!is.null(app_root) && !fs::is_absolute_path(x)) x <- fs::path(app_root, x)
    normalizePath(x, winslash = "/", mustWork = FALSE)
  }

  # Normalize all relevant paths
  p$template    <- make_abs(p$template)
  p$output_dir  <- make_abs(p$output_dir)



  # --- Existence checks (fail early with helpful messages) --------------------
  if (!file.exists(p$template)) {
    stop("Quarto template not found at paths$template: ", p$template, call. = FALSE)
  }
  # A small sanity check: template should be a .qmd
  if (!grepl("\\.qmd$", tolower(p$template))) {
    warning("The template '", p$template, "' does not end with .qmd; is this intended?")
  }


  if (!dir.exists(p$output_dir)) {
    fs::dir_create(p$output_dir)
  }



  # --- Optional: light validation of visual config ----------------------------
  # (Does not fail build; only warns to help catch typos.)
  if (!is.null(cfg$visual) && is.list(cfg$visual) && !is.null(cfg$visual$colors)) {
    hexlike <- function(x) is.character(x) && all(grepl("^#?[0-9A-Fa-f]{6}$", x))
    cols <- cfg$visual$colors
    if (!hexlike(unlist(cols, use.names = FALSE))) {
      warning("Some colors in config$visual$colors are not 6-digit hex codes.")
    }
  }

  # Finalize
  cfg$paths <- p
  cfg
}

# Null-coalescing helper (keeps this file self-contained)
`%||%` <- function(x, y) if (is.null(x)) y else x