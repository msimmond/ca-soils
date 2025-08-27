# =============================================================================
# wrapper.R — Report Generation Orchestration
# =============================================================================
#
# OVERVIEW
# -----------------------------------------------------------------------------
# This file defines the central function `generate_soil_health_report()`,
# which is the *single source of truth* for producing soil health reports.
#
# - It takes in a dataset (CSV file path) plus user selections
#   (producer, year, grouping variable).
# - It performs light validation (producer/year, coordinates).
# - It writes a cleaned temporary CSV for Quarto to consume.
# - It calls Quarto (`quarto::quarto_render()`) to render the
#   specified template (config$paths$template) into an HTML report.
#
# Because both the Shiny app AND any command-line workflows call this function,
# it ensures consistent behavior everywhere.
#
# USER FLOW (in app context)
# -----------------------------------------------------------------------------
# 1. The Shiny module mod_report_server() prepares a temp CSV of the selected
#    dataset and calls this function.
# 2. This function validates and prepares the data, then runs Quarto.
  # 3. Quarto renders the report_template.qmd into an HTML report in the output directory.
# 4. The absolute path to the generated HTML is returned.
#
# PARAMETERS
# -----------------------------------------------------------------------------
# generate_soil_health_report(
#   data_path,     # string, path to the CSV file (already standardized by upload)
#   producer_id,   # string, producer/farm name
#   year,          # numeric or string, reporting year
#   grouping_var,  # string or NULL, optional grouping variable (e.g. "field_id")
#   config,        # list or NULL, app config (from global.R / config.yml).
#                  #   If NULL, defaults to get_cfg()
#   output_dir     # string or NULL, directory for output reports.
#                  #   Defaults to "outputs/" if not provided
# )
#
# RETURNS
# -----------------------------------------------------------------------------
# - A single string: the absolute path to the generated HTML report.
# - Errors if validation fails or Quarto cannot render.
#
# PIPELINE
# -----------------------------------------------------------------------------
# 1. Validate inputs and ensure Quarto is installed.
# 2. Load the dataset (light validation: producer/year, coordinates).
# 3. Write a temporary "cleaned" CSV for Quarto.
# 4. Call quarto_render() with execute_params to pass arguments to template.
# 5. Return the full path to the generated HTML file.
#
# COMMON FAILURE POINTS
# -----------------------------------------------------------------------------
# - Config missing or doesn’t have paths$template.
# - Quarto CLI not on PATH.
# - Producer/year not present in the dataset.
# - Template file not found.
#
# =============================================================================

# Utility: null-or helper (like Python’s `or`)
`%||%` <- function(x, y) if (is.null(x)) y else x

# -----------------------------------------------------------------------------
#' Generate a soil health report via Quarto
#'
#' This function orchestrates the entire workflow of producing an HTML soil
#' health report from a dataset. It validates inputs, writes a temporary
#' cleaned CSV, and calls Quarto to render the template defined in config.
#'
#' @param data_path character, path to the CSV dataset (already standardized).
#' @param producer_id character, the producer/farm name to report on.
#' @param year numeric/string, the reporting year.
#' @param grouping_var character or NULL, optional grouping variable
#'   (e.g. "field_id" or "treatment_id").
#' @param config list or NULL, the application config as returned by load_config().
#'   Must contain `paths$template`. If NULL, falls back to get_cfg().
#' @param output_dir character or NULL, output directory for reports.
#'   Defaults to "outputs".
#'
#' @return Absolute path to the rendered HTML file (string).
#' @examples
#' \dontrun{
#'   cfg <- get_cfg()
#'   generate_soil_health_report("data/washi-data.csv",
#'                               producer_id = "Full Belly Farm",
#'                               year = 2025,
#'                               grouping_var = "field_id",
#'                               config = cfg)
#' }
#' @export
# -----------------------------------------------------------------------------
generate_soil_health_report <- function(
  data_path,
  producer_id,
  year,
  grouping_var = NULL,
  config = NULL,
  output_dir = NULL,
  dict_path = NULL
) {
  # ---- Resolve config --------------------------------------------------------
  if (is.null(config)) config <- get_cfg()
  if (is.null(config) || !is.list(config) || is.null(config$paths$template)) {
    stop("App configuration is missing or invalid: no 'paths$template' found.")
  }

  # ---- Basic argument checks -------------------------------------------------
  if (!is.character(data_path) || length(data_path) != 1L || !file.exists(data_path)) {
    stop("Data file not found at: ", data_path)
  }

  # Resolve template + output directory
  template_abs <- fs::path_abs(config$paths$template)
  if (!fs::file_exists(template_abs)) {
    stop("Quarto template not found: ", template_abs)
  }

  output_dir_abs <- fs::path_abs(output_dir %||% "outputs")
  fs::dir_create(output_dir_abs)

  # ---- Ensure Quarto CLI is available ---------------------------------------
  if (!nzchar(Sys.which("quarto"))) {
    stop("Quarto CLI is required but not found on PATH. Install from https://quarto.org/")
  }

  # ---- Load + validate dataset ----------------------------------------------
  source("R/logic/data.R")
  source("R/logic/validate.R")

  df <- load_lab_data(data_path)

  # Warn if lat/long invalid (but don’t stop app)
  if (all(c("latitude", "longitude") %in% names(df))) {
    coords_ok <- validate_coordinates(df)
    if (!isTRUE(coords_ok)) {
      warning("Coordinate validation failed—maps may not render correctly.")
    }
  }

  # Validate that producer/year exist in the dataset
  validate_producer_year(df, producer_id, year)

  # ---- Write cleaned CSV for Quarto -----------------------------------------
  cleaned_tmp_csv <- fs::file_temp(pattern = "cleaned_", tmp_dir = output_dir_abs, ext = ".csv")
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::write_csv(df, cleaned_tmp_csv)
  } else {
    utils::write.csv(df, cleaned_tmp_csv, row.names = FALSE, na = "")
  }

  # ---- Build params for Quarto ----------------------------------------------
  execute_params <- list(
    data_path    = cleaned_tmp_csv,
    producer_id  = producer_id,
    year         = year,
    grouping_var = grouping_var,
    config       = config,
    dict_path    = dict_path
  )

  timestamp   <- format(Sys.time(), "%Y%m%d_%H%M%S")
  safe_prod   <- gsub("[^A-Za-z0-9_-]", "_", producer_id)
  output_file <- paste0("soil_health_report_", safe_prod, "_", year, "_", timestamp, ".html")

  message("Generating report for ", producer_id, " (", year, ")")
  message("Template path: ", template_abs)
  message("Output file: ", output_file)

  # ---- Call Quarto -----------------------------------------------------------
  tryCatch(
    {
      quarto::quarto_render(
        input          = template_abs,
        execute_params = execute_params,
        output_file    = output_file,
        quiet          = FALSE
      )
    },
    error = function(e) {
      stop("Failed to generate report: ", conditionMessage(e))
    }
  )

  # ---- Verify output + return ------------------------------------------------
  output_path <- fs::path(output_dir_abs, output_file)
  if (!fs::file_exists(output_path)) {
    stop("Report file not found after rendering: ", output_path)
  }

  message("✅ Report generated: ", output_path)
  fs::path_abs(output_path)
}


# -----------------------------------------------------------------------------
# Memoized Variant
# -----------------------------------------------------------------------------
# This wrapper caches results for identical inputs (producer/year/grouping/data).
# If the same report is requested again, the cached HTML is reused without
# re-running Quarto, which can save significant time.
# -----------------------------------------------------------------------------
if (requireNamespace("memoise", quietly = TRUE)) {
  generate_report_memoized <- memoise::memoise(function(
    data_path,
    producer_id,
    year,
    grouping_var = NULL,
    config = NULL,
    output_dir = NULL,
    dict_path = NULL
  ) {
    if (is.null(config)) config <- get_cfg()
    generate_soil_health_report(
      data_path    = data_path,
      producer_id  = producer_id,
      year         = year,
      grouping_var = grouping_var,
      config       = config,
      output_dir   = output_dir,
      dict_path    = dict_path
    )
  })
}