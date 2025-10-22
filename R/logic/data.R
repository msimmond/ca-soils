# data.R ----
# =============================================================================
# Data loading, normalization, and dictionary utilities for the Soil Health app
#
# Design goals
#   - Be tolerant of multiple incoming schemas (legacy Think Tank, Sac Valley,
#     or mapped uploads from the app).
#   - Create or normalize key identifiers (producer_id, field_id, year,
#     sample_id) in one place, based on project config.
#   - Avoid premature "duplicate sample_id" failures; aggregation happens later
#     (in the QMD or an explicit long/aggregate helper when needed).
#   - Do *not* mutate working directory; keep all I/O path-agnostic.
#
# Key concepts
#   - "Grain" (row identity) of the *raw/wide* upload is per physical record
#     (often multiple rows per field/treatment). Duplicates in `sample_id`
#     may be expected and are OK at this stage.
#   - Aggregation to reporting grain (e.g., per sample_id × measurement) is
#     performed in the report (QMD) or using `aggregate_replicates_long()`
#     *after* pivoting to long form.
#
# Public functions
#   - load_lab_data(path)
#   - clean_data(df, config)
#   - join_dictionary(df, dictionary_path)
#   - pivot_long_with_dictionary(df, dictionary_path)        [optional helper]
#   - aggregate_replicates_long(long_df)                     [optional helper]
# =============================================================================

# ---- small utils ------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

# Given a data frame and a derivation rule, build `sample_id` if missing.
# Supported rules:
#   - "producer_id+num_trt"
#   - "producer_id+field_id"
#   - "Farm.Name+Treatment.ID"      (legacy Think Tank)
# If `rule` is NULL, tries common fallbacks automatically.
derive_sample_id <- function(data, rule = NULL) {
  # If already present, keep as-is
  if ("sample_id" %in% names(data)) return(data)

  # Ensure compatible columns exist or bail with helpful error
  choose_rule <- function(df, rule) {
    if (!is.null(rule)) return(rule)
    # Heuristic fallbacks by availability
    if (all(c("producer_id", "num_trt") %in% names(df))) return("producer_id+num_trt")
    if (all(c("producer_id", "field_id") %in% names(df))) return("producer_id+field_id")
    if (all(c("Farm.Name", "Treatment.ID") %in% names(df))) return("Farm.Name+Treatment.ID")
    return(NULL)
  }

  rule <- choose_rule(data, rule)

  if (is.null(rule)) {
    stop(
      "sample_id is missing and cannot be derived: ",
      "expected one of {producer_id+num_trt, producer_id+field_id, Farm.Name+Treatment.ID}. ",
      "Add mapping/columns or configure derive.sample_id in config.yml."
    )
  }

  if (identical(rule, "producer_id+num_trt")) {
    stopifnot(all(c("producer_id", "num_trt") %in% names(data)))
    data$sample_id <- paste(data$producer_id, data$num_trt, sep = "_")
    return(data)
  }

  if (identical(rule, "producer_id+field_id")) {
    stopifnot(all(c("producer_id", "field_id") %in% names(data)))
    data$sample_id <- paste(data$producer_id, data$field_id, sep = "_")
    return(data)
  }

  if (identical(rule, "Farm.Name+Treatment.ID")) {
    stopifnot(all(c("Farm.Name", "Treatment.ID") %in% names(data)))
    data$sample_id <- paste(data$Farm.Name, data$Treatment.ID, sep = "_")
    # Some legacy data include a replicate column
    if ("Rep" %in% names(data)) {
      data$sample_id <- paste(data$sample_id, data$Rep, sep = "_")
    }
    return(data)
  }

  stop("Unsupported derive rule for sample_id: ", rule)
}

# ---- load & clean -----------------------------------------------------------

#' Load laboratory data from CSV file
#'
#' Reads a CSV robustly using base utils; if that fails and {readr} is
#' available, falls back to readr::read_csv().
#'
#' @param path character. Path to CSV file.
#' @return data.frame (or tibble) with loaded data
load_lab_data <- function(path) {
  if (!file.exists(path)) stop("Data file not found: ", path)

  data <- tryCatch({
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  }, error = function(e) {
    if (requireNamespace("readr", quietly = TRUE)) {
      readr::read_csv(path, show_col_types = FALSE)
    } else {
      stop("Failed to read data file: ", e$message)
    }
  })

  if (nrow(data) == 0) stop("Data file is empty")

  message("Loaded ", nrow(data), " rows from ", path)
  data
}

#' Clean and standardize data frame (wide form)
#'
#' Normalizes key identifier columns and simple types, *without* collapsing
#' replicates. Duplicate `sample_id` values are expected and allowed; the
#' reporting layer performs averaging.
#'
#' @param df data.frame. Raw/mapped upload (wide).
#' @param config list. App configuration; may include:
#'   - default_year: numeric fallback if `year` missing
#'   - derive$sample_id: one of "producer_id+num_trt", "producer_id+field_id",
#'                       "Farm.Name+Treatment.ID"
#'   - required_columns: vector of columns to check for presence (not uniqueness)
#' @return data.frame cleaned (still wide)
clean_data <- function(df, config) {
  data <- df

  # Normalize common identifiers if missing (light-touch; no overwrites)
  if (!"producer_id" %in% names(data) && "Farm.Name" %in% names(data)) {
    data$producer_id <- data$Farm.Name
  }
  if (!"field_id" %in% names(data) && "Treatment.ID" %in% names(data)) {
    data$field_id <- data$Treatment.ID
  }
  if (!"year" %in% names(data)) {
    data$year <- as.numeric(config$default_year %||% 2024)
  }

  # Coordinates as numeric (if present)
  if ("latitude" %in% names(data))  data$latitude  <- as.numeric(data$latitude)
  if ("longitude" %in% names(data)) data$longitude <- as.numeric(data$longitude)

  # Require sample_id to be present (no automatic generation)
  if (!"sample_id" %in% names(data)) {
    stop("sample_id column is required but missing from the data. Please ensure your data includes a sample_id column.")
  }

  # Remove rows missing *any* required column values (if configured)
  # Presence is validated; uniqueness is not enforced here.
  req <- config$required_columns %||% NULL
  if (!is.null(req)) {
    # Only check columns that exist; absent columns are caught by validation later
    cols_to_check <- intersect(req, names(data))
    if (length(cols_to_check)) {
      missing_required <- apply(data[, cols_to_check, drop = FALSE], 1, function(row) any(is.na(row)))
      if (any(missing_required)) {
        message("Removing ", sum(missing_required), " rows with missing required data (",
                paste(cols_to_check, collapse = ", "), ")")
        data <- data[!missing_required, , drop = FALSE]
      }
    }
  }

  # Duplicate `sample_id` is allowed; print an informative message instead of a warning.
  if ("sample_id" %in% names(data) && any(duplicated(data$sample_id))) {
    dct <- sum(duplicated(data$sample_id))
    message("Note: ", dct, " duplicated sample_id rows present (expected before aggregation).")
  }

  message("Cleaned data: ", nrow(data), " rows remaining")
  data
}

#' Join data with dictionary information (wide annotations)
#'
#' Adds measurement metadata to the *wide* data frame as attributes on
#' each measurement column and attaches a `measurement_info` table to `df`.
#' This is useful for downstream pivot helpers or for modules that want to
#' know which columns are "measurements".
#'
#' @param df data.frame (wide)
#' @param dictionary_path character. Path to data dictionary CSV.
#' @return data.frame with attributes populated
join_dictionary <- function(df, dictionary_path) {
  if (!file.exists(dictionary_path)) {
    stop("Data dictionary not found: ", dictionary_path)
  }

  dict <- utils::read.csv(dictionary_path, stringsAsFactors = FALSE)

  required_cols <- c("column_name", "measurement_group", "unit")
  missing_cols <- setdiff(required_cols, names(dict))
  if (length(missing_cols)) {
    stop("Data dictionary missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Accept 'abbr' or 'abbreviation'
  abbr_col <- if ("abbreviation" %in% names(dict)) "abbreviation" else "abbr"
  if (!abbr_col %in% names(dict)) {
    stop("Data dictionary missing abbreviation column (expected 'abbr' or 'abbreviation').")
  }

  measurement_cols <- intersect(dict$column_name, names(df))
  if (!length(measurement_cols)) {
    warning("No measurement columns from dictionary found in data")
    return(df)
  }

  info <- dict[dict$column_name %in% measurement_cols, , drop = FALSE]

  for (col in measurement_cols) {
    meta <- info[info$column_name == col, , drop = FALSE]
    if (nrow(meta)) {
      attr(df[[col]], "measurement_group") <- meta$measurement_group[1]
      attr(df[[col]], "abbreviation")      <- meta[[abbr_col]][1]
      attr(df[[col]], "unit")              <- meta$unit[1]
    }
  }

  attr(df, "measurement_info") <- info
  message("Joined dictionary info for ", length(measurement_cols), " measurements")
  df
}

# ---- optional helpers for long + aggregation --------------------------------

#' Pivot to long and attach dictionary metadata (then ready for aggregation)
#'
#' Detects measurement columns via the dictionary, converts them to numeric,
#' pivots to long, and joins dictionary metadata (group, abbr, unit & ordering).
#'
#' @param df data.frame (wide)
#' @param dictionary_path character
#' @return tibble/data.frame with columns:
#'   producer_id, year, sample_id, field_id (if present), measurement,
#'   value, measurement_group, abbr, unit, abbr_unit, measurement_order, group_order
pivot_long_with_dictionary <- function(df, dictionary_path) {
  if (!file.exists(dictionary_path)) stop("Data dictionary not found: ", dictionary_path)
  dict <- utils::read.csv(dictionary_path, stringsAsFactors = FALSE)

  abbr_col <- if ("abbreviation" %in% names(dict)) "abbreviation" else "abbr"
  if (!abbr_col %in% names(dict)) {
    stop("Data dictionary missing abbreviation column (expected 'abbr' or 'abbreviation').")
  }

  # figure measurement columns present in df
  measure_cols <- intersect(dict$column_name, names(df))
  if (!length(measure_cols)) stop("No measurement columns found in data per dictionary")

  # ensure numeric and track conversions
  conversion_log <- list()
  df[measure_cols] <- lapply(df[measure_cols], function(x) {
    # Check for non-numeric values before conversion
    original_na <- is.na(x)
    numeric_x <- suppressWarnings(as.numeric(x))
    new_na <- is.na(numeric_x)
    
    # Find values that became NA (were non-numeric)
    converted_to_na <- !original_na & new_na
    if (any(converted_to_na)) {
      non_numeric_values <- unique(x[converted_to_na])
      conversion_log <<- c(conversion_log, list(non_numeric_values))
    }
    
    numeric_x
  })
  
  # Log conversions if any occurred
  if (length(conversion_log) > 0) {
    for (i in seq_along(measure_cols)) {
      if (i <= length(conversion_log) && length(conversion_log[[i]]) > 0) {
        col_name <- measure_cols[i]
        non_numeric_vals <- conversion_log[[i]]
        message("⚠️  Column '", col_name, "': ", length(non_numeric_vals), " non-numeric values converted to missing: ", 
                paste(head(non_numeric_vals, 5), collapse = ", "), 
                if (length(non_numeric_vals) > 5) paste(" (and", length(non_numeric_vals) - 5, "more)") else "")
      }
    }
  }

  # long
  long <- tidyr::pivot_longer(
    data = df,
    cols  = dplyr::all_of(measure_cols),
    names_to = "measurement",
    values_to = "value"
  )

  dict2 <- dict[dict$column_name %in% measure_cols, , drop = FALSE] |>
    dplyr::mutate(
      abbr_unit = glue::glue("{.data[[abbr_col]]}<br>{unit}"),
      group_order = dplyr::cur_group_id(),
      measurement_order = seq_along(column_name),
      .by = measurement_group
    ) |>
    dplyr::rename(abbr = dplyr::all_of(abbr_col))

  long <- dplyr::inner_join(long, dict2, by = c("measurement" = "column_name"))

  long
}

#' Aggregate replicate rows to the reporting grain (mean)
#'
#' Averages numeric `value` per
#'   producer_id, year, sample_id, measurement, measurement_group, abbr, unit, abbr_unit.
#'
#' @param long_df data.frame produced by `pivot_long_with_dictionary()` or equivalent
#' @return aggregated long data.frame
aggregate_replicates_long <- function(long_df) {
  req_cols <- c("producer_id","year","sample_id","measurement",
                "measurement_group","abbr","unit","abbr_unit","value")
  miss <- setdiff(req_cols, names(long_df))
  if (length(miss)) stop("aggregate_replicates_long: missing columns: ", paste(miss, collapse=", "))

  long_df |>
    dplyr::group_by(producer_id, year, sample_id,
                    measurement, measurement_group, abbr, unit, abbr_unit) |>
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
}