# validate.R ----

#' Validate that required columns are present in the dataset
#' @param df Data frame to validate
#' @param required Character vector of required column names
#' @return TRUE if valid, stops with error message if not
validate_required_columns <- function(df, required) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
  TRUE
}

#' Validate that coordinate columns exist and contain numeric values
#' @param df Data frame to validate
#' @param lat_col Name of latitude column (default: "latitude")
#' @param lon_col Name of longitude column (default: "longitude")
#' @return TRUE if valid, stops with error message if not
validate_coordinates <- function(df, lat_col = "latitude", lon_col = "longitude") {
  # Check columns exist
  if (!lat_col %in% names(df)) {
    stop("Latitude column '", lat_col, "' not found")
  }
  if (!lon_col %in% names(df)) {
    stop("Longitude column '", lon_col, "' not found")
  }
  
  # Check for numeric values - be more informative about what's wrong
  lat_vals <- as.numeric(df[[lat_col]])
  lon_vals <- as.numeric(df[[lon_col]])
  
  if (any(is.na(lat_vals))) {
    non_numeric_lat <- df[[lat_col]][is.na(lat_vals)]
    warning("Non-numeric values found in latitude column: ", paste(unique(non_numeric_lat), collapse = ", "))
    return(FALSE)  # Return FALSE instead of stopping
  }
  if (any(is.na(lon_vals))) {
    non_numeric_lon <- df[[lon_col]][is.na(lon_vals)]
    warning("Non-numeric values found in longitude column: ", paste(unique(non_numeric_lon), collapse = ", "))
    return(FALSE)  # Return FALSE instead of stopping
  }
  
  # Check reasonable ranges
  if (any(lat_vals < -90 | lat_vals > 90)) {
    warning("Latitude values outside valid range (-90 to 90)")
    return(FALSE)
  }
  if (any(lon_vals < -180 | lon_vals > 180)) {
    warning("Longitude values outside valid range (-180 to 180)")
    return(FALSE)
  }
  
  TRUE
}

#' Validate that specified producer and year combination exists in data
#' @param df Data frame to validate
#' @param producer_id Producer ID to check
#' @param year Year to check
#' @param producer_col Name of producer column (default: "producer_id")
#' @param year_col Name of year column (default: "year")
#' @return TRUE if valid, stops with error message if not
validate_producer_year <- function(df, producer_id, year, 
                                  producer_col = "producer_id", 
                                  year_col = "year") {
  # Check columns exist
  if (!producer_col %in% names(df)) {
    stop("Producer column '", producer_col, "' not found")
  }
  if (!year_col %in% names(df)) {
    stop("Year column '", year_col, "' not found")
  }
  
  # Check if producer exists
  if (!producer_id %in% df[[producer_col]]) {
    available <- unique(df[[producer_col]])
    stop("Producer '", producer_id, "' not found. Available: ", 
         paste(available, collapse = ", "))
  }
  
  # Check if year exists for this producer
  producer_data <- df[df[[producer_col]] == producer_id, ]
  if (!year %in% producer_data[[year_col]]) {
    available_years <- unique(producer_data[[year_col]])
    stop("Year ", year, " not found for producer '", producer_id, "'. Available: ", 
         paste(available_years, collapse = ", "))
  }
  
  TRUE
}

#' Validate that measurement columns from dictionary are present in data
#' @param df Data frame to validate
#' @param dictionary_path Path to data dictionary CSV
#' @return TRUE if valid, stops with error message if not
validate_measurements_present <- function(df, dictionary_path) {
  if (!file.exists(dictionary_path)) {
    stop("Data dictionary not found at: ", dictionary_path)
  }
  
  # Load dictionary
  dict <- read.csv(dictionary_path, stringsAsFactors = FALSE)
  
  # Get measurement columns from dictionary
  if (!"column_name" %in% names(dict)) {
    stop("Data dictionary must contain 'column_name' column")
  }
  
  measurement_cols <- dict$column_name[!is.na(dict$column_name)]
  
  # Check which measurements are missing
  missing <- setdiff(measurement_cols, names(df))
  
  if (length(missing) > 0) {
    warning("Missing measurement columns: ", paste(missing, collapse = ", "))
  }
  
  # Check if we have at least some measurements
  present <- intersect(measurement_cols, names(df))
  if (length(present) == 0) {
    stop("No measurement columns found in dataset")
  }
  
  TRUE
}
