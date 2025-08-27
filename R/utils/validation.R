# =============================================================================
# validation.R — Data Validation Functions
#
# Purpose:
#   - Validate uploaded Excel template files
#   - Check for required sheets, columns, and data integrity
#   - Follow the dirt-data-reports validation pattern
#
# =============================================================================

library(tidyverse)
library(readxl)

validate_data_file <- function(file_path, req_fields, language = "english") {
  error_list <- list()
  
  # Read required fields configuration
  req_fields_data <- req_fields %>% filter(sheet == "Data")
  req_fields_dd <- req_fields %>% filter(sheet == "Data Dictionary")
  
  ### Check 1: Does the xlsx file have 'Data' and 'Data Dictionary' tabs?
  sheets_present <- readxl::excel_sheets(file_path)
  required_sheets <- c("Data", "Data Dictionary")
  check1 <- all(required_sheets %in% sheets_present)
  
  if (!check1) {
    error_list$check1 <- paste("Missing sheets:", paste(setdiff(required_sheets, sheets_present), collapse = ", "))
    return(error_list) # Critical failure, stop further checks
  }
  
  # Load the sheets
  data <- tryCatch(
    readxl::read_xlsx(file_path, sheet = "Data"),
    error = function(e) {
      error_list$data_read_error <- paste("Error reading Data sheet:", e$message)
      return(NULL)
    }
  )
  
  data_dict <- tryCatch(
    readxl::read_xlsx(file_path, sheet = "Data Dictionary"),
    error = function(e) {
      error_list$dict_read_error <- paste("Error reading Data Dictionary sheet:", e$message)
      return(NULL)
    }
  )
  
  if (is.null(data) || is.null(data_dict)) {
    return(error_list) # Stop further validation if sheets couldn't be loaded
  }
  
  ### Check 2: Does Data sheet have actual data rows?
  if (nrow(data) == 0) {
    error_list$check2 <- "The Data sheet contains headers but no data rows. Please add your measurement data."
    return(error_list)
  }
  
  ### Check 3: Does 'Data' have required columns?
  required_columns <- req_fields_data$var[req_fields_data$required == TRUE]
  check3 <- all(required_columns %in% colnames(data))
  
  if (!check3) {
    missing_cols <- setdiff(required_columns, colnames(data))
    error_list$check3 <- paste("Missing required columns in Data:", paste(missing_cols, collapse = ", "))
  }
  
  ### Check 4: Does 'Data Dictionary' have required fields?
  required_dict_fields <- req_fields_dd$var[req_fields_dd$required == TRUE]
  check4 <- all(required_dict_fields %in% colnames(data_dict))
  
  if (!check4) {
    missing_dict_fields <- setdiff(required_dict_fields, colnames(data_dict))
    error_list$check4 <- paste("Missing required columns in Data Dictionary:", paste(missing_dict_fields, collapse = ", "))
  }
  
  ### Check 5: Validate data types and content for required fields
  for (i in 1:nrow(req_fields_data)) {
    field <- req_fields_data$var[i]
    data_type <- req_fields_data$data_type[i]
    validation_rule <- req_fields_data$validation_rule[i]
    required <- req_fields_data$required[i]
    
    if (field %in% colnames(data)) {
      # Check data type
      if (data_type == "integer" && required) {
        if (!all(is.na(data[[field]]) | sapply(data[[field]], function(x) is.numeric(x) && x == as.integer(x)))) {
          error_list[[paste0("data_type_", field)]] <- paste(field, "must contain integer values")
        }
      }
      
      # Check validation rules
      if (validation_rule == "no_duplicates" && required) {
        duplicates <- data %>%
          count(!!sym(field)) %>%
          filter(n > 1)
        if (nrow(duplicates) > 0) {
          error_list[[paste0("duplicates_", field)]] <- paste("Duplicate", field, "values found:", paste(duplicates[[field]], collapse = ", "))
        }
      }
      
      if (validation_rule == "not_empty" && required) {
        if (any(is.na(data[[field]]) | data[[field]] == "")) {
          error_list[[paste0("empty_", field)]] <- paste(field, "cannot contain empty values")
        }
      }
      
      if (validation_rule == ">= 2000" && required) {
        if (any(!is.na(data[[field]]) & data[[field]] < 2000)) {
          error_list[[paste0("year_", field)]] <- paste(field, "must be >= 2000")
        }
      }
      
      if (validation_rule == "valid_texture" && required) {
        valid_textures <- c("sand", "loamy sand", "sandy loam", "loam", "silt loam", "silt", "sandy clay loam", "clay loam", "silty clay loam", "sandy clay", "silty clay", "clay")
        invalid_textures <- unique(data[[field]][!is.na(data[[field]]) & !tolower(data[[field]]) %in% valid_textures])
        if (length(invalid_textures) > 0) {
          error_list[[paste0("texture_", field)]] <- paste("Invalid texture values:", paste(invalid_textures, collapse = ", "))
        }
      }
    }
  }
  
  ### Check 6: Check that data dictionary matches data columns
  if (nrow(data_dict) > 0 && "column_name" %in% colnames(data_dict)) {
    data_cols <- colnames(data)
    dict_cols <- data_dict$column_name
    
    # Smart detection: Only columns listed in Data Dictionary are considered measurements
    # This makes the validation completely flexible - the Data Dictionary defines what's a measurement
    
    # Check if all measurement columns (those in Data Dictionary) exist in data
    missing_data <- setdiff(dict_cols, data_cols)
    if (length(missing_data) > 0) {
      error_list$check6 <- paste("Data Dictionary entries for non-existent columns:", paste(missing_data, collapse = ", "))
    }
    
    # Optional: Check if there are data columns not in dictionary (these are treated as metadata)
    # This is just informational - not an error
    extra_data <- setdiff(data_cols, dict_cols)
    if (length(extra_data) > 0) {
      message("Note: The following columns are not in Data Dictionary and will be treated as metadata: ", 
              paste(extra_data, collapse = ", "))
    }
  }
  
  return(error_list)
}

# Helper function to validate uniqueness constraints
validate_uniqueness <- function(data, req_fields) {
  error_list <- list()
  
  unique_checks <- req_fields %>% filter(unique_by != "-")
  
  for (i in seq_len(nrow(unique_checks))) {
    var_name <- unique_checks$var[i]
    unique_by <- unique_checks$unique_by[i]
    
    if (var_name %in% colnames(data)) {
      if (unique_by == var_name) {
        # Globally unique
        duplicates <- data %>%
          count(!!sym(var_name)) %>%
          filter(n > 1)
        
        if (nrow(duplicates) > 0) {
          error_list[[var_name]] <- paste(
            "Duplicate values found in", var_name, "which should be unique:",
            paste(duplicates[[var_name]], collapse = ", ")
          )
        }
      } else {
        # Unique within a group
        group_vars <- str_split(unique_by, ",\\s*")[[1]]
        if (all(group_vars %in% colnames(data))) {
          duplicates <- data %>%
            group_by(across(all_of(group_vars))) %>%
            add_count(!!sym(var_name), name = "field_count") %>%
            filter(field_count > 1) %>%
            distinct(across(all_of(c(group_vars, var_name)))) %>%
            ungroup()
          
          if (nrow(duplicates) > 0) {
            error_list[[var_name]] <- paste(
              "Duplicate values found in", var_name, "within grouping of", unique_by
            )
          }
        }
      }
    }
  }
  
  return(error_list)
} 