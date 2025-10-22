# =============================================================================
# mod_data_upload.R — Data Upload Module (UI + Server)
#
# Purpose:
#   - Handles the second step: uploading completed Excel templates
#   - Validates uploaded data against template structure
#   - Updates app state with validated data
#
# Inputs:
#   id  : Module namespace ID
#   cfg : list, configuration from global.R
#
# Outputs:
#   Updates state$data, state$data_dictionary, state$data_uploaded
# =============================================================================

# ---------------------------
# Data Upload Module UI
# ---------------------------
mod_data_upload_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Data Upload"),

    div(
      style = "padding: 15px; background-color: #f8f9fa; border-radius: 5px; margin: 10px 0;",
      h5("Upload Completed Template"),
      p("Upload your completed Excel template with soil health data."),
      
      fileInput(
        ns("file"),
        "Choose Excel file:",
        accept = c(".xlsx", ".xls"),
        placeholder = "No file selected"
      ),
      helpText("Upload a completed Excel template (.xlsx or .xls)"),
      
      # Validation status area
      div(id = ns("validation_status"))
    ),

    # Show preview if data is available
    conditionalPanel(
      condition = "output.has_data",
      ns = ns,
      tags$hr(),
      h5("Data Preview"),
      verbatimTextOutput(ns("data_info")),
      DT::dataTableOutput(ns("data_preview"))
    )
  )
}

# ---------------------------
# Helper Functions
# ---------------------------

# Check for non-numeric values in measurement columns
check_numeric_conversions <- function(df, dictionary) {
  if (!"column_name" %in% names(dictionary)) return(list())
  
  # Get measurement columns from dictionary (exclude texture which is text)
  measure_cols <- intersect(dictionary$column_name, names(df))
  measure_cols <- measure_cols[measure_cols != "texture"]  # Exclude texture as it's text data
  if (length(measure_cols) == 0) return(list())
  
  conversion_warnings <- list()
  
  for (col in measure_cols) {
    # Check for non-numeric values
    original_na <- is.na(df[[col]])
    numeric_converted <- suppressWarnings(as.numeric(df[[col]]))
    new_na <- is.na(numeric_converted)
    
    # Find values that became NA (were non-numeric)
    converted_to_na <- !original_na & new_na
    if (any(converted_to_na)) {
      non_numeric_values <- unique(df[[col]][converted_to_na])
      conversion_warnings[[col]] <- list(
        column = col,
        count = length(non_numeric_values),
        values = head(non_numeric_values, 5),
        total_values = length(non_numeric_values)
      )
    }
  }
  
  conversion_warnings
}

# Show conversion warnings in UI
show_conversion_warnings <- function(warnings, ns) {
  warning_ui <- div(
    class = "alert alert-warning",
    style = "margin-top: 10px;",
    icon("exclamation-triangle"),
    tags$strong("Data Conversion Warnings:"),
    tags$p("Some non-numeric values were found in numeric measurement columns and converted to missing values:"),
    tags$ul(
      lapply(warnings, function(warning) {
        values_text <- paste(warning$values, collapse = ", ")
        if (warning$total_values > 5) {
          values_text <- paste0(values_text, " (and ", warning$total_values - 5, " more)")
        }
        tags$li(
          tags$strong(warning$column), ": ", warning$count, " non-numeric values converted to missing (", values_text, ")"
        )
      })
    ),
    tags$p(tags$em("These values will be excluded from calculations but won't prevent report generation."))
  )
  
  insertUI(
    selector = paste0("#", ns("validation_status")),
    where = "beforeEnd",
    ui = warning_ui
  )
}

# ---------------------------
# Data Upload Module Server
# ---------------------------
mod_data_upload_server <- function(id, cfg, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Handle file upload
    observeEvent(input$file, {
      req(input$file)
      
      # Debug: Show that file upload was detected
      showNotification(paste("File uploaded:", input$file$name), type = "message")
      
      # Clear previous status
      removeUI(selector = paste0("#", ns("validation_status"), " > *"), immediate = TRUE)
      
      # Validate file extension
      ext <- tolower(tools::file_ext(input$file$name))
      if (!ext %in% c("xlsx", "xls")) {
        show_validation_error("Please upload an Excel file (.xlsx or .xls)")
        return()
      }
      
      # Clear any previous validation messages
      removeUI(selector = paste0("#", ns("validation_status"), " > *"), multiple = TRUE)
      
      # Try to read and validate the Excel file
      tryCatch({
        # First, try to read the data regardless of validation
        uploaded_data <- NULL
        uploaded_data_dictionary <- NULL
        
        # Attempt to read the Excel file
        tryCatch({
          uploaded_data <- readxl::read_xlsx(input$file$datapath, sheet = "Data")
          uploaded_data_dictionary <- readxl::read_xlsx(input$file$datapath, sheet = "Data Dictionary")
          
          # Attach dictionary to data as attribute (required by template)
          attr(uploaded_data, "measurement_info") <- uploaded_data_dictionary
          
          # Update state with data (even if validation fails)
          state$data_unfiltered <- uploaded_data
          state$data <- uploaded_data
          state$data_dictionary <- uploaded_data_dictionary
          state$data_uploaded <- TRUE
          
        }, error = function(read_error) {
          show_validation_error(paste("❌ Error reading Excel file:", read_error$message, 
                                    "\n\nPlease check that your file has 'Data' and 'Data Dictionary' sheets."))
          return()
        })
        
        # Now try validation if data was read successfully
        if (!is.null(uploaded_data)) {
          tryCatch({
            # Load validation functions
            source("R/utils/validation.R")
            
            # Read required fields configuration using readr for better CSV parsing
            req_fields <- readr::read_csv("config/required-fields.csv", 
                                        col_types = readr::cols(.default = "c"),
                                        locale = readr::locale(encoding = "UTF-8"),
                                        show_col_types = FALSE)
            
            # Convert required column to logical
            req_fields$required <- as.logical(req_fields$required)
            
            
            
            # Validate the uploaded file
            validation_results <- validate_data_file(input$file$datapath, req_fields)
            
            if (length(validation_results) == 0) {
              # Validation successful
              show_validation_success("✅ Data uploaded successfully! All validation checks passed.")
              
              # Check for non-numeric values in measurement columns
              conversion_warnings <- check_numeric_conversions(uploaded_data, uploaded_data_dictionary)
              
              # Show conversion warnings if any
              if (length(conversion_warnings) > 0) {
                show_conversion_warnings(conversion_warnings, ns)
              }
              
              # Mark step 2 as valid for stepper
              state$step_2_valid <- TRUE
              
            } else {
              # Validation failed - show errors and block progression
              show_validation_error("❌ Data validation failed. Please fix the following issues before proceeding:")
              
              error_ui <- div(
                class = "alert alert-danger",
                icon("exclamation-triangle"),
                tags$strong("Validation Errors:"),
                tags$ul(
                  lapply(validation_results, function(error) {
                    if (is.list(error)) {
                      lapply(error, function(sub_error) {
                        tags$li(sub_error)
                      })
                    } else {
                      tags$li(error)
                    }
                  })
                ),
                tags$p(tags$em("Please fix these issues and re-upload your data to continue."))
              )
              
              insertUI(
                selector = paste0("#", ns("validation_status")),
                where = "beforeEnd",
                ui = error_ui
              )
              
              # Block progression - don't mark as valid
              state$step_2_valid <- FALSE
            }
            
          }, error = function(validation_error) {
            # Validation function failed - block progression
            error_details <- paste0(
              "❌ Data validation failed: ", validation_error$message,
              "\n\nError details: ", 
              if (!is.null(validation_error$call)) {
                paste("Function:", deparse(validation_error$call)[1])
              } else {
                "Unknown function"
              },
              "\n\nThis usually means there's an issue with the validation configuration or data format.",
              "\nPlease check your data matches the template format and try again."
            )
            show_validation_error(error_details)
            state$step_2_valid <- FALSE
          })
        }
        
      }, error = function(e) {
        show_validation_error(paste("❌ Unexpected error:", e$message, 
                                  "\n\nPlease check your file format and try again."))
      })
    })

    # Helper functions for validation UI
    show_validation_success <- function(message) {
      success_ui <- div(
        class = "alert alert-success",
        icon("check-circle"),
        tags$strong("Success:"),
        message
      )
      insertUI(
        selector = paste0("#", ns("validation_status")),
        where = "beforeEnd",
        ui = success_ui
      )
    }

    show_validation_error <- function(message) {
      error_ui <- div(
        class = "alert alert-danger",
        icon("exclamation-triangle"),
        tags$strong("Error:"),
        message
      )
      insertUI(
        selector = paste0("#", ns("validation_status")),
        where = "beforeEnd",
        ui = error_ui
      )
    }

    # Output for conditional visibility
    output$has_data <- reactive({
      state$data_uploaded && !is.null(state$data)
    })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)

    # Data info output
    output$data_info <- renderPrint({
      req(state$data)
      cat("Dataset loaded successfully!\n")
      cat("Rows:", nrow(state$data), "\n")
      cat("Columns:", ncol(state$data), "\n")
      cat("Producer IDs:", paste(unique(state$data$producer_id), collapse = ", "), "\n")
      cat("Years:", paste(unique(state$data$year), collapse = ", "), "\n")
    })

    # Data preview output
    output$data_preview <- DT::renderDataTable({
      req(state$data)
      DT::datatable(
        head(state$data, 10),
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          dom = "t"
        )
      )
    })
  })
}
