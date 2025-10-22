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
      
      # Try to validate and read the Excel file
      tryCatch({
        # Load validation functions
        source("R/utils/validation.R")
        
        # Read required fields configuration
        req_fields <- read.csv("config/required-fields.csv")
        
        # Validate the uploaded file
        validation_results <- validate_data_file(input$file$datapath, req_fields)
        
        if (length(validation_results) == 0) {
          # Validation successful - update state
          show_validation_success(paste("✅ Data uploaded successfully!", "All validation checks passed."))
          
          # Read the validated data
          uploaded_data <- readxl::read_xlsx(input$file$datapath, sheet = "Data")
          uploaded_data_dictionary <- readxl::read_xlsx(input$file$datapath, sheet = "Data Dictionary")
          
          # Attach dictionary to data as attribute (required by template)
          attr(uploaded_data, "measurement_info") <- uploaded_data_dictionary
          
          # Check for non-numeric values in measurement columns
          conversion_warnings <- check_numeric_conversions(uploaded_data, uploaded_data_dictionary)
          
          # Update the shared state
          # Store both original and filtered data
          state$data_unfiltered <- uploaded_data  # Keep original for report generation
          state$data <- uploaded_data             # This will be filtered by Step 3
          state$data_dictionary <- uploaded_data_dictionary
          state$data_uploaded <- TRUE
          
          # Show conversion warnings if any
          if (length(conversion_warnings) > 0) {
            show_conversion_warnings(conversion_warnings, ns)
          }
          
          # Mark step 2 as valid for stepper
          state$step_2_valid <- TRUE
          
        } else {
          # Validation failed - show errors
          show_validation_error("Please review the following errors and re-upload:")
          
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
            )
          )
          
          insertUI(
            selector = paste0("#", ns("validation_status")),
            where = "beforeEnd",
            ui = error_ui
          )
        }
        
      }, error = function(e) {
        show_validation_error(paste("Error reading file:", e$message))
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
