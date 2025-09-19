# =============================================================================
# mod_upload.R — Data Upload Module (UI + Server)
#
# Purpose:
#   - Provides the first step in the workflow: load a dataset for reporting.
#   - Supports three modes:
#       1) Download Excel template
#       2) User-uploaded CSV/Excel
#       3) Pre-loaded server dataset (bundled under ./data/)
#   - Displays a preview (first 10 rows) and basic dataset info.
#
# Inputs:
#   id  : Module namespace ID
#   cfg : list, configuration from global.R (currently not used here)
#
# Outputs (reactive list to parent):
#   $data : loaded data.frame (raw, not cleaned or mapped)
#   $path : file path to loaded CSV (tmp path for uploads, project path for server dataset)
#   $mode : "template" | "upload" | "server"
#
# Notes:
#   - Calls load_lab_data() from R/logic/data.R to read the dataset.
#   - No cleaning/mapping here; that happens in the mapping/logic layers.
# =============================================================================

# ---------------------------
# Download Module UI
# ---------------------------
mod_download_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Template Download"),

    div(
      style = "padding: 15px; background-color: #f8f9fa; border-radius: 5px; margin: 10px 0;",
      h5("Excel Template"),
      p("Download our standardized Excel template to ensure your data is in the correct format."),
      downloadButton(
        ns("download_template"),
        "Download Template",
        class = "btn-primary",
        style = "width: 100%; margin-bottom: 10px;"
      ),
      
      # Tabbed interface for template instructions
      tabsetPanel(
        id = ns("template_tabs"),
        type = "tabs",
        tabPanel(
          "How to Use This App",
          div(
            style = "max-height: 500px; overflow-y: auto; padding: 10px;",
            h4("Complete Workflow Guide"),
            
            h5("📋 Step 1: Download Template"),
            p("Download the Excel template and fill it out with your soil health data. Each row represents one soil sample."),
            
            h5("📤 Step 2: Upload Your Data"),
            p("Upload your completed Excel file. The app will validate your data and show any issues that need to be fixed."),
            
            h5("🔍 Step 3: Filter Your Data"),
            p("Use the filters to focus on specific crops, soil textures, or other characteristics. This helps you compare similar conditions."),
            
            h5("📝 Step 4: Customize Project Info"),
            p("Add your project name, summary, and any additional notes that will appear in your report."),
            
            h5("👤 Step 5: Select Your Data"),
            p("Choose the specific producer and year you want to analyze. This determines which samples will be highlighted as 'Your Fields' in the report."),
            
            h5("📊 Step 6: Choose Grouping"),
            p("Select how you want to group your data (by field, treatment, etc.) or choose 'No grouping' for farm-level comparisons."),
            
            h5("📄 Step 7: Generate Report"),
            p("Click 'Generate Report' to create your professional soil health report. This may take a few minutes."),
            
            hr(),
            
            h5("📋 Template Requirements:"),
            tags$ul(
              tags$li(tags$strong("Required columns:"), "year, sample_id, producer_id, field_id"),
              tags$li(tags$strong("sample_id must be unique"), "for each sample"),
              tags$li(tags$strong("Texture is required"), "for all samples"),
              tags$li(tags$strong("Coordinates are optional"), "but enable mapping features"),
              tags$li("Extra columns will be ignored by the app")
            ),
            
          )
        )
      )
    )
  )
}

# ---------------------------
# Download Module Server
# ---------------------------
mod_download_server <- function(id, cfg, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Template download handler
    output$download_template <- downloadHandler(
      filename = function() {
        paste0("soil-health-template-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        # Copy the pre-made template file
        template_path <- "files/soil-health-template.xlsx"
        if (file.exists(template_path)) {
          file.copy(template_path, file)
        } else {
          stop("Template file not found. Please ensure soil-health-template.xlsx exists in the files folder.")
        }
      }
    )
    

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
          
          # Update the shared state
          state$data <- uploaded_data
          state$data_dictionary <- uploaded_data_dictionary
          state$data_uploaded <- TRUE
          
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

    # Handle server dataset selection
    observeEvent(input$server_dataset, {
      req(input$mode == "server", input$server_dataset)
      
      tryCatch({
        source("R/logic/data.R")  # exposes load_lab_data()
        
        # Load the selected server dataset
        data_path <- file.path("data", input$server_dataset)
        df <- load_lab_data(data_path)
        
        req(nrow(df) > 0)
        
        # Update the shared state
        state$data <- df
        state$data_dictionary <- NULL  # Server datasets don't have data dictionaries
        state$data_uploaded <- TRUE
        
        showNotification("Server dataset loaded successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(
          paste("Error loading server dataset:", e$message),
          type = "error",
          duration = NULL
        )
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

    # Output: dataset info text
    output$data_info <- renderPrint({
      req(state$data_uploaded, state$data)
      df <- state$data
      cat("Dataset Info:\n")
      cat("Rows:", nrow(df), "\n")
      cat("Columns:", ncol(df), "\n")
      cat("Column names:", paste(names(df), collapse = ", "), "\n")
    })

    # Output: first 10 rows preview
    output$data_preview <- DT::renderDataTable({
      req(state$data_uploaded, state$data)
      df <- state$data
      DT::datatable(
        df[seq_len(min(10, nrow(df))), , drop = FALSE],
        options = list(pageLength = 5, scrollX = TRUE)
      )
    })

    # Output: flag for conditionalPanel preview display
    output$has_data <- reactive({ 
      state$data_uploaded && !is.null(state$data)
    })
    outputOptions(output, "has_data", suspendWhenHidden = FALSE)
  })
}