# =============================================================================
# app.R — Main Shiny Application Entry Point
#
# Purpose:
#   - Define the top-level UI and server for the Soil Health Reports application.
#   - Follows template-based workflow: Download Template → Upload Data → Generate Reports
#   - Simplified workflow without column mapping complexity
#
# Flow:
#   1) Upload module (`mod_upload`)
#        - Download Excel template or upload completed template
#        - Validate uploaded data against template structure
#        - Returns validated data and data dictionary
#   2) Data Filter module (`mod_data_filter`)
#        - Consume validated data; user selects producer/year/field.
#        - Returns list(producer, year, field, options, trigger).
#   3) Filters module (`mod_filters`)
#        - Consume validated data; user selects producer/year/field.
#        - Returns list(producer, year, field, options, trigger).
#   4) Report module (`mod_report`)
#        - Consumes validated data + selections and renders a Quarto report.
#        - Shows status/progress, preview, and download buttons.
#
# Notes:
#   - Configuration is loaded in global.R and exposed via get_cfg().
#   - The report module operates on validated template data (not raw).
#   - Quarto rendering parameters (template, styles, output_dir) are defined in
#     the YAML config and validated by resolve_paths().
# =============================================================================

# --- Load global configuration first -------------------------------------------
source("global.R")

# --- Lightweight extras used across modules (quiet startup) -------------------
suppressPackageStartupMessages({
  library(DT)       # data preview widgets
  library(memoise)  # caching (used inside mod_report)
  library(digest)   # hashing (used inside mod_report)
})

# --- App configuration (read-only), provided by global.R ---------------------
cfg <- get_cfg()

# --- Module sources (use portable paths) -------------------------------------
source(file.path("R", "modules", "mod_upload.R"))
source(file.path("R", "modules", "mod_data_filter.R"))
source(file.path("R", "modules", "mod_filters.R"))
source(file.path("R", "modules", "mod_grouping.R"))
source(file.path("R", "modules", "mod_report.R"))

# =============================================================================
# UI
# =============================================================================
ui <- fluidPage(
  titlePanel("Soil Health Reports"),

  # Minor style tweaks to keep things tidy + optional branded stylesheet
  tags$head(
    tags$style(HTML("
      .btn-group .btn { margin-right: 5px; }
      .progress       { margin: 10px 0;   }
      .alert          { margin: 10px 0;   }
      .step-section   { margin: 20px 0; padding: 15px; border: 1px solid #ddd; border-radius: 5px; }
      .step-title     { font-weight: bold; color: #333; margin-bottom: 10px; }
    ")),
    if (!is.null(cfg$paths$styles) && file.exists(cfg$paths$styles)) {
      # Allow project-branded CSS defined in config.yml (paths.styles)
      tags$link(rel = "stylesheet", type = "text/css", href = cfg$paths$styles)
    }
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      # 1) Template download and data upload
      div(class = "step-section",
        div(class = "step-title", "Step 1: Template & Data"),
        mod_upload_ui("upload")
      ),

      # 2) Filter data by site_type, crop, texture
      div(class = "step-section",
        div(class = "step-title", "Step 2: Filter Data"),
        mod_data_filter_ui("data_filter")
      ),

      # 3) Choose Producer / Year / Field
      div(class = "step-section",
        div(class = "step-title", "Step 3: Select Data"),
        mod_filters_ui("filters")
      ),

      # 4) Choose grouping variable for averaging
      conditionalPanel(
        condition = "output.data_ready",
        div(class = "step-section",
          div(class = "step-title", "Step 4: Choose Grouping Variable"),
          mod_grouping_ui("grouping")
        )
      )
    ),
    mainPanel(
      width = 8,

      # 5) Show progress, preview the HTML, and expose downloads
      div(class = "step-section",
        div(class = "step-title", "Step 5: Generate Reports"),
        mod_report_ui("report")
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

  # Shared state for the application
  state <- reactiveValues(
    data = NULL,
    data_dictionary = NULL,
    data_uploaded = FALSE,

    selected_grouping_var = NULL
  )

  # Single data pipeline that transforms data through all steps
  data_pipeline <- reactive({
    req(state$data_uploaded, state$data)
    
    # Start with original data
    current_data <- state$data
    
    # Step 1: Apply filters based on filter configuration
    filter_config <- read.csv("config/filter-config.csv", stringsAsFactors = FALSE)
    
    for (i in 1:nrow(filter_config)) {
      col_name <- filter_config$column_name[i]
      filter_input_id <- paste0("data_filter-", col_name, "_filter")
      
      if (col_name %in% names(current_data) && !is.null(input[[filter_input_id]])) {
        if (input[[filter_input_id]] != "all") {
          current_data <- current_data[current_data[[col_name]] == input[[filter_input_id]], , drop = FALSE]
        }
      }
    }
    
    # Step 2: Apply producer/year/field selection (if selected)
    if (!is.null(input$`filters-producer`) && input$`filters-producer` != "" && 
        !is.null(input$`filters-year`) && input$`filters-year` != "" &&
        input$`filters-year` != "NULL") {  # Additional check for "NULL" string
      current_data <- current_data[
        current_data$producer_id == input$`filters-producer` & 
        current_data$year == input$`filters-year`, , drop = FALSE
      ]
      
      # Apply field filter if specified
      if (!is.null(input$`filters-field`) && input$`filters-field` != "all" && "field_id" %in% names(current_data)) {
        current_data <- current_data[current_data$field_id == input$`filters-field`, , drop = FALSE]
      }
    }
    
    return(current_data)
  })

  # 1) Upload module: handles template download and data upload, updates state
      mod_upload_server("upload", cfg = cfg, state = state)

  # 2) Data Filter module: provides dynamic filter inputs based on configuration
  mod_data_filter_server("data_filter", state = state)
  
  # 3) Filters module: provides selection inputs (no server logic needed)
  
  # 4) Grouping module: allows selection of grouping variable for averaging
  grouping_result <- mod_grouping_server("grouping", state = state)
  
  # 5) Report module: uses data_pipeline() for report generation
  mod_report_server(
    id     = "report",
    cfg    = cfg,
    state  = state,
    data_pipeline = data_pipeline
  )

  # Update state with selected grouping variable
  observe({
    req(grouping_result())
    state$selected_grouping_var <- grouping_result()
  })

  # Output for conditional visibility
  output$data_ready <- reactive({
    state$data_uploaded && !is.null(state$data)
  })
  outputOptions(output, "data_ready", suspendWhenHidden = FALSE)





  # Populate producer choices based on filtered data (only once when data is loaded)
  observe({
    req(state$data_uploaded, state$data)
    df <- state$data  # Use original data, not filtered data
    cat("Producer population - initial data rows:", nrow(df), "\n")
    if (nrow(df) > 0 && "producer_id" %in% names(df)) {
      producers <- sort(unique(df$producer_id))
      cat("Producer population - unique producers:", producers, "\n")
      cat("Producer population - setting initial choices\n")
      updateSelectInput(
        session, "filters-producer",
        choices = c("Select producer..." = "", producers),
        selected = ""
      )
    }
  })

  # Populate year choices based on producer selection
  observe({
    req(input$`filters-producer`, input$`filters-producer` != "")
    df <- state$data  # Use original data, not filtered data
    cat("Year population - producer:", input$`filters-producer`, "\n")
    if (nrow(df) > 0 && all(c("producer_id", "year") %in% names(df))) {
      yrs <- df$year[df$producer_id == input$`filters-producer`]
      if (is.character(yrs)) {
        yrs <- unique(trimws(yrs))
        suppressWarnings({
          yrs_num <- as.integer(yrs)
        })
        if (!anyNA(yrs_num)) yrs <- yrs_num
      }
      yrs <- sort(unique(yrs), na.last = TRUE)
      cat("Year population - available years:", yrs, "\n")
      cat("Year population - updating choices\n")
      updateSelectInput(
        session, "filters-year",
        choices = c("Select year..." = "", yrs),
        selected = ""
      )
      cat("Year population - set to empty selection\n")
    }
  })

  # Populate field choices based on producer/year selection
  observe({
    req(data_pipeline(), input$`filters-producer`, input$`filters-year`)
    df <- data_pipeline()
    if (nrow(df) > 0 && all(c("producer_id", "year") %in% names(df))) {
      sub <- df[df$producer_id == input$`filters-producer` & df$year == input$`filters-year`, , drop = FALSE]
      
      if (!"field_id" %in% names(sub)) {
        updateSelectInput(session, "filters-field", choices = c("All fields" = "all"), selected = "all")
        return()
      }
      
      fields <- sort(unique(sub$field_id))
      field_choices <- c("All fields" = "all", setNames(fields, fields))
      updateSelectInput(session, "filters-field", choices = field_choices, selected = "all")
    }
  })

  # Update shared state when selections change
  observeEvent(input$`filters-producer`, {
    state$selected_producer <- input$`filters-producer`
  })
  
  observeEvent(input$`filters-year`, {
    state$selected_year <- input$`filters-year`
  })
  
  observeEvent(input$`filters-field`, {
    state$selected_field <- input$`filters-field`
  })


}

# Launch application
shinyApp(ui, server)