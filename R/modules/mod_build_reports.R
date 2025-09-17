# =============================================================================
# mod_build_reports.R — Main Build Reports Module with Stepper
# =============================================================================
# This module manages the 8-step workflow for building soil health reports
# using a stepper interface similar to dirt-data-reports

mod_build_reports_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "container-reports",
      tags$div(
        style = "display:flex",
        # Vertical Side Stepper (clickable vertical steps)
        tags$div(
          class = "stepper",
          uiOutput(ns("stepper_ui"))
        ),
        # Main Content Panel for Form
        tags$div(
          class = "form-section",
          # Mobile Progress Bar (hidden on desktop)
          tags$div(
            class = "progress-bar-container",
            tags$div(
              class = "progress-bar-header",
              tags$span(class = "progress-title", "Step Progress"),
              tags$span(id = "progress-step-text", class = "progress-step-text", "1/8")
            ),
            tags$div(
              class = "progress-bar-wrapper",
              tags$div(id = "progress-bar", class = "progress-bar", style = "width: 12.5%;")
            )
          ),
          # Form Content
          tags$div(id = ns("dynamic_content"), uiOutput(ns("step_ui"))),
          # Form Buttons
          tags$div(class = "buttons", uiOutput(ns("step_nav_buttons")))
        )
      )
    )
  )
}

mod_build_reports_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # State management for the stepper
    state <- reactiveValues(
      current_step = 1,
      step_1_valid = TRUE,  # Download template - always valid
      step_2_valid = FALSE, # Data upload
      step_3_valid = FALSE, # Data filter
      step_4_valid = FALSE, # Project info
      step_5_valid = FALSE, # Select data (filters)
      step_6_valid = FALSE, # Grouping
      step_7_valid = FALSE, # Indicator selection
      step_8_valid = FALSE, # Report generation
      data = NULL,
      data_dictionary = NULL,
      data_uploaded = FALSE,
      selected_producer = NULL,
      selected_year = NULL,
      selected_grouping_var = NULL
    )
    
    # Data pipeline that transforms data through all steps
    data_pipeline <- reactive({
      req(state$data_uploaded, state$data)
      
      # Start with original data
      current_data <- state$data
      
      # Step 1: Apply filters based on filter configuration
      filter_config <- read.csv("config/filter-config.csv", stringsAsFactors = FALSE)
      
      for (i in 1:nrow(filter_config)) {
        col_name <- filter_config$column_name[i]
        filter_input_id <- ns(paste0("step3-", col_name, "_filter"))
        
        if (col_name %in% names(current_data) && !is.null(input[[filter_input_id]])) {
          if (input[[filter_input_id]] != "all") {
            current_data <- current_data[current_data[[col_name]] == input[[filter_input_id]], , drop = FALSE]
          }
        }
      }
      
      # Step 2: Apply producer/year/field selection (if selected)
      # Note: Input IDs are namespaced as step5-producer, step5-year, step5-field
      producer_input <- input[[ns("step5-producer")]]
      year_input <- input[[ns("step5-year")]]
      field_input <- input[[ns("step5-field")]]
      
      if (!is.null(producer_input) && producer_input != "" && 
          !is.null(year_input) && year_input != "" &&
          year_input != "NULL") {  # Additional check for "NULL" string
        current_data <- current_data[
          current_data$producer_id == producer_input & 
          current_data$year == year_input, , drop = FALSE
        ]
        
        # Apply field filter if specified
        if (!is.null(field_input) && field_input != "all" && "field_id" %in% names(current_data)) {
          current_data <- current_data[current_data$field_id == field_input, , drop = FALSE]
        }
      }
      
      return(current_data)
    })
    
    # Stepper UI with all 8 steps
    output$stepper_ui <- renderUI({
      tagList(
        # Step 1: Download Template
        tags$div(
          id = ns("step_1"),
          class = paste("step", if (state$current_step == 1) "active"),
          `data-step` = 1,
          title = "Download the Excel template to get started",
          onclick = paste0("setStep(this, '", ns("step_click"), "')"),
          tags$div(class = "step-circle", icon("download")),
          tags$div(
            div(class = "step-num", "Step 1"), 
            div(class = "step-text", "Download Template")
          )
        ),
        
        # Step 2: Upload Data
        tags$div(
          id = ns("step_2"),
          class = paste("step", if (state$current_step == 2) "active"),
          `data-step` = 2,
          title = "Upload your completed data template",
          onclick = paste0("setStep(this, '", ns("step_click"), "')"),
          tags$div(class = "step-circle", icon("upload")),
          tags$div(
            div(class = "step-num", "Step 2"), 
            div(class = "step-text", "Upload Data")
          )
        ),
        
        # Step 3: Filter Data
        tags$div(
          id = ns("step_3"),
          class = paste(
            "step",
            if (!state$step_2_valid) "disabled",
            if (state$current_step == 3) "active"
          ),
          `data-step` = 3,
          title = if (!state$step_2_valid) "Please upload valid data in Step 2 first" else "Filter your data by site type, crop, and texture",
          onclick = if (state$step_2_valid) paste0("setStep(this, '", ns("step_click"), "')") else NULL,
          tags$div(class = "step-circle", icon("filter")),
          tags$div(
            div(class = "step-num", "Step 3"), 
            div(class = "step-text", "Filter Data")
          )
        ),
        
        # Step 4: Project Information
        tags$div(
          id = ns("step_4"),
          class = paste(
            "step",
            if (!state$step_2_valid) "disabled",
            if (state$current_step == 4) "active"
          ),
          `data-step` = 4,
          title = if (!state$step_2_valid) "Please upload valid data in Step 2 first" else "Customize your project information",
          onclick = if (state$step_2_valid) paste0("setStep(this, '", ns("step_click"), "')") else NULL,
          tags$div(class = "step-circle", icon("info-circle")),
          tags$div(
            div(class = "step-num", "Step 4"), 
            div(class = "step-text", "Project Information")
          )
        ),
        
        # Step 5: Select Data
        tags$div(
          id = ns("step_5"),
          class = paste(
            "step",
            if (!state$step_2_valid) "disabled",
            if (state$current_step == 5) "active"
          ),
          `data-step` = 5,
          title = if (!state$step_2_valid) "Please upload valid data in Step 2 first" else "Choose producer, year, and field for your report",
          onclick = if (state$step_2_valid) paste0("setStep(this, '", ns("step_click"), "')") else NULL,
          tags$div(class = "step-circle", icon("database")),
          tags$div(
            div(class = "step-num", "Step 5"), 
            div(class = "step-text", "Select Data")
          )
        ),
        
        # Step 6: Choose Grouping Variable
        tags$div(
          id = ns("step_6"),
          class = paste(
            "step",
            if (!state$step_5_valid) "disabled",
            if (state$current_step == 6) "active"
          ),
          `data-step` = 6,
          title = if (!state$step_5_valid) "Please select data in Step 5 first" else "Choose grouping variable for averaging",
          onclick = if (state$step_5_valid) paste0("setStep(this, '", ns("step_click"), "')") else NULL,
          tags$div(class = "step-circle", icon("layer-group")),
          tags$div(
            div(class = "step-num", "Step 6"), 
            div(class = "step-text", "Choose Grouping")
          )
        ),
        
        # Step 7: Select Indicators
        tags$div(
          id = ns("step_7"),
          class = paste(
            "step",
            if (!state$step_5_valid) "disabled",
            if (state$current_step == 7) "active"
          ),
          `data-step` = 7,
          title = if (!state$step_5_valid) "Please select data in Step 5 first" else "Select which indicators to include in your report",
          onclick = if (state$step_5_valid) paste0("setStep(this, '", ns("step_click"), "')") else NULL,
          tags$div(class = "step-circle", icon("list-check")),
          tags$div(
            div(class = "step-num", "Step 7"), 
            div(class = "step-text", "Select Indicators")
          )
        ),
        
        # Step 8: Generate Reports
        tags$div(
          id = ns("step_8"),
          class = paste(
            "step",
            if (!state$step_5_valid) "disabled",
            if (state$current_step == 8) "active"
          ),
          `data-step` = 8,
          title = if (!state$step_5_valid) "Please select data in Step 5 first" else "Generate and download your soil health report",
          onclick = if (state$step_5_valid) paste0("setStep(this, '", ns("step_click"), "')") else NULL,
          tags$div(class = "step-circle", icon("file-alt")),
          tags$div(
            div(class = "step-num", "Step 8"), 
            div(class = "step-text", "Generate Reports")
          )
        )
      )
    })
    
    # Step content loader based on current step
    output$step_ui <- renderUI({
      switch(
        state$current_step,
        mod_download_ui(ns("step1")),           # Step 1: Download Template
        mod_data_upload_ui(ns("step2")),        # Step 2: Upload Data
        mod_data_filter_ui(ns("step3")),        # Step 3: Filter Data
        mod_project_info_ui(ns("step4")),       # Step 4: Project Information
        mod_filters_ui(ns("step5")),            # Step 5: Select Data
        mod_grouping_ui(ns("step6")),           # Step 6: Choose Grouping Variable
        mod_indicator_selection_ui(ns("step7")), # Step 7: Select Indicators
        mod_report_ui(ns("step8"))              # Step 8: Generate Reports
      )
    })
    
    # Navigation buttons at the bottom
    output$step_nav_buttons <- renderUI({
      # Previous button
      prev_btn <- if (state$current_step > 1) {
        actionButton(
          ns("prev_step"), 
          "Previous", 
          class = "btn btn-secondary",
          style = "min-width: 100px;"
        )
      } else {
        div() # Empty div to maintain layout
      }
      
      # Next button logic
      next_disabled <- FALSE
      next_text <- "Next"
      
      if (state$current_step == 8) {
        next_text <- "Finish"
        next_disabled <- !state$step_5_valid # Need at least data selection
      } else if (state$current_step == 2 && !state$step_2_valid) {
        next_disabled <- TRUE
      } else if (state$current_step == 5 && !state$step_5_valid) {
        next_disabled <- TRUE
      }
      
      next_btn <- actionButton(
        ns("next_step"), 
        next_text,
        class = "btn btn-primary",
        disabled = next_disabled,
        style = "min-width: 100px;"
      )
      
      # Only show navigation buttons if not on the final step
      if (state$current_step < 8) {
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          div(prev_btn),
          div(next_btn)
        )
      } else {
        # On final step, only show Previous button
        div(
          style = "display: flex; justify-content: flex-start; align-items: center;",
          div(prev_btn)
        )
      }
    })
    
    # Step navigation logic
    observeEvent(input$next_step, {
      if (state$current_step < 8) {
        state$current_step <- state$current_step + 1
      }
    })
    
    observeEvent(input$prev_step, {
      if (state$current_step > 1) {
        state$current_step <- state$current_step - 1
      }
    })
    
    # Handle step clicks from stepper
    observeEvent(input$step_click, {
      if (!is.null(input$step_click)) {
        state$current_step <- input$step_click
      }
    })
    
    # Call individual step modules - load all at startup to ensure they're available
    mod_download_server("step1", cfg = get_cfg(), state = state)
    mod_data_upload_server("step2", state = state)
    mod_data_filter_server("step3", state = state)
    mod_project_info_server("step4", state = state)
    mod_filters_server("step5", state = state)  # Load filters module early
    mod_grouping_server("step6", state = state)
    mod_indicator_selection_server("step7", state = state)
    mod_report_server("step8", cfg = get_cfg(), state = state, data_pipeline = data_pipeline)
    
    # Return state for other modules to access
    return(state)
  })
}
