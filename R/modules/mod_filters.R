# =============================================================================
# mod_filters.R — Data Selection Module (UI + Server)
#
# Purpose:
#   - Provide dynamic UI inputs for selecting Producer / Year / Field
#   - Use renderUI to populate dropdowns based on uploaded data
#   - Handle step validation for stepper (Step 5)
#
# Inputs:
#   id    : Module namespace ID
#   state : reactiveValues, shared app state containing uploaded data
#
# Outputs:
#   - Dynamic UI inputs for selection (producer, year, field) using renderUI
#   - Report options (include_comparisons, include_maps)
#   - Updates state$step_5_valid when producer and year are selected
#
# Implementation Notes:
#   - Uses renderUI instead of updateSelectInput to avoid UI re-rendering issues
#   - Dropdowns are populated directly from state$data when UI is rendered
#   - Year dropdown depends on producer selection, field dropdown depends on both
# =============================================================================

mod_filters_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Report Filters"),
    
    # Producer selection - use renderUI to get choices from state
    uiOutput(ns("producer_ui")),
    
    # Year selection - use renderUI to get choices from state
    uiOutput(ns("year_ui")),
    
    
    # Report options
    tags$hr(),
    h5("Report Options"),
    checkboxInput(
      ns("include_comparisons"),
      "Include regional comparisons",
      value = TRUE
    ),
    checkboxInput(
      ns("include_maps"),
      "Include field maps",
      value = TRUE
    ),
    
  )
}

# ---------------------------
# Filters Module Server
# ---------------------------
mod_filters_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Render producer dropdown with choices from state
    output$producer_ui <- renderUI({
      # Use filtered data if available, otherwise use original data
      data_to_use <- if (!is.null(state$filtered_data)) state$filtered_data else state$data
      
      if (!is.null(data_to_use) && "producer_id" %in% names(data_to_use)) {
        producers <- sort(unique(data_to_use$producer_id))
        selectInput(
          ns("producer"),
          "Select Producer:",
          choices = producers,
          selected = NULL
        )
      } else {
        selectInput(
          ns("producer"),
          "Select Producer:",
          choices = c("Please upload data first" = ""),
          selected = NULL
        )
      }
    })
    
    # Render year dropdown with choices based on selected producer
    output$year_ui <- renderUI({
      # Force reactivity to producer selection - use the actual input ID
      producer_selected <- input$producer
      
      # Use filtered data if available, otherwise use original data
      data_to_use <- if (!is.null(state$filtered_data)) state$filtered_data else state$data
      
      if (!is.null(data_to_use) && !is.null(producer_selected) && producer_selected != "") {
        years <- sort(unique(data_to_use$year[data_to_use$producer_id == producer_selected]))
        selectInput(
          ns("year"),
          "Select Year:",
          choices = years,
          selected = NULL
        )
      } else {
        selectInput(
          ns("year"),
          "Select Year:",
          choices = c("Please select a producer first" = ""),
          selected = NULL
        )
      }
    })
    
    
    # Validate step 5 when producer and year are selected
    observe({
      req(input$producer, input$year)
      
      # Update state with selected values
      state$selected_producer <- input$producer
      state$selected_year <- input$year
      
      # Mark step 5 as valid if producer and year are selected
      if (input$producer != "" && input$year != "" && input$year != "NULL") {
        state$step_5_valid <- TRUE
      } else {
        state$step_5_valid <- FALSE
      }
    })
    
  })
}