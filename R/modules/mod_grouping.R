# =============================================================================
# mod_grouping.R — Grouping Variable Selection Module
#
# Purpose:
#   - Allow users to select which variable to group by for averaging and comparisons
#   - Options are configured in config/grouping_config.csv
#   - This selection affects how the report calculates averages and comparisons
#
# Inputs:
#   - state: reactiveValues containing uploaded data
#
# Outputs:
#   - selected_grouping_var: the column name selected for grouping
# =============================================================================

# -----------------------------
# UI
# -----------------------------
mod_grouping_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class = "form-group",
      # Use renderUI instead of static selectInput
      uiOutput(ns("grouping_var_ui")),
      helpText("Choose how to organize your data for comparisons:"),
      div(style = "margin: 10px 0; padding: 10px; background-color: #f8f9fa; border-left: 4px solid #9FC9E4; border-radius: 3px;",
        tags$strong("With grouping:"), " Compare individual fields/treatments within your farm and to other anonymized producers' fields/treatments if available",
        br(),
        tags$strong("Without grouping:"), " Compare your farm average to other anonymized producers' averages, and the project average",
        br(),
        br(),
        tags$em("Note:"), " Tables show your farm average vs. project average. Plots show individual dots for each producer × year combination, including your own samples from other years."
      ),
      
      # Show available values for the selected grouping variable
      conditionalPanel(
        condition = "input.grouping_var != ''",
        ns = ns,
        div(style = "margin-top: 10px;",
          strong("Available values:"),
          verbatimTextOutput(ns("grouping_values")),
          br(),
          div(style = "margin-top: 10px; padding: 8px; background-color: #f8f9fa; border-left: 4px solid #9FC9E4; border-radius: 3px;",
            strong("Note:"), " The system supports up to 8 different groups with unique symbols and colors. If you have more than 8 groups, the symbols will cycle through the available options."
          )
        )
      )
    )
  )
}

# -----------------------------
# Server
# -----------------------------
mod_grouping_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load grouping configuration
    grouping_config <- reactive({
      config_path <- "config/grouping_config.csv"
      if (file.exists(config_path)) {
        read.csv(config_path, stringsAsFactors = FALSE)
      } else {
        # Fallback to default configuration
        data.frame(
          column_name = c("field_id", "treatment_id"),
          grouping_label = c("Field ID", "Treatment ID"),
          grouping_type = c("dropdown", "dropdown"),
          description = c("Group by different fields on the farm", "Group by different treatments or management practices"),
          stringsAsFactors = FALSE
        )
      }
    })
    
    # Render grouping variable dropdown with choices from state
    output$grouping_var_ui <- renderUI({
      if (!is.null(state$data)) {
        # Get configured grouping options
        config <- grouping_config()
        available_columns <- names(state$data)
        
        # Filter to only include columns that exist in the data
        valid_options <- config[config$column_name %in% available_columns, ]
        
        # Create choices with labels
        if (nrow(valid_options) > 0) {
          choices <- setNames(valid_options$column_name, valid_options$grouping_label)
          selectInput(
            ns("grouping_var"),
            "Select grouping variable for averaging and comparisons:",
            choices = c("No grouping (farm-level comparison)" = "no_grouping", "Select grouping variable..." = "", choices),
            selected = ""
          )
        } else {
          selectInput(
            ns("grouping_var"),
            "Select grouping variable for averaging and comparisons:",
            choices = c("No grouping options available" = ""),
            selected = ""
          )
        }
      } else {
        selectInput(
          ns("grouping_var"),
          "Select grouping variable for averaging and comparisons:",
          choices = c("Please upload data first" = ""),
          selected = ""
        )
      }
    })
    
    # Show available values for selected grouping variable
    output$grouping_values <- renderText({
      req(input$grouping_var, input$grouping_var != "", input$grouping_var != "no_grouping", state$data)
      
      values <- unique(state$data[[input$grouping_var]])
      values <- values[!is.na(values)]
      paste(values, collapse = ", ")
    })
    
    # Update state when grouping variable is selected
    observe({
      req(input$grouping_var)
      
      # Handle "no_grouping" option
      if (input$grouping_var == "no_grouping") {
        state$selected_grouping_var <- NULL  # Set to NULL for no grouping
      } else {
        state$selected_grouping_var <- input$grouping_var
      }
      
      # Mark step 6 as valid if a grouping variable is selected or "no grouping" is chosen
      if (input$grouping_var != "" && input$grouping_var != "Select grouping variable...") {
        state$step_6_valid <- TRUE
      } else {
        state$step_6_valid <- FALSE
      }
    })
    
    # Return the selected grouping variable
    reactive({
      input$grouping_var
    })
  })
} 