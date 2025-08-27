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
      selectInput(
        ns("grouping_var"),
        label = "Select grouping variable for averaging and comparisons:",
        choices = c("Select grouping variable..." = ""),
        selected = ""
      ),
      helpText("Choose which variable to group by for averaging and comparisons. Options are configured in the grouping configuration."),
      
      # Show available values for the selected grouping variable
      conditionalPanel(
        condition = "input.grouping_var != ''",
        ns = ns,
        div(style = "margin-top: 10px;",
          strong("Available values:"),
          verbatimTextOutput(ns("grouping_values")),
          br(),
          div(style = "margin-top: 10px; padding: 8px; background-color: #f8f9fa; border-left: 4px solid #007bff; border-radius: 3px;",
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
    
    # Update grouping variable choices based on available data and configuration
    observe({
      req(state$data)
      
      # Get configured grouping options
      config <- grouping_config()
      available_columns <- names(state$data)
      
      # Filter to only include columns that exist in the data
      valid_options <- config[config$column_name %in% available_columns, ]
      
      # Create choices with labels
      if (nrow(valid_options) > 0) {
        choices <- setNames(valid_options$column_name, valid_options$grouping_label)
      } else {
        choices <- c()
      }
      
      # Update choices
      updateSelectInput(
        session,
        "grouping_var",
        choices = c("Select grouping variable..." = "", choices),
        selected = ""
      )
    })
    
    # Show available values for selected grouping variable
    output$grouping_values <- renderText({
      req(input$grouping_var, input$grouping_var != "", state$data)
      
      values <- unique(state$data[[input$grouping_var]])
      values <- values[!is.na(values)]
      paste(values, collapse = ", ")
    })
    
    # Return the selected grouping variable
    reactive({
      input$grouping_var
    })
  })
} 