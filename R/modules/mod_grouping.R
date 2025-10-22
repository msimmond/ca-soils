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
      div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
        tags$label(
          class = "control-label",
          `for` = ns("grouping_var"),
          "Select grouping variable for averaging and comparisons:"
        ),
        uiOutput(ns("grouping_var_ui")),
        conditionalPanel(
          condition = "output.show_required_label",
          ns = ns,
          span(style = "color: #dc3545; font-weight: bold; white-space: nowrap;", "Required")
        )
      ),
      helpText("Choose how to organize your data for comparisons:"),
      div(style = "margin: 10px 0; padding: 10px; background-color: #f8f9fa; border-left: 4px solid #9FC9E4; border-radius: 3px;",
        tags$strong("With grouping:"), " Compare individual fields/treatments within your farm and to other anonymized producers' fields/treatments if available",
        br(),
        tags$strong("Without grouping:"), " Compare your farm average to other anonymized producers' averages, and the project average",
        br(),
        br(),
        tags$em("Note:"), " Tables show your farm average vs. project average. Plots show individual dots for each producer × year combination, including your own samples from other years."
      ),
      
      # Show diagnostic information about grouping variables
      conditionalPanel(
        condition = "output.show_grouping_diagnostics",
        ns = ns,
        div(style = "margin-top: 15px; padding: 10px; background-color: #f8f9fa; border-left: 4px solid #6c757d; border-radius: 3px;",
          strong("Status of available columns to group by:"),
          verbatimTextOutput(ns("grouping_diagnostics"))
        )
      ),
      
      # Show available values for the selected grouping variable (only when a grouping variable is selected, not "no_grouping")
      conditionalPanel(
        condition = "input.grouping_var != '' && input.grouping_var != 'no_grouping'",
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
      # Use filtered data if available, otherwise use original data
      data_to_use <- if (!is.null(state$filtered_data)) state$filtered_data else state$data
      
      if (!is.null(data_to_use)) {
        # Get configured grouping options
        config <- grouping_config()
        available_columns <- names(data_to_use)
        
        # Filter to only include columns that exist in the data AND have sufficient non-NA values
        valid_options <- config[config$column_name %in% available_columns, ]
        
        # Additional filter: only include columns where ALL records have non-NA values
        if (nrow(valid_options) > 0) {
          valid_options <- valid_options[sapply(valid_options$column_name, function(col) {
            non_na_count <- sum(!is.na(data_to_use[[col]]))
            total_count <- nrow(data_to_use)
            
            # All records must have values (no missing data allowed)
            non_na_count == total_count
          }), ]
        }
        
        # Create choices with labels
        if (nrow(valid_options) > 0) {
          choices <- setNames(valid_options$column_name, valid_options$grouping_label)
          selectInput(
            ns("grouping_var"),
            label = NULL,
            choices = c("No grouping (farm-level comparison)" = "no_grouping", "Select grouping variable..." = "", choices),
            selected = ""
          )
        } else {
          div(
            selectInput(
              ns("grouping_var"),
              label = NULL,
              choices = c("No grouping options available" = ""),
              selected = ""
            ),
            div(style = "margin-top: 10px; padding: 8px; background-color: #fff3cd; border-left: 4px solid #ffc107; border-radius: 3px;",
              tags$strong("Note:"), " No grouping variables are available because either:",
              tags$ul(
                tags$li("The required columns (field_id, treatment_id) are missing from your data, or"),
                tags$li("The available columns have missing values (all records must have values for grouping)")
              ),
              "You can still generate a report using 'No grouping' for farm-level comparisons."
            )
          )
        }
      } else {
        selectInput(
          ns("grouping_var"),
          label = NULL,
          choices = c("Please upload data first" = ""),
          selected = ""
        )
      }
    })
    
    # Show diagnostic information about grouping variables
    output$show_grouping_diagnostics <- reactive({
      !is.null(state$data) && state$data_uploaded
    })
    outputOptions(output, "show_grouping_diagnostics", suspendWhenHidden = FALSE)
    
    # Show "Required" label when no selection is made
    output$show_required_label <- reactive({
      !is.null(state$data) && state$data_uploaded && 
      (is.null(input$grouping_var) || input$grouping_var == "" || input$grouping_var == "Select grouping variable...")
    })
    outputOptions(output, "show_required_label", suspendWhenHidden = FALSE)
    
    output$grouping_diagnostics <- renderText({
      # Use filtered data if available, otherwise use original data
      data_to_use <- if (!is.null(state$filtered_data)) state$filtered_data else state$data
      
      if (is.null(data_to_use)) return("No data available")
      
      config <- grouping_config()
      available_columns <- names(data_to_use)
      
      # Check each potential grouping variable
      diagnostics <- c()
      for (i in 1:nrow(config)) {
        col_name <- config$column_name[i]
        label <- config$grouping_label[i]
        
        if (col_name %in% available_columns) {
          non_na_count <- sum(!is.na(data_to_use[[col_name]]))
          total_count <- nrow(data_to_use)
          unique_values <- unique(data_to_use[[col_name]][!is.na(data_to_use[[col_name]])])
          
          # Limit display to first 5 values to avoid overwhelming output
          if (length(unique_values) <= 5) {
            unique_values_text <- paste(unique_values, collapse = ", ")
          } else {
            unique_values_text <- paste0(paste(unique_values[1:5], collapse = ", "), " (and ", length(unique_values) - 5, " more)")
          }
          
          if (non_na_count == total_count) {
            status <- paste0("✅ ", label, ": Available (", length(unique_values), " unique values: ", unique_values_text, ")")
          } else if (non_na_count < total_count) {
            missing_count <- total_count - non_na_count
            status <- paste0("❌ ", label, ": Missing values (", missing_count, " out of ", total_count, " records) - ", length(unique_values), " unique values present: ", unique_values_text)
          } else {
            status <- paste0("❌ ", label, ": Unknown issue")
          }
        } else {
          status <- paste0("❌ ", label, ": Column not found in data")
        }
        
        diagnostics <- c(diagnostics, status)
      }
      
      paste(diagnostics, collapse = "\n")
    })
    
    # Show available values for selected grouping variable
    output$grouping_values <- renderText({
      # Use filtered data if available, otherwise use original data
      data_to_use <- if (!is.null(state$filtered_data)) state$filtered_data else state$data
      
      req(input$grouping_var, input$grouping_var != "", input$grouping_var != "no_grouping", data_to_use)
      
      values <- unique(data_to_use[[input$grouping_var]])
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