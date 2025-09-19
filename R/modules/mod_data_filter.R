# =============================================================================
# mod_data_filter.R — Data Filtering Module (UI + Server)
#
# Purpose:
#   - Provide UI inputs for filtering data based on filter-config.csv
#   - The actual filtering is handled by the data pipeline in app.R
#
# Inputs:
#   id    : Module namespace ID
#   state : reactiveValues containing the uploaded data
#
# Outputs:
#   - Dynamic UI inputs for filtering based on configuration
# =============================================================================

mod_data_filter_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Filter Data"),
    
    # Dynamic filter inputs
    uiOutput(ns("filter_inputs")),
    
    helpText("Select filters to narrow down your data before selecting producers and years.")
  )
}

mod_data_filter_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Render dynamic filter inputs based on configuration
    output$filter_inputs <- renderUI({
      req(state$data_uploaded, state$data)
      df <- state$data
      
      # Read filter configuration
      filter_config <- read.csv("config/filter-config.csv", stringsAsFactors = FALSE)
      
      # Generate filter UI elements
      filter_ui_elements <- list()
      
      for (i in 1:nrow(filter_config)) {
        col_name <- filter_config$column_name[i]
        filter_label <- filter_config$filter_label[i]
        
        if (col_name %in% names(df)) {
          # Get unique values for this column
          unique_values <- sort(unique(df[[col_name]]))
          
                     # Create selectInput for this filter
           choices_list <- c("all")
           # Create a proper "All" label
           all_label <- paste("All", tolower(filter_label), "s")
           # Fix common pluralization issues
           all_label <- gsub("site type s", "site types", all_label)
           all_label <- gsub("crop s", "crops", all_label)
           all_label <- gsub("texture s", "textures", all_label)
           names(choices_list) <- all_label
           
           for (val in unique_values) {
             choices_list <- c(choices_list, val)
             names(choices_list)[length(choices_list)] <- val
           }
           
           filter_ui_elements[[col_name]] <- selectInput(
             inputId = ns(paste0(col_name, "_filter")),
             label = paste("Filter by", filter_label, ":"),
             choices = choices_list,
             selected = "all"
           )
        }
      }
      
      return(tagList(filter_ui_elements))
    })
    
    # Apply filters to data when any filter changes
    observe({
      req(state$data_uploaded, state$data)
      
      # Read filter configuration
      filter_config <- read.csv("config/filter-config.csv", stringsAsFactors = FALSE)
      
      # Start with original data
      filtered_data <- state$data
      
      # Apply each filter
      for (i in 1:nrow(filter_config)) {
        col_name <- filter_config$column_name[i]
        
        if (col_name %in% names(filtered_data)) {
          filter_input_id <- paste0(col_name, "_filter")
          
          # Check if the filter input exists and has a value
          if (!is.null(input[[filter_input_id]]) && input[[filter_input_id]] != "all") {
            filtered_data <- filtered_data[filtered_data[[col_name]] == input[[filter_input_id]], ]
          }
        }
      }
      
      # Update the filtered data in state
      state$filtered_data <- filtered_data
      # Also update state$data so dropdowns work correctly
      state$data <- filtered_data
      
      # Mark step 3 as valid if we have data
      state$step_3_valid <- nrow(filtered_data) > 0
    })
  })
} 