# =============================================================================
# mod_filters.R — Filter + Generate Module (UI only)
#
# Purpose:
#   - Provide UI inputs for selecting Producer / Year / Field
#   - Provide report options and generate button
#   - The actual selection logic is handled by the data pipeline in app.R
#
# Inputs:
#   id    : Module namespace ID
#
# Outputs:
#   - UI inputs for selection (producer, year, field)
#   - Report options (include_comparisons, include_maps)
#   - Generate button (generate)
# =============================================================================

mod_filters_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Report Filters"),
    
    # Producer selection
    selectInput(
      ns("producer"),
      "Select Producer:",
      choices = NULL,
      selected = NULL
    ),
    
    # Year selection
    selectInput(
      ns("year"),
      "Select Year:",
      choices = NULL,
      selected = NULL
    ),
    
    # Field selection (for on-farm comparison)
    conditionalPanel(
      condition = "output.has_multiple_fields",
      ns = ns,
      selectInput(
        ns("field"),
        "Select Field (optional):",
        choices = c("All fields" = "all"),
        selected = "all"
      ),
      helpText("Select a specific field for detailed analysis, or 'All fields' for summary")
    ),
    
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
    
    # Hidden output to track if button should be enabled
    verbatimTextOutput(ns("debug_selections"))
  )
}