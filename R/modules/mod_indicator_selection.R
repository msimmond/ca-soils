# =============================================================================
# mod_indicator_selection.R — Indicator Selection Module
# =============================================================================

mod_indicator_selection_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "well",
      h4("Select Indicators to Include in Report"),
      p("Choose which soil health indicators you want to include in your report."),
      
      # Load the indicators from the same source as the report table
      uiOutput(ns("indicator_checkboxes")),
      
      div(
        style = "margin-top: 20px;",
        actionButton(ns("select_all"), "Select All", class = "btn btn-sm btn-outline-primary"),
        actionButton(ns("deselect_all"), "Deselect All", class = "btn btn-sm btn-outline-secondary"),
        style = "margin-left: 10px;"
      )
    )
  )
}

mod_indicator_selection_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load indicators from the same source as the report table
    indicators_data <- reactive({
      indicators_path <- file.path("quarto", "inst", "extdata", "indicators.csv")
      if (file.exists(indicators_path)) {
        indicators <- read.csv(indicators_path, check.names = FALSE, encoding = "UTF-8", strip.white = TRUE)
        
        # Safety check for required columns
        if (!"Soil Health Indicator" %in% names(indicators)) {
          stop("Column 'Soil Health Indicator' not found in indicators.csv")
        }
        if (!"Soil Function" %in% names(indicators)) {
          stop("Column 'Soil Function' not found in indicators.csv")
        }
        
        return(indicators)
      } else {
        # Fallback: return empty data frame if file not found
        return(data.frame(
          "Soil Health Indicator" = character(0),
          "Soil Function" = character(0),
          "Scoring Curve Type" = character(0),
          stringsAsFactors = FALSE
        ))
      }
    })
    
    # Create mapping from indicator names to column names using data dictionary
    indicator_mapping <- reactive({
      req(state$data_dictionary)
      
      dict <- state$data_dictionary
      indicators <- indicators_data()
      
      
      # Create explicit mapping table for indicator names to column names
      explicit_mapping <- list(
        "Water Stable Aggregates" = "water_stable_aggregates_pct",
        "Aggregate Stability Index" = "agg_stability_slakes", 
        "Infiltration (1st inch)" = "infil1_sec",
        "Infiltration (2nd inch)" = "infil2_sec",
        "pH" = "ph",
        "CEC" = "cec_meq_100g",
        "P" = "p_ppm",
        "K" = "k_ppm",
        "Ca" = "ca_ppm",
        "Mg" = "mg_ppm",
        "Total N" = "n_pct",
        "PMN - 28 days" = "pmn_t28",
        "NO3-N" = "no3",
        "Nitrate Quick Test" = "nitrate_quick",
        "Total C" = "c_pct",
        "Organic Matter" = "om_pct",
        "CO2 Resp." = "co2_resp_24",
        "POXC" = "poxc_ppm",
        "Microbial Biomass" = "microbial_biomass",
        "Bacterial Biomass" = "bacterial_biomass",
        "Fungal Biomass" = "fungal_biomass",
        "AMF Biomass" = "amf_biomass",
        "Fungi:Bacteria" = "fungi_bacteria_ratio",
        "Protozoan Biomass" = "protozoa_biomass",
        "Undifferentiated Biomass" = "undiff_biomass",
        "Biopore Count" = "biopore_count",
        "Visible Invert. Count" = "biodiversity_count"
      )
      
      # Filter mapping to only include indicators that exist in the CSV
      mapping <- list()
      if (!is.null(indicators) && nrow(indicators) > 0) {
        for (i in 1:nrow(indicators)) {
          indicator_name <- indicators[["Soil Health Indicator"]][i]
          if (indicator_name %in% names(explicit_mapping)) {
            mapping[[indicator_name]] <- explicit_mapping[[indicator_name]]
          }
        }
      }
      
      return(mapping)
    })
    
    
    # Create indicator dropdown with checkboxes
    output$indicator_checkboxes <- renderUI({
      req(indicators_data(), indicator_mapping())
      
      indicators <- indicators_data()
      mapping <- indicator_mapping()
      
      # Get all indicator names that have mappings
      all_indicators <- indicators[["Soil Health Indicator"]]
      available_indicators <- all_indicators[all_indicators %in% names(mapping)]
      
      # Create choices using the indicator names
      choices <- as.character(available_indicators)
      choices <- setNames(choices, choices)
      
      div(
        class = "indicator-selection",
        style = "padding: 15px; border: 1px solid #ddd; border-radius: 5px;",
        div(
          style = "margin-bottom: 10px;",
          tags$label("Select indicators to include in your report:", style = "font-weight: bold;")
        ),
        div(
          style = "max-height: 300px; overflow-y: auto; border: 1px solid #ccc; border-radius: 4px; padding: 10px; background-color: #f9f9f9;",
          checkboxGroupInput(
            inputId = ns("all_indicators"),
            label = NULL,
            choices = choices,
            selected = choices, # Select all by default
            inline = FALSE
          )
        )
      )
    })
    
    # Select all indicators
    observeEvent(input$select_all, {
      req(indicators_data(), indicator_mapping())
      
      indicators <- indicators_data()
      mapping <- indicator_mapping()
      
      # Get all indicator names that have mappings
      all_indicators <- indicators[["Soil Health Indicator"]]
      available_indicators <- all_indicators[all_indicators %in% names(mapping)]
      
      # Create choices using the indicator names
      choices <- as.character(available_indicators)
      choices <- setNames(choices, choices)
      
      updateCheckboxGroupInput(
        session = session,
        inputId = "all_indicators",
        selected = choices
      )
    })
    
    # Deselect all indicators
    observeEvent(input$deselect_all, {
      updateCheckboxGroupInput(
        session = session,
        inputId = "all_indicators",
        selected = character(0)
      )
    })
    
    # Collect selected indicators and convert to column names
    selected_indicators <- reactive({
      req(indicators_data(), indicator_mapping())
      
      mapping <- indicator_mapping()
      
      # Get selected display names from the single checkbox group
      selected_display_names <- input$all_indicators
      
      # Convert display names to column names
      selected_column_names <- unlist(mapping[selected_display_names], use.names = FALSE)
      selected_column_names <- selected_column_names[!is.na(selected_column_names)]
      
      return(selected_column_names)
    })
    
    # Update state with selected indicators (as column names)
    observe({
      state$selected_indicators <- selected_indicators()
    })
    
    # Return selected indicators for use in other modules
    return(list(
      selected_indicators = selected_indicators
    ))
  })
}