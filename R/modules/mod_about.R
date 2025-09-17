# =============================================================================
# mod_about.R — About Section Module
#
# Purpose:
#   - Provides an "About" tab with information about the California Soil Health Reports app
#   - Similar to the "Learn More" tab in dirt-data-reports
#
# =============================================================================

mod_about_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "container-fluid",
      style = "padding: 20px;",
      
      # Header
      div(
        class = "row",
        div(
          class = "col-12",
          h2("About California Soil Health Reports", style = "color: #2c3e50; margin-bottom: 20px;"),
          hr()
        )
      ),
      
      # Main content
      div(
        class = "row",
        div(
          class = "col-12",
          
          # App Description
          div(
            class = "well",
            h3("What is this app?", style = "color: #34495e;"),
            p("The California Soil Health Reports app is a web-based tool that helps agricultural producers, researchers, and extension professionals generate comprehensive soil health reports from their field data. Simply upload your data, customize your project information, and download reports in HTML or DOCX format.")
          ),
          
          # How it works
          div(
            class = "well",
            h3("How does it work?", style = "color: #34495e;"),
            p("The app follows a simple 7-step workflow:"),
            tags$ol(
              tags$li("Download the Excel template with standardized data collection fields"),
              tags$li("Upload your completed data file"),
              tags$li("Filter your data by site type, crop, and soil texture"),
              tags$li("Customize project information and summaries"),
              tags$li("Select specific producer, year, and field data"),
              tags$li("Choose grouping variables for data analysis"),
              tags$li("Generate and download your professional soil health report")
            )
          ),
          
          # Soil Health Indicators
          div(
            class = "well",
            h3("Soil Health Indicators", style = "color: #34495e;"),
            p("The reports include comprehensive analysis of four key soil health categories:"),
            tags$ul(
              tags$li(tags$strong("Physical:"), "Soil texture, wet aggregate stability, infiltration rate"),
              tags$li(tags$strong("Chemical:"), "pH, cation exchange capacity, essential plant nutrients, nitrogen forms"),
              tags$li(tags$strong("Biological:"), "Microbial biomass, fungi:bacteria ratios, biopores, visible biodiversity"),
              tags$li(tags$strong("Carbon:"), "Total carbon, soil organic matter, soil respiration, active carbon")
            ),
            p("Each indicator includes detailed explanations of what it measures, why it's important, and how it was analyzed in your specific assessment.")
          ),
          
          
          # Development & Credits
          div(
            class = "well",
            h3("Development & Credits", style = "color: #34495e;"),
            p("This application was developed by", tags$strong("Maegen Simmonds"), "in collaboration with UC Agriculture and Natural Resources (UCANR) and the California Farm Demonstration Network (CFDN)."),
            p("The app is built using the {casoilsutils} R package, also developed by Maegen Simmonds, which builds on and reuses functions originally developed in the {soils} package, created by the Washington State Department of Agriculture and Washington State University as part of the Washington Soil Health Initiative (WASHI)."),
            p("Development of this application was supported by funds from the", tags$strong("Climate Action Research Grants Program of the University of California, Grant # R02CP6986"), ".")
          ),
          
          # Contact Information
          div(
            class = "well",
            h3("Contact & Support", style = "color: #34495e;"),
            p("For technical support or questions about soil health interpretation, please contact your local UC Cooperative Extension office or visit the", 
              tags$a(href = "https://ucanr.edu/", target = "_blank", "UCANR website", .noWS = "after"), ".")
          )
        )
      )
    )
  )
}

mod_about_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server-side logic needed for the About page
  })
}
