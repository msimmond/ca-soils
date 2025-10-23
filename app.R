# =============================================================================
# app.R — Main Shiny Application Entry Point
#
# Purpose:
#   - Define the top-level UI and server for the California Soil Health Reports application.
#   - Follows template-based workflow: Download Template → Upload Data → Generate Reports
#   - Simplified workflow without column mapping complexity
#
# Flow:
#   1) Download module (`mod_download`)
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
  library(digest)   # hashing (used inside mod_report)
})

# --- App configuration (read-only), provided by global.R ---------------------
cfg <- get_cfg()

# --- Module sources (use portable paths) -------------------------------------
source(file.path("R", "modules", "mod_build_reports.R"))
source(file.path("R", "modules", "mod_download.R"))
source(file.path("R", "modules", "mod_data_upload.R"))
source(file.path("R", "modules", "mod_data_filter.R"))
source(file.path("R", "modules", "mod_filters.R"))
source(file.path("R", "modules", "mod_project_info.R"))
source(file.path("R", "modules", "mod_grouping.R"))
source(file.path("R", "modules", "mod_indicator_selection.R"))
source(file.path("R", "modules", "mod_report.R"))
source(file.path("R", "modules", "mod_about.R"))

# =============================================================================
# UI
# =============================================================================
  ui <- navbarPage(
    title = div(
      style = "display: flex; align-items: center; gap: 10px;",
      img(src = "logo.png", height = "50px"),
      span("California Soil Health Reports", style = "color: #8b4513; font-weight: bold; font-size: 22px;")
    ),
    windowTitle = "California Soil Health Reports",
    id = "main_page",
    collapsible = TRUE,
    selected = "page_build_reports",
  
  # Header with styles
  header = tags$head(
    # Stepper CSS
    tags$link(rel = "stylesheet", type = "text/css", href = "css/stepper.css"),
    # Stepper JavaScript
    tags$script(src = "js/stepper.js"),
    # Font Awesome for icons
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    tags$style(HTML("
      .btn-group .btn { margin-right: 5px; }
      .progress       { margin: 10px 0;   }
      .alert          { margin: 10px 0;   }
      .well           { background-color: #f8f6f3; border: 1px solid #d4a574; border-radius: 4px; padding: 19px; margin-bottom: 20px; }
      .btn-primary, .btn.btn-primary    { background-color: #F1B434; border-color: #F1B434; }
      .btn-primary:hover, .btn.btn-primary:hover { background-color: #9FC9E4; border-color: #9FC9E4; }
      .btn-secondary, .btn.btn-secondary  { background-color: #F1B434; border-color: #F1B434; color: white; }
      .btn-secondary:hover, .btn.btn-secondary:hover { background-color: #9FC9E4; border-color: #9FC9E4; color: white; }
      .btn-outline-primary, .btn.btn-outline-primary { background-color: transparent; border-color: #F1B434; color: #F1B434; }
      .btn-outline-primary:hover, .btn.btn-outline-primary:hover { background-color: #F1B434; border-color: #F1B434; color: white; }
      .btn-outline-secondary, .btn.btn-outline-secondary { background-color: transparent; border-color: #F1B434; color: #F1B434; }
      .btn-outline-secondary:hover, .btn.btn-outline-secondary:hover { background-color: #F1B434; border-color: #F1B434; color: white; }
      .btn-success    { background-color: #6bb6ff; border-color: #6bb6ff; }
      .btn-success:hover { background-color: #F1B434; border-color: #F1B434; }
      .navbar-default { background-color: #f8f6f3; border-color: #d4a574; padding-top: 5px; padding-bottom: 5px; }
      .navbar-default .navbar-brand { color: #8b4513; font-weight: bold; padding: 5px 15px; }
      .navbar-default .navbar-nav > li > a { color: #8b4513; }
      .navbar-default .navbar-nav > .active > a { background-color: #5A8B3A; color: white; }
      .navbar-default .navbar-nav > .active > a:hover { background-color: #6bb6ff; color: white; }
      .step-item { opacity: 0.4; transition: opacity 0.3s ease; }
      .step-item.active { opacity: 1; }
      .step-item.completed { opacity: 0.7; }
    ")),
    if (!is.null(cfg$paths$styles) && file.exists(cfg$paths$styles)) {
      # Allow project-branded CSS defined in config.yml (paths.styles)
      tags$link(rel = "stylesheet", type = "text/css", href = cfg$paths$styles)
    }
  ),
  
  # Build Reports Tab (main functionality)
  tabPanel(
    title = "Build Reports",
    value = "page_build_reports",
    mod_build_reports_ui("build_reports")
  ),
  
  # About Tab
  tabPanel(
    title = "About",
    value = "page_about",
    mod_about_ui("about")
  )
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

  # Note: State and data pipeline are now managed within the stepper module

  # Main stepper module that manages all 8 steps
  stepper_state <- mod_build_reports_server("build_reports")
  
  # About module
  mod_about_server("about")

  # Note: All data handling, filtering, and state management is now done within the stepper module


}

# Launch application
shinyApp(ui, server)