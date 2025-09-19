# =============================================================================
# mod_project_info.R — Project Info Customization Module
#
# Purpose:
#   - Allows users to customize project-specific information for reports
#   - Provides sensible defaults for common project text
#   - Stores values in app state for use in Quarto rendering
#
# Features:
#   - Project name customization
#   - Project summary (introduction) editing
#   - Project results section introduction
#   - Looking forward (conclusion) text
#   - Rich text editing with markdown support
# =============================================================================

mod_project_info_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "step-section",
    div(class = "step-title", "Step 3: Project Information"),
    div(
      class = "form-content",
      p(
        class = "form-text",
        "Customize your report with project-specific information. The text below will appear in your generated reports."
      ),
      
      # Project Name
      textInput(
        ns("project_name"),
        label = "Project Name",
        value = "Soil Health Assessment Project",
        placeholder = "Enter your project name"
      ),
      
      # Producer Name (for report title)
      textInput(
        ns("producer_name"),
        label = "Producer Name (for Report Title)",
        value = "",
        placeholder = "Enter producer name as it should appear in report title (leave empty to use data selection)"
      ),
      
      # Project Summary
      div(
        style = "margin: 15px 0;",
        tags$label("Project Summary (Introduction)"),
        tags$small(
          class = "form-text text-muted",
          "This appears at the top of your report below the title"
        ),
        textAreaInput(
          ns("project_summary"),
          label = NULL,
          value = "Thank you for participating in our soil health assessment project. This report provides detailed analysis of soil samples collected from your fields, including physical, chemical, biological, and carbon indicators of soil health.",
          rows = 12,
          placeholder = "Describe your project, thank participants, and provide context..."
        )
      ),
      
      # Looking Forward
      div(
        style = "margin: 15px 0;",
        tags$label("Looking Forward (Conclusion)"),
        tags$small(
          class = "form-text text-muted",
          "This appears at the end of your report as a summary and call to action"
        ),
        textAreaInput(
          ns("looking_forward"),
          label = NULL,
          value = "",
          rows = 4,
          placeholder = "Provide summary, next steps, and thank participants..."
        )
      ),
      
      # Preview button
      div(
        style = "margin-top: 20px;",
        actionButton(ns("preview_text"), "Preview Text", class = "btn-primary")
      )
    )
  )
}

mod_project_info_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Store project info values in state
    observe({
      state$project_info <- list(
        project_name = input$project_name,
        producer_name = input$producer_name,
        project_summary = input$project_summary,
        looking_forward = input$looking_forward
      )
    })
    
    # Preview functionality
    observeEvent(input$preview_text, {
      showModal(modalDialog(
        title = "Text Preview",
        size = "l",
        div(
          style = "max-height: 70vh; overflow-y: auto;",
          
          # Project Name
          tags$h3(input$project_name),
          
          # Project Summary
          tags$h4("Project Summary"),
          tags$div(
            style = "background: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;",
            input$project_summary
          ),
          
          # Looking Forward
          tags$h4("Looking Forward"),
          tags$div(
            style = "background: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;",
            input$looking_forward
          )
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    # Return reactive values for external access
    reactive({
      list(
        project_name = input$project_name,
        producer_name = input$producer_name,
        project_summary = input$project_summary,
        looking_forward = input$looking_forward
      )
    })
  })
}
