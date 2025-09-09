# =============================================================================
# mod_report.R — Report Generation Module (UI + Server)
#
# OVERVIEW
# -----------------------------------------------------------------------------
# This module lets a user render a Producer Report (HTML) via the Quarto
# template (`quarto/report_template.qmd`) using the dataset already loaded into the app.
#
# What it does:
#   1) Validates the current selections (producer, year, grouping var).
#   2) Writes the in-memory data.frame to a stable, hashed CSV on disk
#      so Quarto can read it.
#   3) Calls generate_soil_health_report() (wrapper around Quarto).
#   4) Shows a preview iframe and download buttons.
#
# Key implementation details for developers:
#    - Source wrapper.R so generate_soil_health_report() is available
#    - We compute a data hash (via {digest} if available) and memoise the
#      rendering call on (data_hash, producer, year, grouping, config).
#      This gives fast re-renders when inputs are identical.
#    - The temporary CSV path is stable (based on the same hash) so the memoisation
#     cache can actually hit across runs.
#   - Validation is explicit and returns structured results to the UI
#     (no stray `stop()`s inside the reactive).
#   - We compare years as *character* to avoid numeric/string mismatches.
#
# Inputs provided by parent server:
#   - cfg   : list, from global.R (get_cfg()); needs cfg$paths$output_dir
#   - state : reactiveValues carrying:
#       $data                  (data.frame)   uploaded/filtered dataset
#       $data_uploaded         (logical)      TRUE after upload
#       $selected_producer     (character)
#       $selected_year         (character or numeric)
#       $selected_grouping_var (character)
#       $selected_field        (optional)     specific field ID or "all"
#
# Outputs shown in the UI:
#   - Status banner (ready/generating/complete/error)
#   - Report preview (iframe)
#   - Download buttons (HTML/PDF)
#
# External dependencies used here:
#   - memoise, fs, readr (installed via renv)
#     We use digest::digest() if available; if not installed, we fall back to
#     a lightweight hash (row/col/NA count) so caching still works.
# =============================================================================


# -----------------------------
# UI
# -----------------------------
mod_report_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Report"),

    actionButton(
      ns("generate"),
      "Generate Report",
      class = "btn-primary btn-lg",
      width = "100%"
    ),

    tags$hr(),

    conditionalPanel(
      condition = "output.report_status != 'ready'",
      ns = ns,
      div(class = "alert alert-info", id = ns("status_box"),
          textOutput(ns("status_text")))
    ),

    # Progress bar for report generation - using withProgress instead
    conditionalPanel(
      condition = "output.show_progress",
      ns = ns,
      div(class = "progress",
          div(class = "progress-bar progress-bar-striped progress-bar-animated",
              role = "progressbar", style = "width: 100%",
              "Generating report..."))
    ),

    conditionalPanel(
      condition = "output.has_report",
      ns = ns,
      tags$hr(),
      h5("Generated Report"),
      verbatimTextOutput(ns("report_info")),

      # Report preview and download buttons removed - not needed
    ),

    conditionalPanel(
      condition = "output.has_error",
      ns = ns,
      div(class = "alert alert-danger", textOutput(ns("error_text")))
    )
  )
}


# -----------------------------
# SERVER
# -----------------------------
mod_report_server <- function(id, cfg, state, data_pipeline) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(x, y) if (is.null(x)) y else x

    # ---- Ensure wrapper is available (defines generate_soil_health_report) ---
    # Source once per session; harmless if sourced again.
    wrapper_path <- file.path(getwd(), "R", "logic", "wrapper.R")
    if (!file.exists(wrapper_path)) {
      stop("Could not find wrapper at: ", wrapper_path)
    }
    # Only source if the function is not already in the environment
    if (!exists("generate_soil_health_report", mode = "function")) {
      source(wrapper_path, local = FALSE)
    }

    # ---- Output directory & static path for preview --------------------------
    output_dir <- cfg$paths$output_dir %||% "outputs"
    fs::dir_create(output_dir)
    shiny::addResourcePath("reports", output_dir)

    # Stable cache dir for hashed CSVs
    cache_dir <- fs::path(output_dir, ".cache")
    fs::dir_create(cache_dir)

    # ---- helpers -------------------------------------------------------------

    # Compute a stable hash for the current data. Prefer {digest}, else a fallback.
    compute_df_hash <- function(df) {
      if (requireNamespace("digest", quietly = TRUE)) {
        return(digest::digest(df))
      }
      paste("fallback", nrow(df), ncol(df), sum(is.na(df)), sep = "_")
    }

    # Validate producer/year (compare year as character to avoid type mismatch)
    validate_producer_year_choice <- function(df, producer, year_chr) {
      if (!"producer_id" %in% names(df) || !"year" %in% names(df)) {
        return(list(ok = FALSE,
                    msg = "Data is missing required columns: 'producer_id' and/or 'year'."))
      }
      if (!producer %in% df$producer_id) {
        return(list(
          ok  = FALSE,
          msg = paste0(
            "Producer '", producer, "' not found. Available: ",
            paste(unique(df$producer_id), collapse = ", ")
          )
        ))
      }
      df$year_chr <- as.character(df$year)
      available   <- unique(df$year_chr[df$producer_id == producer])
      if (!as.character(year_chr) %in% available) {
        return(list(
          ok  = FALSE,
          msg = paste0(
            "Year ", year_chr, " not found for producer '", producer, "'. Available: ",
            paste(available, collapse = ", ")
          )
        ))
      }
      list(ok = TRUE, msg = NULL)
    }

    # Memoised wrapper around the Quarto render call.
    # Keys: (df_hash, producer_id, year_chr, grouping_var, config_hash, project_info_hash)
    generate_report_memoized <- memoise::memoise(
      function(df_hash, producer_id, year_chr, grouping_var, config_hash, tmp_csv_path, out_dir, dict_path = NULL, project_info_hash = NULL) {
        generate_soil_health_report(
          data_path    = tmp_csv_path,
          producer_id  = producer_id,
          year         = year_chr,          # pass character; template handles it
          grouping_var = grouping_var,
          config       = get_cfg(),         # config lives in options()
          output_dir   = out_dir,
          dict_path    = dict_path,
          project_info = if (!is.null(project_info_hash)) state$project_info else NULL
        )
      },
      cache = memoise::cache_memory()
    )

    # --------------------------
    # Button → render report
    # --------------------------
    report_result <- eventReactive(input$generate, {
      req(state$data_uploaded, state$data)

      if (is.null(state$selected_producer) || state$selected_producer == "")
        return(list(status = "error", path = NULL, error = "Please select a producer."))
      if (is.null(state$selected_year) || state$selected_year == "")
        return(list(status = "error", path = NULL, error = "Please select a year."))
      if (is.null(state$selected_grouping_var) || state$selected_grouping_var == "")
        return(list(status = "error", path = NULL, error = "Please select a grouping variable."))

      df        <- state$data
      producer  <- state$selected_producer
      year_chr  <- as.character(state$selected_year)   # normalize to character
      grouping  <- state$selected_grouping_var

      # Validate against data
      v <- validate_producer_year_choice(df, producer, year_chr)
      if (!isTRUE(v$ok)) {
        return(list(status = "error", path = NULL, error = v$msg))
      }

      # Stable hash & CSV path
      df_hash <- compute_df_hash(df)
      config_hash <- if (requireNamespace("digest", quietly = TRUE)) {
        digest::digest(get_cfg())
      } else {
        paste0("cfg_", length(unlist(get_cfg())))
      }

      tmp_csv <- fs::path(cache_dir, paste0("report_", df_hash, ".csv"))
      tmp_dict <- NULL
      
      # Always write the dictionary sidecar if the attribute exists, regardless of CSV cache
      dict_attr <- attr(df, "measurement_info")
      tmp_dict <- NULL
      if (!is.null(dict_attr)) {
        tmp_dict <- sub("\\.csv$", "_dictionary.csv", tmp_csv)
        readr::write_csv(dict_attr, tmp_dict)
      }
      
      if (!fs::file_exists(tmp_csv)) {
        readr::write_csv(df, tmp_csv)
      }

      # Create hash for project info to include in memoization
      project_info_hash <- if (!is.null(state$project_info)) {
        if (requireNamespace("digest", quietly = TRUE)) {
          digest::digest(state$project_info)
        } else {
          paste0("proj_", length(unlist(state$project_info)))
        }
      } else NULL

      res <- tryCatch({
        withProgress(message = "Generating report...",
                     detail  = paste("Producer:", producer, "Year:", year_chr),
                     value   = 0, {
          incProgress(0.05, detail = "Validating data...")
          Sys.sleep(0.3)
          
          incProgress(0.1, detail = "Preparing data files...")
          Sys.sleep(0.2)
          
          # Write CSV and dictionary files
          if (!fs::file_exists(tmp_csv)) {
            readr::write_csv(df, tmp_csv)
          }
          
          incProgress(0.15, detail = "Setting up Quarto parameters...")
          Sys.sleep(0.2)
          
          incProgress(0.2, detail = "Starting Quarto rendering...")
          Sys.sleep(0.5)  # Give user time to see this message
          
          # The actual Quarto rendering happens here - this is the long part
          out_path <- generate_report_memoized(
            df_hash       = df_hash,
            producer_id   = producer,
            year_chr      = year_chr,
            grouping_var  = grouping,
            config_hash   = config_hash,
            tmp_csv_path  = tmp_csv,
            out_dir       = output_dir,
            dict_path     = tmp_dict,
            project_info_hash = project_info_hash
          )
          
          incProgress(0.95, detail = "Finalizing report...")
          Sys.sleep(0.3)
          
          incProgress(1, detail = "Done!")
          Sys.sleep(0.2)
          list(status = "success", path = out_path, error = NULL)
        })
      }, error = function(e) {
        list(status = "error", path = NULL, error = conditionMessage(e))
      })
      
      res
    }, ignoreInit = TRUE)


    
    # --------------------------
    # Reactive flags for UI
    # --------------------------
    output$report_status <- reactive({
      if (is.null(report_result())) "ready"
      else if (identical(report_result()$status, "success")) "complete"
      else "error"
    })
    outputOptions(output, "report_status", suspendWhenHidden = FALSE)

    output$show_progress <- reactive({ FALSE })
    outputOptions(output, "show_progress", suspendWhenHidden = FALSE)

    output$has_report <- reactive({
      !is.null(report_result()) && identical(report_result()$status, "success")
    })
    outputOptions(output, "has_report", suspendWhenHidden = FALSE)

    output$has_error <- reactive({
      !is.null(report_result()) && identical(report_result()$status, "error")
    })
    outputOptions(output, "has_error", suspendWhenHidden = FALSE)

    # --------------------------
    # Text + Preview
    # --------------------------
    output$status_text <- renderText({
      if (is.null(report_result())) "Ready to generate a report."
      else if (identical(report_result()$status, "success")) "Report generated successfully!"
      else "Error generating report."
    })

    output$report_info <- renderPrint({
      res <- report_result(); req(res$status == "success")
      
      # Get the base filename without extension from the HTML path
      html_path <- res$path
      base_name <- tools::file_path_sans_ext(basename(html_path))
      quarto_dir <- dirname(html_path)
      
      # Construct paths for both files
      html_file <- basename(html_path)
      docx_file <- paste0(base_name, ".docx")
      docx_path <- file.path(quarto_dir, docx_file)
      
      cat("Report Information:\n")
      cat("HTML File:", html_file, "\n")
      cat("HTML Size:", file.size(html_path), "bytes\n")
      cat("DOCX File:", docx_file, "\n")
      if (file.exists(docx_path)) {
        cat("DOCX Size:", file.size(docx_path), "bytes\n")
      } else {
        cat("DOCX Size: File not found\n")
      }
      cat("Generated:", format(file.info(html_path)$mtime), "\n")
    })

    # Report preview removed - not needed

    output$error_text <- renderText({
      res <- report_result(); req(res$status == "error")
      res$error
    })

    # Download buttons removed - reports are already generated and saved locally
  })
}