# =============================================================================
# mod_column_mapping.R — Column Mapping Module (UI + Server)
#
# Purpose:
#   - Provides an interface to map CSV column names to standard names.
#   - After "Apply Mapping" is clicked, outputs a data.frame with columns
#     renamed to standard names for consistent downstream processing.
#
# Inputs:
#   id            : Module namespace ID.
#   data_in       : reactive() returning a data.frame (raw upload).
#   required_cols : Character vector of required standard names.
#   initial_map   : Named character vector (optional), names=standard, values=source.
#
# Outputs (list of reactives to parent):
#   $mapped_df() : data.frame with columns renamed to standard names.
#   $mapping()   : Named character vector (std -> source).
#   $ok()        : Logical reactive; TRUE only after valid "Apply Mapping".
#
# Notes:
#   - No value cleaning/transformations are performed; only column renaming.
#   - Duplicate-source selection is prevented via validation.
# =============================================================================

mod_column_mapping_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Column Mapping"),
    uiOutput(ns("mapping_ui")),  # rendered only when data exists
    uiOutput(ns("map_status"))   # hidden until data exists; NA shows nothing
  )
}

mod_column_mapping_server <- function(id, data_in, required_cols, initial_map = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    `%||%` <- function(x, y) if (is.null(x)) y else x
    is_filled <- function(x) !is.na(x) & nzchar(x)

    stopifnot(length(required_cols) > 0L)

    # Validate and normalize initial_map to known required_cols
    if (!is.null(initial_map)) {
      if (is.null(names(initial_map)) || any(!nzchar(names(initial_map)))) {
        warning("initial_map must be a named character vector; ignoring invalid entries.")
      }
      initial_map <- initial_map[names(initial_map) %in% required_cols]
    }

    # Start state: one entry per required standard name (value = NA until chosen)
    start_map <- initial_map %||% setNames(rep(NA_character_, length(required_cols)), required_cols)

    map_rv <- reactiveVal(start_map)  # named chr vector: std -> source col (may be incomplete)
    ok_rv  <- reactiveVal(NA)         # NA = no status yet; TRUE/FALSE after apply/edit

    # Reset status when a new dataset is provided
    observeEvent(data_in(), {
      ok_rv(NA)
      # To reset selects when the dataset changes, uncomment:
      # map_rv(start_map)
    }, ignoreInit = TRUE)

    # --- Build the mapping UI (only after data exists) -------------------------
    output$mapping_ui <- renderUI({
      df <- data_in()
      req(df)  # nothing shown until a dataset exists

      choices <- names(df)

      tagList(
        lapply(names(map_rv()), function(std) {
          selectInput(
            ns(paste0("map_", std)),
            label   = paste("Select column for:", std),
            choices = c("", choices),   # "" = not chosen yet
            selected = {
              sel <- map_rv()[[std]]
              if (isTRUE(is_filled(sel))) sel else ""
            }
          )
        }),
        div(
          style = "margin-top: 0.75rem;",
          actionButton(ns("apply"), "Apply Mapping", class = "btn btn-primary")
        ),
        helpText("Once mapping is applied, all downstream steps will use the standard names.")
      )
    })

    # --- Track user changes; invalidate status only after data exists ----------
    observe({
      df <- data_in()
      req(df)

      current <- map_rv()
      changed <- FALSE
      for (std in names(current)) {
        val <- input[[paste0("map_", std)]]
        if (!is.null(val) && !identical(val, current[[std]])) {
          current[[std]] <- val
          changed <- TRUE
        }
      }
      if (changed) {
        map_rv(current)
        if (isTRUE(ok_rv())) ok_rv(FALSE)
        if (isFALSE(ok_rv())) ok_rv(FALSE)
      }
    })

    # --- Apply Mapping: validate & lock ---------------------------------------
    observeEvent(input$apply, {
      df <- data_in(); req(df)
      mapping <- map_rv()

      # Treat NA and "" as missing
      missing <- names(mapping)[!is_filled(mapping)]

      # Validate only filled entries
      filled <- mapping[is_filled(mapping)]
      bad <- setdiff(unname(filled), names(df))

      vals <- unname(filled)
      dups <- vals[duplicated(vals)]

      if (length(missing)) {
        showNotification(
          paste("Missing mappings for:", paste(missing, collapse = ", ")),
          type = "error"
        )
        ok_rv(FALSE); return(invisible())
      }
      if (length(bad)) {
        showNotification(
          paste("Mapped to unknown columns:", paste(bad, collapse = ", ")),
          type = "error"
        )
        ok_rv(FALSE); return(invisible())
      }
      if (length(dups)) {
        clash_src <- unique(dups)
        who <- lapply(clash_src, function(src) names(mapping)[mapping == src])
        msg <- paste0(
          "Each standard must map to a distinct source column.\n",
          paste(mapply(function(src, stds) sprintf("• %s → %s", src, paste(stds, collapse = ", ")),
                       clash_src, who),
                collapse = "\n")
        )
        showNotification(msg, type = "error", duration = NULL)
        ok_rv(FALSE); return(invisible())
      }

      ok_rv(TRUE)
      showNotification("Mapping applied.", type = "message")
    })

    # --- Status chip (hidden until data exists; NA = no status) ---------------
    output$map_status <- renderUI({
      df <- data_in()
      if (is.null(df)) return(NULL)

      ok <- ok_rv()
      if (isTRUE(ok)) {
        tags$span("✅ Mapping OK", style = "color:#2b8a3e;")
      } else if (isFALSE(ok)) {
        tags$span("⚠️ Apply mapping to continue", style = "color:#a60f2d;")
      } else {
        NULL
      }
    })

    # --- Expose mapped data (only after valid Apply) ---------------------------
    mapped_df <- reactive({
      req(isTRUE(ok_rv()))
      df <- data_in()
      mp <- map_rv()

      # Use only filled entries to build the rename map
      mp <- mp[is_filled(mp)]

      # Build index of source columns and validate existence
      src <- unname(mp)        # source column names in df
      dst <- names(mp)         # target standard names
      idx <- match(src, names(df))

      missing_sources <- src[is.na(idx)]
      validate(need(!length(missing_sources),
                    paste("Mapped source columns not found:",
                          paste(missing_sources, collapse = ", "))))

      # Base-R rename
      names(df)[idx] <- dst
      df
    })

    # Return API for downstream modules
    list(
      mapped_df = mapped_df,          # reactive(data.frame)
      mapping   = reactive(map_rv()), # reactive(named chr)
      ok        = ok_rv               # reactive logical (NA/TRUE/FALSE)
    )
  })
}