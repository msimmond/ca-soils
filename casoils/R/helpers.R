#' Pull unique values from one column of dataframe
#'
#' @param df Dataframe with column to extract unique values from.
#' @param target Variable to pull unique vector of (i.e. crop or
#'   county).
#'
#' @returns Vector of unique values from target column.
#' @examples
#' washi_data |>
#'   pull_unique(target = crop)
#'
#' @export
#'
pull_unique <- function(df, target) {
  unique(df[[target]])
}

#' Summarize by project
#'
#' @param results_long Long format dataframe with soil health results.
#' @param dictionary Data dictionary with measurement information.
#'
#' @returns Dataframe with project-level summaries.
#' @export
#'
summarize_by_project <- function(results_long, dictionary) {
  results_long |>
    dplyr::summarize(
      Texture = calculate_mode(texture),
      .by = measurement_group
    )
}

#' Summarize by variable
#'
#' @param results_long Long format dataframe with soil health results.
#' @param dictionary Data dictionary with measurement information.
#'
#' @returns Dataframe with variable-level summaries.
#' @export
#'
summarize_by_var <- function(results_long, dictionary) {
  results_long |>
    dplyr::summarize(
      value = mean(value, na.rm = TRUE),
      .by = c(measurement, measurement_group)
    )
}

# Helper function for mode calculation (internal use)
calculate_mode <- function(x) {
  uniqx <- unique(stats::na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
