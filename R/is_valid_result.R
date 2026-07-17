#' Validate Complete JATOS Results Structure
#'
#' Validates whether a JATOS results object has the expected structure, column names,
#' and data types as returned by [load_results_from_text()].
#'
#' @param rr A tibble representing complete JATOS results (typically from
#'   [load_results_from_text()]).
#' @param verbose Logical. If `TRUE`, prints information about failed checks.
#'   Default is `FALSE`.
#'
#' @return A logical scalar: `TRUE` if all validation checks pass, `FALSE` otherwise.
#'
#' @details
#' Performs comprehensive validation of a JATOS results object including:
#' - Verifies the input is a tibble
#' - Checks for required columns: `api_version`, `study_id`, `study_uuid`, `study_title`
#' - Validates data types: `study_id` (integer), `study_uuid` and `study_title` (character)
#' - Validates the `study_results` column is a list
#' - Validates all nested study results are valid (via [is_valid_study_result()])
#'
#' This function validates the complete hierarchical structure: top-level metadata,
#' study results, and all nested component results.
#'
#' When `verbose = TRUE`, failed checks are reported via `cli::cli_alert_info()`.
#'
#' @seealso [load_results_from_text()] which creates results that pass this validation;
#' [is_valid_study_result()], [is_valid_component_result()] for nested validation.
#'
#' @examples
#' \dontrun{
#' # Validate JATOS results
#' demo <- prepare_demo(nofile = TRUE)
#' txt <- demo$meta |> httr2::resp_body_string()
#' r1 <- load_results_from_text(txt)
#' is_valid_result(r1)
#'
#' # Show which checks fail
#' r1$study_id <- "001"
#' is_valid_result(r1, verbose = TRUE)
#' }
#'
#' @export
is_valid_result <- function(rr, verbose = FALSE) {
  expected_cols <- c("api_version", "study_id", "study_uuid", "study_title")
  study_status <-
    purrr::map_lgl(rr$study_results, \(x) is_valid_study_result(x))

  status <- list(
    "It is a tibble" = tibble::is_tibble(rr),
    "Expected columns are present" = all(expected_cols %in% names(rr)),
    "Column types are correct" = is.integer(rr$study_id) &&
      is.character(rr$study_uuid) &&
      is.character(rr$study_title),
    "Study results column is present" = ("study_results" %in% names(rr)),
    "Study results column is a list" = is.list(rr$study_results),
    "Study results are valid" = all(study_status == TRUE)
  )
  result <- status |> unlist() |> all()
  if ((result == FALSE) && verbose) {
    for (i in which(unlist(status) == FALSE)) {
      cli::cli_alert_info("Not TRUE: {names(status)[i]}")
    }
  }
  result
}
