#' Validate Study Result Structure
#'
#' Validates whether a study result has the expected structure, column names,
#' and data types as returned by [load_study_results()].
#'
#' @param sr A tibble representing a study result from JATOS.
#' @param verbose Logical. If `TRUE`, prints information about failed checks.
#'   Default is `FALSE`.
#'
#' @return A logical scalar: `TRUE` if all validation checks pass, `FALSE` otherwise.
#'
#' @details
#' Performs comprehensive validation of a study result including:
#' - Verifies the input is a tibble
#' - Checks for required numerical columns: `id`, `worker_id`, `batch_id`,
#'   `group_id`, `is_quota_reached`
#' - Checks for required text columns: `uuid`, `study_code`, `study_state`,
#'   `message`, `worker_type`, `batch_uuid`, `batch_title`
#' - Checks for required date/duration columns: `start_date`, `end_date`,
#'   `duration`, `last_seen_date`
#' - Validates the `component_results` column is a list
#' - Validates all nested component results are valid (via
#'   [is_valid_component_result()])
#' - Ensures date columns are either POSIXct/Duration types OR numeric/character
#'   (but not a mix)
#'
#' When `verbose = TRUE`, failed checks are reported via `cli::cli_alert_info()`.
#'
#' @seealso [load_study_results()] which creates study results that pass
#' this validation; [is_valid_component_result()] for component-level validation.
#'
#' @examples
#' \dontrun{
#' # Validate a study result
#' result <- load_study_results(study_data)
#' is_valid_study_result(result)
#'
#' # Show which checks fail
#' is_valid_study_result(result, verbose = TRUE)
#' }
#'
#' @export
is_valid_study_result <- function(sr, verbose = FALSE) {
  scols_num <- c("id", "worker_id", "batch_id", "group_id", "is_quota_reached")
  scols_chr <- c(
    "uuid",
    "study_code",
    "study_state",
    "message",
    "worker_type",
    "batch_uuid",
    "batch_title"
  )
  scols_dat <- c("start_date", "end_date", "duration", "last_seen_date")

  component_status <-
    purrr::map_lgl(
      sr$component_results,
      \(x) {
        is_valid_component_result(x)
      }
    )

  dates_as_dates <-
    purrr::map_lgl(
      scols_dat,
      \(x) {
        if (stringr::str_detect(x, "date")) {
          lubridate::is.timepoint(sr[[x]])
        } else {
          lubridate::is.duration(sr[[x]])
        }
      }
    )
  dates_as_numbers_or_text <-
    purrr::map_lgl(
      scols_dat,
      \(x) {
        if (stringr::str_detect(x, "date")) {
          is.numeric(sr[[x]])
        } else {
          is.character(sr[[x]])
        }
      }
    )

  status <- list(
    "It is a tibble" = tibble::is_tibble(sr),
    "Numerical columns present" = all(scols_num %in% names(sr)),
    "Text columns present" = all(scols_chr %in% names(sr)),
    "Date columns present" = all(scols_dat %in% names(sr)),
    "Numerical columns contain numbers" = all(purrr::map_lgl(scols_num, \(x) {
      is.numeric(sr[[x]])
    })),
    "Text columns contain text" = all(purrr::map_lgl(scols_chr, \(x) {
      is.character(sr[[x]])
    })),
    "Date columns contain dates or numbers" = all(
      dates_as_dates != dates_as_numbers_or_text
    ),
    "Component results column is present" = ("component_results" %in%
      names(sr)),
    "Component results column is a list" = is.list(sr$component_results),
    "Component results are valid" = all(component_status == TRUE)
  )
  result <- status |> unlist() |> all()
  if ((result == FALSE) && verbose) {
    for (i in which(unlist(status) == FALSE)) {
      cli::cli_alert_info("Not TRUE: {names(status)[i]}")
    }
  }
  result
}
