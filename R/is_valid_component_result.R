#' Validate Component Result Structure
#'
#' Validates whether a component result has the expected structure, column names,
#' and data types as returned by [load_component_results()].
#'
#' @param cr A tibble representing a component result.
#' @param verbose Prints a report of failed checks (default FALSE).
#'
#' @return A logical scalar: `TRUE` if all validation checks pass, `FALSE` otherwise.
#'
#' @details
#' Performs comprehensive validation of a component result including:
#' - Verifies the input is a tibble
#' - Checks for required numerical columns: `id`, `component_id`, `is_quota_reached`
#' - Checks for required text columns: `component_uuid`, `component_state`, `path`,
#'   `data_size_human_readable`
#' - Checks for required date/duration columns: `start_date`, `end_date`, `duration`
#' - Validates the `files` column is a list containing either `NULL` or tibbles with
#'   columns `filename`, `size`, `size_human_readable`
#'
#' All checks must pass for the function to return `TRUE`.
#'
#' @seealso [load_component_results()] which creates component results that pass
#' this validation.
#'
#' @examples
#' \dontrun{
#' # Validate a component result
#' result <- load_component_results(component_data)
#' is_valid_component_result(result)
#' }
#'
#' @export
is_valid_component_result <- function(cr, verbose = FALSE) {
  ccols_num <- c("id", "component_id", "is_quota_reached")
  ccols_chr <- c(
    "component_uuid",
    "component_state",
    "path",
    "data_size_human_readable"
  )
  ccols_dat <- c("start_date", "end_date", "duration")
  fcols <- c("filename", "size", "size_human_readable")
  ff_status <-
    purrr::map_lgl(
      cr$files,
      \(x) (is.null(x) || (tibble::is_tibble(x) && all(fcols %in% names(x))))
    )
  dates_as_dates <-
    purrr::map_lgl(
      ccols_dat,
      \(x) {
        if (stringr::str_detect(x, "date")) {
          lubridate::is.timepoint(cr[[x]])
        } else {
          lubridate::is.duration(cr[[x]])
        }
      }
    )
  dates_as_numbers_or_text <-
    purrr::map_lgl(
      ccols_dat,
      \(x) {
        if (stringr::str_detect(x, "date")) {
          is.numeric(cr[[x]])
        } else {
          is.character(cr[[x]])
        }
      }
    )

  status <- list(
    "It is a tibble" = tibble::is_tibble(cr),
    "Numerical columns present" = all(ccols_num %in% names(cr)),
    "Text columns present" = all(ccols_chr %in% names(cr)),
    "Date columns present" = all(ccols_dat %in% names(cr)),
    "Numerical columns contain numbers" = all(purrr::map_lgl(ccols_num, \(x) {
      is.numeric(cr[[x]])
    })),
    "Text columns contain text" = all(purrr::map_lgl(ccols_chr, \(x) {
      is.character(cr[[x]])
    })),
    "Date columns contain dates or numbers" = all(
      dates_as_dates != dates_as_numbers_or_text
    ),
    "Files column is present" = ("files" %in% names(cr)),
    "Files column is a list" = is.list(cr$files),
    "Files column contains NULLs or tibbles" = all(ff_status)
  )
  result <- status |> unlist() |> all()
  if ((result == FALSE) && verbose) {
    for (i in which(unlist(status) == FALSE)) {
      cli::cli_alert_info("Not TRUE: {names(status)[i]}")
    }
  }
  result
}
