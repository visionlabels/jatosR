#' Load Component Results from JATOS data JSON
#'
#' Transforms raw JATOS component result data into a tidy tibble format,
#' handling nested structures and optional date parsing.
#' It expects the results were imported from JSON using
#' `jsonlite::fromJSON(json, simplifyVector = F)`.
#'
#' @param cr A list containing raw component result data from JSON.
#' @param parse_dates Logical. If `TRUE` (default), parses date columns to
#'   `POSIXct` (via `lubridate::as_datetime()`) and duration to
#'   `Duration` (via `lubridate::as.duration()`).
#'   If `FALSE`, keeps them as numeric or character.
#'
#' @return A single-row tibble with columns:
#'   - `id`: Component result ID (numeric)
#'   - `component_id`: Component ID (numeric)
#'   - `component_uuid`: Unique component identifier (character)
#'   - `start_date`: Start time (POSIXct or numeric, depending on `parse_dates`)
#'   - `end_date`: End time (POSIXct or numeric, depending on `parse_dates`)
#'   - `duration`: Elapsed time (Duration or character, depending on `parse_dates`)
#'   - `component_state`: Component state/status (character)
#'   - `path`: Study path (character)
#'   - `data_size`: Data size in bytes (numeric)
#'   - `data_size_human_readable`: Formatted data size (character)
#'   - `files`: List column containing tibble of file data or `NULL`
#'   - `is_quota_reached`: Whether quota was reached (numeric, 0/1)
#'
#' @details
#' Missing values in the input are filled with `NA` values of the appropriate type.
#' File data is processed separately via [load_files_results()].
#'
#' @seealso [load_files_results()] for details on file processing.
#'
#' @examples
#' \dontrun{
#' # Load component results from JATOS JSON
#' demo <- prepare_demo(nofile = TRUE)
#' txt <- demo$meta |> httr2::resp_body_string()
#' json <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
#' cr_data <- json$data[[1]]$studyResults[[1]]$componentResults
#' r1 <- load_component_results(cr_data[[1]], parse_dates = TRUE)
#' r2 <- load_component_results(cr_data[[1]], parse_dates = FALSE)
#' is_valid_component_result(r1)
#' }
#'
#' @export
load_component_results <- function(cr, parse_dates = TRUE) {
  r <-
    tibble::tibble(
      id = cr$id %||% NA_integer_,
      component_id = cr$componentId %||% NA_integer_,
      component_uuid = cr$componentUuid %||% NA_character_,
      start_date = convert_date_conditional(cr$startDate, parse_dates),
      end_date = convert_date_conditional(cr$endDate, parse_dates),
      duration = convert_duration_conditional(cr$duration, parse_dates),
      component_state = cr$componentState %||% NA_character_,
      path = cr$path %||% NA_character_,
      data_size = cr$data$size %||% NA_integer_,
      data_size_human_readable = cr$data$sizeHumanReadable %||% NA_character_,
      files = list(load_files_results(cr$files)),
      is_quota_reached = cr$isQuotaReached %||% NA_integer_
    )
  r
}

#' Load Files Data from Component Results
#'
#' Converts raw file data from JATOS component results into a tidy tibble
#' with standardized column names.
#'
#' @param fr A list of file objects from JATOS component result data.
#'
#' @return A tibble with columns:
#'   - `filename`: Name of the file (character)
#'   - `size`: File size in bytes (numeric)
#'   - `size_human_readable`: Formatted file size (character)
#'
#'   Returns `NULL` if the input list is empty (no files).
#'
#' @details
#' This is a helper function called by [load_component_results()]. It processes
#' file data and combines multiple files into a single tibble using
#' [dplyr::bind_rows()].
#'
#' Missing values are filled with `NA` of appropriate type.
#'
#' @keywords internal
load_files_results <- function(fr) {
  if (length(fr) > 0) {
    flist <- list()
    for (i in seq_along(fr)) {
      flist[[i]] <- tibble::tibble(
        filename = fr[[i]]$filename %||% NA_character_,
        size = fr[[i]]$size %||% NA_integer_,
        size_human_readable = fr[[i]]$sizeHumanReadable %||% NA_character_
      )
    }
    dplyr::bind_rows(flist)
  } else {
    NULL
  }
}
