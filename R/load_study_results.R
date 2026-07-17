#' Load Study Results from JATOS JSON
#'
#' Transforms raw JATOS study result data into a tidy tibble format,
#' handling nested component results and optional date parsing.
#' It expects the results were imported from JSON using
#' `jsonlite::fromJSON(json, simplifyVector = F)`.
#'
#' @param sr A list containing raw study result data from JATOS JSON.
#' @param parse_dates Logical. If `TRUE` (default), parses date columns to
#'   `POSIXct` (via `lubridate::as_datetime()`) and duration to
#'   `Duration` (via `lubridate::as.duration()`).
#'   If `FALSE`, keeps them as numeric or character.
#'
#' @return A single-row tibble with columns:
#'   - `id`: Study result ID (numeric)
#'   - `uuid`: Unique study result identifier (character)
#'   - `study_code`: Study code (character)
#'   - `start_date`: When the study started (POSIXct or numeric)
#'   - `end_date`: When the study ended (POSIXct or numeric)
#'   - `duration`: Total study duration (Duration or character)
#'   - `last_seen_date`: When participant was last active (POSIXct or numeric)
#'   - `study_state`: Current state (e.g., "FINISHED", "ABANDONED") (character)
#'   - `message`: Optional message (character)
#'   - `worker_id`: Worker/participant ID (numeric)
#'   - `worker_type`: Type of worker (character)
#'   - `batch_id`: Batch ID (numeric)
#'   - `batch_uuid`: Batch UUID (character)
#'   - `batch_title`: Human-readable batch title (character)
#'   - `group_id`: Group ID for group-based studies (numeric)
#'   - `is_quota_reached`: Whether quota was reached (numeric, 0/1)
#'   - `component_results`: List column containing a tibble of component results
#'     (as produced by [load_component_results()]), or `NULL` if no components
#'
#' @details
#' This function recursively processes nested component results by calling
#' [load_component_results()] on each component. Missing values in the input
#' are filled with `NA` values of the appropriate type.
#'
#' @seealso [load_component_results()] for details on component result processing.
#'
#' @examples
#' \dontrun{
#' # Load study results from JATOS JSON
#' study_result <- load_study_results(raw_study_data)
#' }
#'
#' @export
load_study_results <- function(sr, parse_dates = TRUE) {
  r <-
    tibble::tibble(
      id = sr$id %||% NA_integer_,
      uuid = sr$uuid %||% NA_character_,
      study_code = sr$studyCode %||% NA_character_,
      start_date = convert_date_conditional(sr$startDate, parse_dates),
      end_date = convert_date_conditional(sr$endDate, parse_dates),
      duration = convert_duration_conditional(sr$duration, parse_dates),
      last_seen_date = convert_date_conditional(sr$lastSeenDate, parse_dates),
      study_state = sr$studyState %||% NA_character_,
      message = sr$message %||% NA_character_,
      worker_id = sr$workerId %||% NA_integer_,
      worker_type = sr$workerType %||% NA_character_,
      batch_id = sr$batchId %||% NA_integer_,
      batch_uuid = sr$batchUuid %||% NA_character_,
      batch_title = sr$batchTitle %||% NA_character_,
      group_id = sr$groupId %||% NA_integer_,
      is_quota_reached = sr$isQuotaReached %||% NA_integer_,
      component_results = ifelse(
        length(sr$componentResults) == 0,
        list(NULL),
        purrr::map(
          sr$componentResults,
          \(x) load_component_results(x, parse_dates = parse_dates)
        ) |>
          dplyr::bind_rows() |>
          list()
      )
    )
  r
}
