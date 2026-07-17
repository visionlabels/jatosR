#' Load JATOS Results from JSON Text
#'
#' Parses JATOS study results from a JSON text string and transforms them into
#' a tidy tibble format. This is the primary entry point for importing JATOS data.
#'
#' @param txt A character string containing JATOS results in JSON format.
#'   Typically obtained from a JATOS API response or JSON file export.
#' @param parse_dates Logical. If `TRUE` (default), parses date columns to
#'   `POSIXct` (via `lubridate::as_datetime()`) and duration to
#'   `Duration` (via `lubridate::as.duration()`).
#'   If `FALSE`, keeps them as numeric or character.
#'
#' @return A single-row tibble with columns:
#'   - `api_version`: JATOS API version (character)
#'   - `study_id`: Study ID (numeric)
#'   - `study_uuid`: Study UUID (character)
#'   - `study_title`: Human-readable study title (character)
#'   - `study_results`: List column containing a tibble of study results
#'     (as produced by [load_study_results()]), or `NULL` if no results
#'
#' @details
#' This function is typically the first step in a data import pipeline:
#' 1. Parse JSON from text via `jsonlite::fromJSON()`
#' 2. Extract study metadata (id, uuid, title)
#' 3. Recursively process nested study results via [load_study_results()]
#' 4. Return a single-row tibble with list columns containing nested data
#'
#' The resulting structure allows for further processing and analysis of all
#' study and component results in a tidy, relational format.
#'
#' @seealso [load_study_results()] for details on study result processing;
#' [is_valid_component_result()], [is_valid_study_result()] for validation.
#'
#' @examples
#' \dontrun{
#' # Load results from JSON file
#' json_text <- readLines("study_results.json") |> paste(collapse = "")
#' results <- load_results_from_text(json_text)
#'
#' # Or from an API response
#' response <- httr2::request("https://jatos.server/api/...") |>
#'   httr2::req_perform()
#' results <- load_results_from_text(httr2::resp_body_string(response))
#' }
#'
#' @export
load_results_from_text <- function(txt, parse_dates = TRUE) {
  mj <- jsonlite::fromJSON(txt, simplifyVector = F)

  rr <- tibble::tibble(
    api_version = mj$apiVersion %||% NA_character_,
    study_id = mj$data[[1]]$studyId,
    study_uuid = mj$data[[1]]$studyUuid,
    study_title = mj$data[[1]]$studyTitle,
    study_results = ifelse(
      length(mj$data[[1]]$studyResults) == 0,
      list(NULL),
      purrr::map(
        mj$data[[1]]$studyResults,
        \(x) load_study_results(x, parse_dates = parse_dates)
      ) |>
        dplyr::bind_rows() |>
        list()
    )
  )
  rr
}
