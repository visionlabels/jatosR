#' Turns JATOS metadata JSON into a single table
#'
#' Parses JATOS metadata (the `studyResults`/`componentResults` tree) shared
#' by `process_results` and `process_results_from_file`, regardless of
#' whether it came from a live API response or a local export. Only the
#' metadata is processed here; neither function adds the actual response
#' data (column `data_raw`) until after this returns.
#'
#' @param txt JSON metadata as text, a URL, or a path to a JSON file — passed
#'   through to `jsonlite::fromJSON()`, which accepts any of those forms
#'
#' @return list with
#' \describe{
#'  \item{`meta`}{General info like study ID or API version}
#'  \item{`data`}{Tibble with all meta data, one row per study/component result}
#' }
#' @export
#'
#' @examples
#' fn <- system.file("extdata", "srtdemo_jatos_results.zip", package = "jatosR")
#' tmpfolder <- tempfile()
#' utils::unzip(fn, exdir = tmpfolder)
#' r <- process_metadata_from_text(file.path(tmpfolder, "metadata.json"))
#' r
process_metadata_from_text <- function(txt) {
  mj <- jsonlite::fromJSON(txt)

  minfo <- list(
    api_version = mj$apiVersion %||% NA_character_,
    # data_node_count = length(mj$data),
    study_id = mj$data$studyId,
    study_uuid = mj$data$studyUuid,
    study_title = mj$data$studyTitle,
    study_results_node_count = nrow(mj$data$studyResults[[1]])
  )

  sres <-
    mj$data$studyResults |>
    dplyr::bind_rows() |>
    dplyr::mutate(xid = .data$id) |>
    tidyr::nest(.by = "xid") |>
    dplyr::mutate(
      study_results = purrr::map(
        .data$data,
        \(x) study_result_info(x) |> dplyr::as_tibble()
      )
    ) |>
    dplyr::mutate(
      component_results = purrr::map(.data$data, \(x) {
        x$componentResults |>
          purrr::map(\(x) {
            component_result_info(x) |>
              dplyr::as_tibble()
          }) |>
          dplyr::bind_rows()
      })
    )

  res <- list(
    meta = minfo,
    data = sres |>
      tidyr::unnest("study_results") |>
      tidyr::unnest("component_results") |>
      dplyr::select(-"xid", -"data")
  )
  res
}
