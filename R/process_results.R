#' Turns metadata and data into a single table
#'
#' @details
#' Combines the metadata returned by `get_metadata` with the raw data
#' downloaded by `get_results`: the zip file referenced in `results$body` is
#' unzipped into a temporary folder, `data.txt` for each study/component
#' result is read into memory, and the temporary folder is removed again.
#'
#' @param meta Metadata from `get_metadata` function
#' @param results Results from `get_results` function. `results$body` must
#'   be a path to a downloaded file (i.e. `get_results` was called with a
#'   `filename`, or its default temporary file).
#' @param data_node Index of the study data node to use. Defaults to `1`;
#'   change it if more than one data node is present in `meta`.
#' @param component_node Index of the componentResults node to use within
#'   each study result. Defaults to `1`; change it if more than one
#'   componentResults node is present in your study.
#'
#' @return list with
#' \describe{
#'  \item{`meta`}{General info like study ID or API version}
#'  \item{`data`}{Tibble with all meta data, column `data_raw` stores the results for each entry in text format. This can be parsed for example with `jsonlite` or `jspsychread` packages}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' cc <- define_connection(
#'   "https://www.myjatosinstance.org",
#'   "Bearer jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"
#' )
#' meta <- get_metadata(cc, batch_id = 100)
#' results <- get_results(cc, batch_id = 100)
#' r <- process_results(meta, results)
#' }
process_results <- function(meta, results, data_node = 1, component_node = 1) {
  # metadata
  mj <- meta |> httr2::resp_body_json()
  dn <- mj$data[[data_node]]

  minfo <- list(
    api_version = mj$apiVersion,
    data_node_count = length(mj$data),
    study_id = dn$studyId,
    study_uuid = dn$studyUuid,
    study_title = dn$studyTitle,
    study_results_node_count = length(dn$studyResults)
  )

  sres <-
    dn$studyResults |>
    purrr::map(\(x) study_result_info(x) |> dplyr::as_tibble()) |>
    dplyr::bind_rows()
  stopifnot(nrow(sres) == minfo$study_results_node_count)

  comp <-
    dn$studyResults |>
    purrr::map(
      \(x) component_result_info(x$componentResults[[component_node]]) |> dplyr::as_tibble()
    ) |>
    dplyr::bind_rows()
  stopifnot(nrow(comp) == minfo$study_results_node_count)

  res <- list(
    meta = minfo,
    data = dplyr::bind_cols(sres, comp)
  )

  # extract data files
  stopifnot(
    "File not included in the results" = (class(results$body) == "httr2_path")
  )
  zipfile <- results$body
  tmpfolder <- file.path(tempdir(), "zipped_data")
  utils::unzip(zipfile, exdir = tmpfolder)

  res$data <- res$data |> dplyr::mutate(data_raw = NA_character_)
  for (i in 1:nrow(res$data)) {
    fn <- file.path(
      tmpfolder,
      res$data$data_path[i],
      "data.txt"
    )
    if (file.exists(fn)) {
      res$data$data_raw[i] <-
        readr::read_file(fn)
    }
  }

  # cleanup
  unlink(tmpfolder, recursive = TRUE)

  res
}
