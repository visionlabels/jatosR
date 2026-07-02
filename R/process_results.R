#' Turns metadata and data into a single table
#'
#' @details
#' Combines the metadata returned by `get_metadata` with the raw data
#' downloaded by `get_results`: the zip file referenced in `results$body` is
#' unzipped into a temporary folder, `data.txt` for each study/component
#' result is read into memory, and the temporary folder is removed again.
#'
#' If `results` is `NULL` or its `body` isn't a downloaded file (i.e.
#' `get_results` wasn't called, or was called without a `filename`/default
#' temporary file), a warning is issued and `data_raw` is left as `NA` for
#' every row instead of erroring.
#'
#' @param meta Metadata from `get_metadata` function
#' @param results Results from `get_results` function, or `NULL` to process
#'   metadata only, leaving `data_raw` as `NA`.
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
process_results <- function(meta, results) {
  res <- process_metadata_from_text(meta |> httr2::resp_body_string())

  # extract data files
  res$data <- res$data |> dplyr::mutate(data_raw = NA_character_)

  if (is.null(results) || !isa(results$body, "httr2_path")) {
    cli::cli_alert_warning("No results provided, data_raw will be set to NA's.")
    return(res)
  }

  zipfile <- results$body
  tmpfolder <- file.path(tempdir(), "zipped_data")
  utils::unzip(zipfile, exdir = tmpfolder)

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
