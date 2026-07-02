#' Turns metadata and data into a single table, from a local results file
#'
#' Like `process_results`, but reads both metadata and raw data from a single
#' zip archive already downloaded to disk (e.g. exported from the JATOS web
#' interface), instead of combining separate `get_metadata`/`get_results` API
#' responses. The archive is expected to contain a top-level `metadata.json`
#' plus a data folder per component result, mirroring the layout produced by
#' JATOS's own results export.
#'
#' @param path Path to the zip file with results and metadata
#'
#' @return list with
#' \describe{
#'  \item{`meta`}{General info like study ID or study title}
#'  \item{`data`}{Tibble with all meta data, column `data_raw` stores the results for each entry in text format. This can be parsed for example with `jsonlite` or `jspsychread` packages}
#' }
#' @export
#'
#' @examples
#' fn <- system.file("extdata", "srtdemo_jatos_results.zip", package = "jatosR")
#' r <- process_results_from_file(fn)
#' r
process_results_from_file <- function(path) {
  tmpfolder <- file.path(tempdir(), "zipped_data")
  utils::unzip(path, exdir = tmpfolder)
  # the content was extracted to tmpfolder (directly, no other subfolder in between)

  res <- process_metadata_from_text(file.path(tmpfolder, "metadata.json"))

  # extract data files
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
