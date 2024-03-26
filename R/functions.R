#' Define JATOS connection
#'
#' @param url Base URL to the JATOS instance (with no API endpoints)
#' @param token API token obtained from JATOS instance
#'
#' Check https://www.jatos.org/JATOS-API.html#personal-access-tokens
#' about the process of getting a token.
#'
#' You don't need to worry about trailing slash in URL
#' (it is automatically removed). If you omit "Bearer " part
#' of the token, but the token seems to be valid, because it starts
#' with "jap", the "Bearer " part is added.
#'
#' @return A simple list with url and token values to be used in other functions
#' @export
#'
#' @examples
#' \dontrun{
#' cc <- define_connection(
#'   "https://www.myjatosinstance.org",
#'   "Bearer jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"
#' )
#' test_connection(cc)
#' }
define_connection <- function(url, token) {
  # remove trailing slash (all endpoints start with /)
  url <- stringr::str_remove(url, "/*$")
  # if only japXXX part provided, add "Bearer " at start
  if (stringr::str_starts(token, "jap")) {
    token <- stringr::str_c("Bearer ", token)
  }
  stopifnot(
    "Bad token format" =
      stringr::str_starts(token, "Bearer jap")
  )
  list(url = url, token = token)
}

#' URL setup helper
#'
#' @param jc JATOS connection info, e.g. from `define_connection`
#' @param endpoint API endpoint address
#'
#' @return URL address created by matching the connection url and endpoint string.
#' @export
#'
#' @examples
#' \dontrun{
#' cc <- define_connection(
#'   "https://www.myjatosinstance.org",
#'   "Bearer jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"
#' )
#' # API for token info
#' api_url(cc, "/jatos/api/v1/admin/token")
#' }
api_url <- function(jc, endpoint) {
  stringr::str_c(jc$url, endpoint)
}

#' Basic JATOS HTTP request helper
#'
#' @param jc JATOS connection info, e.g. from `define_connection`
#' @param endpoint API endpoint address
#'
#' @return `httr2::request` object
#' @export
#'
#' @examples
#' \dontrun{
#' cc <- define_connection(
#'   "https://www.myjatosinstance.org",
#'   "Bearer jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"
#' )
#'
#' req <- base_request(cc, "/jatos/api/v1/admin/token")
#' }
base_request <- function(jc, endpoint) {
  req_base <-
    httr2::request(api_url(jc, endpoint)) |>
    httr2::req_headers(Authorization = jc$token)
  req_base
}

#' Test JATOS connection
#'
#' @param jc JATOS connection info, e.g. from `define_connection`
#'
#' @return HTTP response object returned by `httr2::req_perform()`
#' @export
#'
#' @examples
#' \dontrun{
#' cc <- define_connection(
#'   "https://www.myjatosinstance.org",
#'   "Bearer jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"
#' )
#' test_connection(cc)
#' }
test_connection <- function(jc) {
  req_base <- base_request(jc, "/jatos/api/v1/admin/token")
  resp <- req_base |> httr2::req_perform()
  resp
}

#' Get metadata for JATOS results
#'
#' @param jc JATOS connection info, e.g. from `define_connection`
#' @param batch_id Integer identifier for a particular experiment batch
#' @param component_id Integer identifier for a particular experiment component
#'
#' Only one of batch_id or component_id should be provided.
#'
#' @return HTTP response object returned by `httr2::req_perform()`. The actual metadata are stored in the response `body` and can be analyzed with `process_results`. The provided `batch_id` and `component_id` are added to the response data.
#' @export
#'
#' @examples
#' \dontrun{
#' cc <- define_connection(
#'   "https://www.myjatosinstance.org",
#'   "Bearer jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"
#' )
#' meta <- get_metadata(cc, batch_id = 100)
#' meta$batch_id
#' }
get_metadata <- function(jc, batch_id = NULL, component_id = NULL) {
  req_base <- base_request(jc, "/jatos/api/v1/results/metadata")
  stopifnot(
    "Either batch_id or component_id must be provided" =
      !(is.null(batch_id) && is.null(component_id))
  )
  stopifnot(
    "Only one of batch_id or component_id must be provided" =
      (is.null(batch_id) || is.null(component_id))
  )
  if (!is.null(batch_id)) {
    req_results <-
      req_base |>
      httr2::req_url_query(batchId = batch_id)
  }
  if (!is.null(component_id)) {
    req_results <-
      req_base |>
      httr2::req_url_query(componentId = component_id)
  }
  resp_results <- req_results |> httr2::req_perform()
  resp_results$batch_id <- batch_id
  resp_results$component_id <- component_id
  resp_results
}


#' Get raw data for JATOS results
#'
#' @param jc JATOS connection info, e.g. from `define_connection`
#' @param batch_id Integer identifier for a particular experiment batch
#' @param component_id Integer identifier for a particular experiment component
#' @param filename Optional name of the file where the data should be stored. If not provided, temporary file is created in system folder for temporary files
#'
#' @return HTTP response object returned by `httr2::req_perform()`. The data are stored in the provided file and can be analyzed with `process_results`. The provided `batch_id` and `component_id` are added to the response data.
#' @export
#'
#' @examples
#' \dontrun{
#' cc <- define_connection(
#'   "https://www.myjatosinstance.org",
#'   "Bearer jap_xXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXxXx"
#' )
#' results <- get_results(cc, batch_id = 100)
#' results$batch_id
#' # Name of the temporary file with the zipped data
#' results$body
#' }
get_results <- function(jc,
                        batch_id = NULL,
                        component_id = NULL,
                        filename = NULL) {
  req_base <- base_request(jc, "/jatos/api/v1/results/data")
  stopifnot(
    "Either batch_id or component_id must be provided" =
      !(is.null(batch_id) && is.null(component_id))
  )
  stopifnot(
    "Only one of batch_id or component_id must be provided" =
      (is.null(batch_id) || is.null(component_id))
  )
  if (!is.null(batch_id)) {
    req_results <-
      req_base |>
      httr2::req_url_query(batchId = batch_id)
  }
  if (!is.null(component_id)) {
    req_results <-
      req_base |>
      httr2::req_url_query(componentId = component_id)
  }
  if (!is.null(filename)) {
    folder <- dirname(filename)
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
  } else {
    filename <- tempfile()
  }
  resp_results <- req_results |> httr2::req_perform(path = filename)
  resp_results$batch_id <- batch_id
  resp_results$component_id <- component_id
  resp_results
}

#' Date conversion helper
#'
#' @param date_integer Numerical format of times in JATOS metadata
#'
#' @return Lubridate date
#' @export
#'
#' @examples
#' convert_date(1642617959901)
convert_date <- function(date_integer) {
  timestamp_in_seconds <- date_integer / 1000
  # Convert the Unix timestamp to a POSIXct date-time format
  date_time <-
    as.POSIXct(timestamp_in_seconds,
      origin = "1970-01-01", tz = "UTC"
    )
  # Convert the POSIXct date to a lubridate date
  lubridate::as_datetime(date_time)
}

#' Parser of studyResults data
#'
#' @param sr list corresponding to the studyResults part of the data
#'
#' @return List of size-one vectors, which could be converted to tibble if needed.
#' @export
#'
#' @examples
#' demo <- prepare_demo(nofile = TRUE)
#' json <- demo$meta |> httr2::resp_body_json()
#' sr <- study_result_info(json$data[[1]]$studyResults[[1]])
#' sr
study_result_info <- function(sr) {
  r <-
    list(
      id_sr = sr$id %||% NA_integer_,
      uuid = sr$uuid %||% NA_character_,
      study_code = sr$studyCode %||% NA_character_,
      start_date_sr = convert_date(sr$startDate %||% NA_integer_),
      end_date_sr = convert_date(sr$endDate %||% NA_integer_),
      duration_sr = sr$duration %||% NA_character_,
      last_seen_date = convert_date(sr$lastSeenDate %||% NA_integer_),
      study_state = sr$studyState %||% NA_character_,
      # url_query_parameters = sr$urlQueryParameters,
      worker_id = sr$workerId %||% NA_integer_,
      worker_type = sr$workerType %||% NA_character_,
      batch_id = sr$batchId %||% NA_integer_,
      batch_uuid = sr$batchUuid %||% NA_integer_,
      batch_title = sr$batchTitle %||% NA_character_,
      group_id = sr$groupId %||% NA_integer_,
      component_results_node_count = length(sr$componentResults)
    )
  r$duration_sr <- r$end_date_sr - r$start_date_sr
  r
}

#' Parser of componentResults data
#'
#' @param cr list corresponding to the componentResults part of the data
#'
#' @return List of size-one vectors, which could be converted to tibble if needed.
#' @export
#'
#' @examples
#' demo <- prepare_demo(nofile = TRUE)
#' json <- demo$meta |> httr2::resp_body_json()
#' cr <- component_result_info(
#'   json$data[[1]]$studyResults[[1]]$componentResults[[1]]
#' )
#' cr
component_result_info <- function(cr) {
  r <-
    list(
      id_cr = cr$id %||% NA_integer_,
      component_id = cr$componentId %||% NA_integer_,
      component_uuid = cr$componentUuid %||% NA_character_,
      start_date_cr = convert_date(cr$startDate %||% NA_integer_),
      end_date_cr = convert_date(cr$endDate %||% NA_integer_),
      duration_cr = cr$duration %||% NA_character_,
      component_state = cr$componentState %||% NA_character_,
      data_path = cr$path %||% NA_character_,
      data_filename = cr$data$filename %||% NA_character_,
      data_size = cr$data$size %||% NA_integer_
    )
  r$duration_cr <- r$end_date_cr - r$start_date_cr
  r
}

#' Turns metadata and data into a single table
#'
#' @param meta Metadata from `get_metadata` function
#' @param results Results from `get_results` function. It should link to an external file with zipped data.
#' @param data_node Default 1. You can change it if more data nodes are present in your study.
#' @param component_node Default 1. You can change it if more componentResults nodes are present in your study.
#'
#' @return list with
#' \itemize{
#'  \item{`meta`}{General info like study ID or API version}
#'  \item{`data`}{Tibble with all meta data, column `raw` stores the results for each entry in text format. This can be parsed for example with `jsonlite` or `jspsychread` packages}
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
process_results <- function(meta, results,
                            data_node = 1, component_node = 1) {
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
      \(x) component_result_info(x$componentResults[[1]]) |> dplyr::as_tibble()
    ) |>
    dplyr::bind_rows()
  stopifnot(nrow(comp) == minfo$study_results_node_count)

  res <- list(
    meta = minfo,
    data = dplyr::bind_cols(sres, comp)
  )

  # extract data files
  stopifnot(
    "File not included in the results" =
      (class(results$body) == "httr2_path")
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

#' Simple Response Time demo
#'
#' @param nofile Default `FALSE`. Use `TRUE` if you want to load only the metadata, without creating the local file.
#'
#' @return list with `meta` and `results`, which could be used in `process_results`. It also copies file `srt_demo.zip` in the working directory. After `process_results`, you can delete it, or use `clen_demo()`.
#' @export
#'
#' @examples
#' demo <- prepare_demo()
#' r <- process_results(demo$meta, demo$results)
#' r
#' clean_demo()
prepare_demo <- function(nofile = F) {
  if (!nofile) {
    file.copy(
      system.file("extdata", "srtdemo.zip", package = "jatosR"),
      getwd()
    )
  }
  demo_srt
}

#' Demo clean up
#'
#' Cleans temporary file after `prepare_demo()`.
#'
#' @return Nothing
#' @export
#'
#' @examples
#' demo <- prepare_demo()
#' r <- process_results(demo$meta, demo$results)
#' r
#' clean_demo()
clean_demo <- function() {
  fn <- file.path(getwd(), "srtdemo.zip")
  if (file.exists(fn)) unlink(fn)
}

#' Transforms raw data to JSON list
#'
#' Takes raw results stored in `data_raw` and adds new column `data_json` containing data in lists created by `jsonlite` package.
#' The package `jsonlite` must be installed for this function to work.
#' It is expected that the parameter `d` is the dataset obtained with `process_results`,
#' but the dataset can be simpler, because only the column `data_raw` is used.
#'
#' Function `process_results` returns a list (let's assume called `results`),
#' `raw_json` is intended to be used in tibble pipeline, i.e. on the `result$data` tibble.
#' To simplify the process, the function detects, whether it is provided
#' with a list containing a tibble called `data`. In that case,
#' it processes the `data` tibble, puts it back into the original list and returns it.
#'
#' @param d Tibble with data obtained with `process_results`
#'
#' @return Tibble with new list column `data_json` or a list with `data` tibble (see above)
#' @export
#'
#' @examples
#' \dontrun{
#' demo <- prepare_demo()
#' r <- process_results(demo$meta, demo$results)
#' # working with tibbles
#' rdata <- r$data |> raw_json()
#' # shortcut for the results list also works
#' r <- r |> raw_json()
#' clean_demo()
#' }
raw_json <- function(d) {
  stopifnot("Package jsonline must be installed" = requireNamespace("jsonlite", quietly = TRUE))
  if (tibble::is_tibble(d)) {
    result <-
      d |>
      dplyr::mutate(
        data_json =
          lapply(
            .data[["data_raw"]],
            function(x) jsonlite::parse_json(x)
          )
      )
    return(result)
  }
  if (is.list(d) &&
      !is.null(d[["data"]]) &&
      tibble::is_tibble(d[["data"]])) {
    result <- d
    result$data <- result$data |> raw_json()
    return(result)
  }

}
