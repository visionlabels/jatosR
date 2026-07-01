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
    as.POSIXct(timestamp_in_seconds, origin = "1970-01-01", tz = "UTC")
  # Convert the POSIXct date to a lubridate date
  lubridate::as_datetime(date_time)
}
