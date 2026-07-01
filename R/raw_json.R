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
#' @param d Tibble with data obtained with `process_results`, or a list
#'   containing such a tibble in its `data` element. Errors if neither shape
#'   is provided.
#'
#' @return Tibble with new list column `data_json`, or the input list with
#'   its `data` tibble replaced by the same (see Details)
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
  stopifnot(
    "Package jsonlite must be installed" = requireNamespace(
      "jsonlite",
      quietly = TRUE
    )
  )
  if (tibble::is_tibble(d)) {
    result <-
      d |>
      dplyr::mutate(
        data_json = lapply(
          .data[["data_raw"]],
          function(x) jsonlite::parse_json(x)
        )
      )
    return(result)
  }
  if (
    is.list(d) &&
      !is.null(d[["data"]]) &&
      tibble::is_tibble(d[["data"]])
  ) {
    result <- d
    result$data <- result$data |> raw_json()
    return(result)
  }
  stop("Unsupported input type")
}
