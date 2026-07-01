#' Simple Reaction Time Task data
#'
#' Simple reaction time study
#' ([Simple Reaction Time Task](https://github.com/JATOS/JATOS_examples/raw/main/examples/jspsych_7_simple_reaction_time_task.jzip)),
#' which is included in [JATOS examples](https://www.jatos.org/Example-Studies).
#' The example study was uploaded to MindProbe server and run four times.
#' You can play with the raw data yourself calling `prepare_demo` function.
#' The processed data are useful if you are interested
#' in the final format provided by jatosR package after calling `process_results`.
#'
#' @format ## `demo_srt_processed`
#' A list with two items:
#' \describe{
#'   \item{meta}{A list with metadata, as returned by `process_results`}
#'   \item{data}{A tibble with 4 rows and 26 columns. Raw data are in text format in the `data_raw` column.}
#' }
#' @source <https://github.com/visionlabels/jatosR>
"demo_srt_processed"

#' Simple Reaction Time Task raw response data
#'
#' Simple reaction time study
#' ([Simple Reaction Time Task](https://github.com/JATOS/JATOS_examples/raw/main/examples/jspsych_7_simple_reaction_time_task.jzip)),
#' which is included in [JATOS examples](https://www.jatos.org/Example-Studies).
#' The example study was uploaded to MindProbe server and run four times.
#' See `process_results` on how to turn the raw data into processed values.
#'
#' This is the raw response data with two `httr2_response` values, as used
#' by `prepare_demo()`.
#' @format ## `demo_srt`
#' A list with two items:
#' \describe{
#'   \item{meta}{`httr2_response` as returned by `get_metadata`}
#'   \item{results}{`httr2_response` as returned by `get_results`, with
#'   `body` referring to the bundled `srtdemo.zip` file,
#'   which you can copy into current working directory with `prepare_demo`}
#' }
#' @source <https://github.com/visionlabels/jatosR>
"demo_srt"
