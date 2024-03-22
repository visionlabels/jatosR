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
#' @format ## `who`
#' A list with two items:
#' \describe{
#'   \item{meta}{A list with metadata}
#'   \item{data}{A tibble with 4 rows and 26 columns. Raw data are in text format in the `raw` column.}
#' }
#' @source <https://github.com/visionlabels/jatosR>
"demo_srt_processed"
