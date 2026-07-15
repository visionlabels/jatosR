demo <- prepare_demo(nofile = TRUE)
txt <- demo$meta |> httr2::resp_body_string()
json <- jsonlite::fromJSON(txt)
sr_data <- json$data$studyResults[[1]]
cr_data <- json$data$studyResults[[1]]$componentResults
cr <- component_result_info(json$data[[1]]$studyResults[[1]]$componentResults[[
  1
]])
cr

load_component_results <- function(cr) {}
