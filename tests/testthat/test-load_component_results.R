demo <- prepare_demo(nofile = TRUE)
txt <- demo$meta |> httr2::resp_body_string()
json <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
sr_data <- json$data[[1]]$studyResults
cr_data <- json$data[[1]]$studyResults[[1]]$componentResults

r1 <- load_component_results(cr_data[[1]], parse_dates = TRUE)
r2 <- load_component_results(cr_data[[1]], parse_dates = FALSE)

ff <- cr_data[[1]]
ff$files <- list(list(
  filename = "file.txt",
  size = 0,
  sizeHumanReadable = "0 B"
))
rfiles <- load_component_results(ff)

ccols <- c(
  "id",
  "component_id",
  "component_uuid",
  "start_date",
  "end_date",
  "duration",
  "component_state",
  "path",
  "data_size",
  "data_size_human_readable",
  "files",
  "is_quota_reached"
)
fcols <- c("filename", "size", "size_human_readable")

test_that("Component results as expected", {
  expect_equal(nrow(r1), 1)
  expect_equal(nrow(rfiles), 1)
  expect_equal(r1$id, 605082)
  expect_equal(r1$component_id, 21204)
  expect_equal(r1$path, "/study_result_442488/comp-result_605082")
  expect_equal(r1$data_size, 5775)
  expect_true(is.null(r1$files[[1]]))
  expect_false(is.null(rfiles$files[[1]]))
  f1 <- rfiles$files[[1]]
  expect_true(tibble::is_tibble(f1))
  expect_true(all(fcols %in% colnames(f1)))
})

test_that("Expected columns present", {
  expect_true(all(ccols %in% colnames(r1)))
  expect_true(all(ccols %in% colnames(r2)))
  expect_true(is.numeric(r1[["id"]]))
  expect_true(is.numeric(r1[["component_id"]]))
  expect_true(is.character(r1[["component_uuid"]]))
  expect_true(is.character(r1[["component_state"]]))
  expect_true(is.character(r1[["path"]]))
  expect_true(is.numeric(r1[["data_size"]]))
  expect_true(is.character(r1[["data_size_human_readable"]]))
  expect_true(is.numeric(r1[["is_quota_reached"]]))
})

test_that("Self-test is working", {
  expect_true(is_valid_component_result(r1))
  expect_true(is_valid_component_result(r2))
  expect_true(is_valid_component_result(rfiles))
  expect_true(is_valid_component_result(dplyr::bind_rows(r1, rfiles)))
})

test_that("Dates parsed correctly", {
  expect_true(is.numeric(r2[["start_date"]]))
  expect_true(is.numeric(r2[["end_date"]]))
  expect_true(is.character(r2[["duration"]]))
  expect_true(lubridate::is.timepoint(r1[["start_date"]]))
  expect_true(lubridate::is.timepoint(r1[["end_date"]]))
  expect_true(lubridate::is.duration(r1[["duration"]]))
})
