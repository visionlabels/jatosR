demo <- prepare_demo(nofile = TRUE)
txt <- demo$meta |> httr2::resp_body_string()
json <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
sr_data <- json$data[[1]]$studyResults

r1 <- load_study_results(sr_data[[1]], parse_dates = TRUE)
r2 <- load_study_results(sr_data[[1]], parse_dates = FALSE)

test_that("Study results as expected", {
  expect_equal(nrow(r1), 1)
  expect_equal(nrow(r2), 1)
  expect_equal(r1$id, 442488)
  expect_equal(r1$study_state, "FINISHED")
  expect_equal(r1$worker_id, 471179)
})

test_that("Self-test is working", {
  expect_true(is_valid_study_result(r1))
  expect_true(is_valid_study_result(dplyr::bind_rows(r1, r1)))
  expect_true(is_valid_study_result(r2))
})

test_that("Dates parsed correctly", {
  expect_true(is.numeric(r2[["start_date"]]))
  expect_true(is.numeric(r2[["end_date"]]))
  expect_true(is.numeric(r2[["last_seen_date"]]))
  expect_true(is.character(r2[["duration"]]))
  expect_true(lubridate::is.timepoint(r1[["start_date"]]))
  expect_true(lubridate::is.timepoint(r1[["end_date"]]))
  expect_true(lubridate::is.timepoint(r1[["last_seen_date"]]))
  expect_true(lubridate::is.duration(r1[["duration"]]))
})
