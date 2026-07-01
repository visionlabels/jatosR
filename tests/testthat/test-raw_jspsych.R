demo <- prepare_demo()
r <- process_results(demo$meta, demo$results)
rdata <- r$data |> raw_jspsych()
# rdata |> dplyr::select(start_date_sr, data_jspsych)
rj <- rdata$data_jspsych[[1]]
clean_demo()

test_that("raw_jspsych works", {
  expect_equal(nrow(rdata), 4)
  expect_equal(nrow(rj), 24)
  expect_equal(rj$trial_type[2], "html-keyboard-response")
})
