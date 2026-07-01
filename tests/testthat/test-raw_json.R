demo <- prepare_demo()
r <- process_results(demo$meta, demo$results)
rdata <- r$data |> raw_json()
rj <- rdata$data_json[[1]]
clean_demo()

test_that("raw_json works", {
  expect_equal(nrow(rdata), 4)
  expect_equal(length(rj), 24)
  expect_equal(rj[[24]]$trial_type, "html-keyboard-response")
})
