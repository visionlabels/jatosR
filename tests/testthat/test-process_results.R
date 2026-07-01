demo <- prepare_demo()
r <- process_results(demo$meta, demo$results)
r
clean_demo()

test_that("Results as expected", {
  expect_true(all(c("meta", "data") %in% names(r)))
  expect_equal(nrow(r$data), 4)
})
