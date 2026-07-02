fn <- system.file("extdata", "srtdemo_jatos_results.zip", package = "jatosR")

r <- process_results_from_file(fn)

test_that("Results from file as expected", {
  expect_true(all(c("meta", "data") %in% names(r)))
  expect_equal(nrow(r$data), 4)
})
