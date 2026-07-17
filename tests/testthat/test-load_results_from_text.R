demo <- prepare_demo(nofile = TRUE)
txt <- demo$meta |> httr2::resp_body_string()

r1 <- load_results_from_text(txt)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
