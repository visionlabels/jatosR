demo <- prepare_demo(nofile = TRUE)
txt <- demo$meta |> httr2::resp_body_string()

r1 <- load_results_from_text(txt)

test_that("Self-test is working", {
  expect_true(is_valid_result(r1))
})
