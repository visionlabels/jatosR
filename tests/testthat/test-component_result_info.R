demo <- prepare_demo(nofile = TRUE)
json <- demo$meta |> httr2::resp_body_json()
cr <- component_result_info(
  json$data[[1]]$studyResults[[1]]$componentResults[[1]]
)
cr
clean_demo()

test_that("Component results as expected", {
  expect_equal(cr$id_cr, 605082)
  expect_equal(cr$component_id, 21204)
  expect_equal(cr$data_path, "/study_result_442488/comp-result_605082")
  expect_equal(cr$data_size, 5775)
  expect_true(is.na(cr$data_filename))
})
