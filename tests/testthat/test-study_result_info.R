demo <- prepare_demo(nofile = TRUE)
json <- demo$meta |> httr2::resp_body_json()
sr <- study_result_info(json$data[[1]]$studyResults[[1]])

test_that("Study results as expected", {
  expect_equal(sr$id_sr, 442488)
  expect_equal(sr$study_code, "aiJynkFlNJj")
  expect_equal(sr$study_state, "FINISHED")
  expect_equal(sr$batch_id, 15883)
  expect_equal(sr$worker_type, "GeneralMultiple")
  expect_true(is.na(sr$group_id))
})
