test_that('control values', {
  skip_if(!run_tests())

  set.seed(822)
  ctrl <- control_tab_pfn(
    n_preprocessing_jobs = 2L,
    device = "cpu",
    ignore_pretraining_limits = TRUE,
    inference_precision = "float32",
    fit_mode = "low_memory",
    memory_saving_mode = TRUE,
    random_state = 12345L
  )

  expect_s3_class(ctrl, "control_tab_pfn")
  expect_equal(ctrl$n_preprocessing_jobs, 2L)
  expect_equal(ctrl$device, "cpu")
  expect_equal(ctrl$ignore_pretraining_limits, TRUE)
  expect_equal(ctrl$inference_precision, "float32")
  expect_equal(ctrl$fit_mode, "low_memory")
  expect_equal(ctrl$memory_saving_mode, TRUE)
  expect_equal(ctrl$random_state, 12345L)

  set.seed(822)
  expect_snapshot(ctrl)

  set.seed(822)
  expect_snapshot(control_tab_pfn(random_state = 1))
})
