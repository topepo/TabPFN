test_that('data constraints', {
  skip_if(!run_tests())
  skip_on_cran()
  skip_if_not_installed("modeldata")

  set.seed(418)
  orig_data <- tab_pfn(
    Class ~ .,
    data = modeldata::two_class_dat,
    num_estimators = 1,
  )

  expect_equal(orig_data$training[1], nrow(modeldata::two_class_dat))

  reps <- rep(1:nrow(modeldata::two_class_dat), each = 13)

  set.seed(418)
  smaller_data <- tab_pfn(
    Class ~ .,
    data = modeldata::two_class_dat[reps, ],
    num_estimators = 1,
    control = control_tab_pfn(ignore_pretraining_limits = TRUE)
  )

  expect_equal(smaller_data$training[1], 10000L)

  set.seed(418)
  larger_data <- tab_pfn(
    Class ~ .,
    data = modeldata::two_class_dat[reps, ],
    num_estimators = 1,
    training_set_limit = Inf,
    control = control_tab_pfn(ignore_pretraining_limits = TRUE)
  )

  expect_equal(larger_data$training[1], length(reps))

  expect_snapshot_error({
    set.seed(418)
    tab_pfn(
      Class ~ .,
      data = modeldata::two_class_dat[reps, ],
      num_estimators = 1,
      training_set_limit = Inf
    )
  })
})
