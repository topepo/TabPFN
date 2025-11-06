new_tab_pfn <- function(
  fit,
  levels,
  training,
  logging,
  blueprint,
  call = NULL
) {
  cls <- c(
    "tabpfn.regressor.TabPFNRegressor",
    "tabpfn.classifier.TabPFNClassifier"
  )

  if (!inherits(fit, cls)) {
    cli::cli_abort(
      "The model fit object should have class {.cls {.or {cls}}}, not
			{.cls {class(fit)}}.",
      call = call
    )
  }

  check_character(levels, allow_null = TRUE)

  hardhat::new_model(
    fit = fit,
    levels = levels,
    training = training,
    logging = logging,
    blueprint = blueprint,
    class = "tab_pfn"
  )
}
