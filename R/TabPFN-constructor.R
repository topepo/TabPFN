new_TabPFN <- function(
	fit,
	levels,
	training,
	versions,
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
		versions = versions,
		logging = logging,
		blueprint = blueprint,
		class = "TabPFN"
	)
}
