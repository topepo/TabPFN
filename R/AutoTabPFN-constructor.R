new_AutoTabPFN <- function(
	fit,
	levels,
	training,
	versions,
	logging,
	blueprint,
	call = NULL
) {
	cls <- c(
		"tabpfn_extensions.post_hoc_ensembles.sklearn_interface.AutoTabPFNRegressor",
		"tabpfn_extensions.post_hoc_ensembles.sklearn_interface.AutoTabPFNClassifier"
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
		class = "AutoTabPFN"
	)
}
