#' Fit a `TabPFN` model.
#'
#' `tab_pfn()` fits a model.
#'
#' @param x Depending on the context:
#'
#'   * A __data frame__ of predictors.
#'   * A __matrix__ of predictors.
#'   * A __recipe__ specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' @param y When `x` is a __data frame__ or __matrix__, `y` is the outcome
#' specified as:
#'
#'   * A __data frame__ with 1 numeric column.
#'   * A __matrix__ with 1 numeric column.
#'   * A numeric __vector__ for regression or a __factor__ for classification.
#'
#' @param data When a __recipe__ or __formula__ is used, `data` is specified as:
#'
#'   * A __data frame__ containing both the predictors and the outcome.
#'
#' @param formula A formula specifying the outcome terms on the left-hand side,
#' and the predictor terms on the right-hand side.
#'
#' @param ignore_pretraining_limits A logical. The maximum number of features
#' 500 officially supported by the TabPFN python api. Set
#' `ignore_pretraining_limits` to `TRUE` to override.
#'
#' @param n_jobs The number of parallel process workers.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @details
#'
#' Predictors do not require preprocessing; missing values and factor vectors
#' are allowed.
#'
#' @return
#'
#' A `tab_pfn` object with elements:
#'
#'   * `fit`: the python object containing the model.
#'   * `levels`: a character string of class levels (or NULL for regression)
#'   * `training`: a vector with the training set dimensions.
#'   * `versions`: a list of python and python package versions and information.
#'   * `logging`: any R or python messages produced by the computations.
#'   * `blueprint`: am object produced by [hardhat::mold()] used to process
#'      new data during prediction.
#'
#' @references
#'
#' Hollmann, Noah, Samuel Müller, Lennart Purucker, Arjun Krishnakumar, Max
#' Körfer, Shi Bin Hoo, Robin Tibor Schirrmeister, and Frank Hutter.
#' "Accurate predictions on small data with a tabular foundation model."
#'  _Nature_ 637, no. 8045 (2025): 319-326.
#'
#' Hollmann, Noah, Samuel Müller, Katharina Eggensperger, and Frank Hutter.
#' "Tabpfn: A transformer that solves small tabular classification problems in
#' a second." _arXiv preprint_ arXiv:2207.01848 (2022).
#'
#' Müller, Samuel, Noah Hollmann, Sebastian Pineda Arango, Josif Grabocka, and
#' Frank Hutter. "Transformers can do bayesian inference." _arXiv preprint_
#' arXiv:2112.10510 (2021).
#'
#' @examples
#' predictors <- mtcars[, -1]
#' outcome <- mtcars[, 1]
#'
#' # XY interface
#' mod <- tab_pfn(predictors, outcome)
#'
#' # Formula interface
#' mod2 <- tab_pfn(mpg ~ ., mtcars)
#'
#' # Recipes interface
#' if (!rlang::is_installed("recipes")) {
#'  library(recipes)
#'  rec <-
#'   recipe(mpg ~ ., mtcars) %>%
#'   step_log(disp)
#'
#'  mod3 <- tab_pfn(rec, mtcars)
#'  mod3
#' }
#'
#' @export
tab_pfn <- function(x, ...) {
	UseMethod("tab_pfn")
}

#' @export
#' @rdname tab_pfn
tab_pfn.default <- function(x, ...) {
	cli::cli_abort("{.fn tab_pfn} is not defined for {obj_type_friendly(x)}.")
}

# XY method - data frame

#' @export
#' @rdname tab_pfn
tab_pfn.data.frame <- function(
	x,
	y,
	ignore_pretraining_limits = FALSE,
	n_jobs = 1L,
	...
) {
 options <- list(
  ignore_pretraining_limits = ignore_pretraining_limits,
  n_jobs = n_jobs)

	processed <- hardhat::mold(x, y)
	tab_pfn_bridge(processed, options, ...)
}

# XY method - matrix

#' @export
#' @rdname tab_pfn
tab_pfn.matrix <- function(
	x,
	y,
	ignore_pretraining_limits = FALSE,
	n_jobs = 1L,
	...
) {
 options <- list(
  ignore_pretraining_limits = ignore_pretraining_limits,
  n_jobs = n_jobs)

	processed <- hardhat::mold(x, y)
	tab_pfn_bridge(processed, options, ...)
}

# Formula method

#' @export
#' @rdname tab_pfn
tab_pfn.formula <- function(
	formula,
	data,
	ignore_pretraining_limits = FALSE,
	n_jobs = 1L,
	...
) {
 options <- list(
  ignore_pretraining_limits = ignore_pretraining_limits,
  n_jobs = n_jobs)

	# No not convert factors to indicators:
	bp <- hardhat::default_formula_blueprint(
		intercept = FALSE,
		allow_novel_levels = FALSE,
		indicators = "none",
		composition = "tibble"
	)
	processed <- hardhat::mold(formula, data, blueprint = bp)
	tab_pfn_bridge(processed, options, ...)
}

# Recipe method

#' @export
#' @rdname tab_pfn
tab_pfn.recipe <- function(
	x,
	data,
	ignore_pretraining_limits = FALSE,
	n_jobs = 1L,
	...
) {
 options <- list(
  ignore_pretraining_limits = ignore_pretraining_limits,
  n_jobs = n_jobs)

	processed <- hardhat::mold(x, data)
	tab_pfn_bridge(processed, options, ...)
}

# ------------------------------------------------------------------------------
# Bridge

tab_pfn_bridge <- function(processed, options, ...) {
 rlang::check_dots_empty()

	predictors <- processed$predictors
	outcome <- processed$outcomes[[1]]
	res <- tab_pfn_impl(predictors, outcome, options)

	new_tab_pfn(
		fit = res$fit,
		levels = res$lvls,
		training = res$train,
		versions = res$versions,
		logging = res$logging,
		blueprint = processed$blueprint
	)
}

# ------------------------------------------------------------------------------
# Implementation

tab_pfn_impl <- function(x, y, opts) {
	tabpfn <- reticulate::import("tabpfn")

	if (is.factor(y)) {
		mod_obj <- tabpfn$TabPFNClassifier(
			ignore_pretraining_limits = opts$ignore_pretraining_limits,
			n_jobs = opts$n_jobs
		)
	} else if (is.numeric(y)) {
		mod_obj <- tabpfn$TabPFNRegressor(
			ignore_pretraining_limits = opts$ignore_pretraining_limits,
			n_jobs = opts$n_jobs
		)
	}

	py_msg <- reticulate::py_capture_output(
		model_fit <- try(mod_obj$fit(x, y), silent = TRUE)
	)

	if (inherits(model_fit, "try-error")) {
		msgs <- as.character(model_fit)
		cli::cli_abort("Model failed: {msgs}")
	} else {
		msgs <- character(0)
	}

	# check for failures
	res <- list(
		fit = model_fit,
		lvls = levels(y),
		train = dim(x),
		versions = reticulate::py_config(), # will add package list back later
		logging = c(r = msgs, py = py_msg)
	)
	class(res) <- c("tab_pfn")
	res
}

#' @export
print.tab_pfn <- function(x, ...) {
	type <- ifelse(is.null(x$levels), "Regression", "Classification")
	cli::cli_inform("tab_pfn {type} Model")
	cat("\n")
	cli::cli_inform("Training set\n\n")
	cli::cli_inform(c(i = "{x$training[1]} data point{?s}"))
	cli::cli_inform(c(i = "{x$training[2]} predictor{?s}"))

	if (!is.null(x$levels)) {
		cli::cli_inform(c(i = "class levels: {.val {x$levels}}"))
	}

	invisible(x)
}
