#' Fit a `TabPFN` model.
#'
#' `TabPFN()` fits a model.
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
#'   * A numeric __vector__.
#'
#' @param data When a __recipe__ or __formula__ is used, `data` is specified as:
#'
#'   * A __data frame__ containing both the predictors and the outcome.
#'
#' @param formula A formula specifying the outcome terms on the left-hand side,
#' and the predictor terms on the right-hand side.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @return
#'
#' A `TabPFN` object.
#'
#' @examples
#' predictors <- mtcars[, -1]
#' outcome <- mtcars[, 1]
#'
#' # XY interface
#' mod <- TabPFN(predictors, outcome)
#'
#' # Formula interface
#' mod2 <- TabPFN(mpg ~ ., mtcars)
#'
#' # Recipes interface
#' library(recipes)
#' rec <- recipe(mpg ~ ., mtcars)
#' rec <- step_log(rec, disp)
#' mod3 <- TabPFN(rec, mtcars)
#'
#' @export
TabPFN <- function(x, ...) {
	UseMethod("TabPFN")
}

#' @export
#' @rdname TabPFN
TabPFN.default <- function(x, ...) {
	cli::cli_abort("{.fn TabPFN} is not defined for {obj_type_friendly(x)}.")
}

# XY method - data frame

#' @export
#' @rdname TabPFN
TabPFN.data.frame <- function(x, y, ...) {
	processed <- hardhat::mold(x, y)
	TabPFN_bridge(processed, ...)
}

# XY method - matrix

#' @export
#' @rdname TabPFN
TabPFN.matrix <- function(x, y, ...) {
	processed <- hardhat::mold(x, y)
	TabPFN_bridge(processed, ...)
}

# Formula method

#' @export
#' @rdname TabPFN
TabPFN.formula <- function(formula, data, ...) {
	# No not convert factors to indicators:
	bp <- hardhat::default_formula_blueprint(
		intercept = FALSE,
		allow_novel_levels = FALSE,
		indicators = "none",
		composition = "tibble"
	)
	processed <- hardhat::mold(formula, data, blueprint = bp)
	TabPFN_bridge(processed, ...)
}

# Recipe method

#' @export
#' @rdname TabPFN
TabPFN.recipe <- function(x, data, ...) {
	processed <- hardhat::mold(x, data)
	TabPFN_bridge(processed, ...)
}

# ------------------------------------------------------------------------------
# Bridge

TabPFN_bridge <- function(processed, ...) {
	predictors <- processed$predictors
	outcome <- processed$outcomes[[1]]

	fit <- TabPFN_impl(predictors, outcome)

	new_TabPFN(
		fit = fit,
		blueprint = processed$blueprint
	)
}

# ------------------------------------------------------------------------------
# Implementation

TabPFN_impl <- function(x, y) {
	reticulate::source_python(system.file("functions.py", package = "TabPFN"))

	if (is.numeric(y)) {
		py_msg <- reticulate::py_capture_output(
			model_fit <- try(fit_reg(x, y), silent = TRUE)
		)
	} else {
		py_msg <- reticulate::py_capture_output(
			model_fit <- try(fit_cls(x, y), silent = TRUE)
		)
	}

	if (inherits(model_fit, "try-error")) {
		msgs <- as.character(model_fit)
		cli::cli_abort("Model failed: {msgs}")
	} else {
		msgs <- character(0)
	}

	# check for failures
	res <- list(
		fit = model_fit$fit,
		lvls = levels(y),
		train = dim(x),
		logging = c(r = msgs, py = py_msg)
	)
	class(res) <- c("tab_pfn")
	res
}

# TODO:
# figure out preprocessing
# call python functions using import("tabpfn")$...
# how to get python module versions
# print and predict methods
# flag for local versus server-side
