#' Predict from a `TabPFN`
#'
#' @param object A `TabPFN` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @examples
#' train <- mtcars[1:20,]
#' test <- mtcars[21:32, -1]
#'
#' # Fit
#' mod <- TabPFN(mpg ~ cyl + log(drat), train)
#'
#' # Predict, with preprocessing
#' predict(mod, test)
#'
#' @export
predict.TabPFN <- function(object, new_data, ...) {
	rlang::check_dots_empty()
	forged <- hardhat::forge(new_data, object$blueprint)$predictors
	res <- predict(object$fit, forged, object$levels)
	res
}

# ------------------------------------------------------------------------------
# Implementation

#' @export
predict.tabpfn.regressor.TabPFNRegressor <- function(
	object,
	new_data,
	levels,
	...
) {
	py_msg <- reticulate::py_capture_output(
		res <- try(object$predict(new_data), silent = TRUE)
	)

	if (inherits(res, "try-error")) {
		msgs <- as.character(res)
		cli::cli_abort("Prediction failed: {msgs}")
	} else {
		res <- tibble::tibble(.pred = as.vector(res))
	}

	res
}

#' @export
predict.tabpfn.classifier.TabPFNClassifier <- function(
	object,
	new_data,
	levels,
	...
) {
	py_msg <- reticulate::py_capture_output(
		res <- try(object$predict_proba(new_data), silent = TRUE)
	)

	if (inherits(res, "try-error")) {
		msgs <- as.character(res)
		cli::cli_abort("Prediction failed: {msgs}")
	} else {
		colnames(res) <- paste0(".pred_", levels)
		cls_ind <- apply(res, 1, which.max)
		res <- tibble::as_tibble(res)
		res$.pred_class <- levels[cls_ind]
	}

	res
}
