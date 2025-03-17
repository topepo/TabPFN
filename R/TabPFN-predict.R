#' Predict using `TabPFN`
#'
#' @param object,x A `TabPFN` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @return
#'
#' [predict()] returns a tibble of predictions and [augment()] appends the
#' columns in `new_data`. In either case, the number of rows in the tibble is
#' guaranteed to be the same as the number of rows in `new_data`.
#'
#' For regression data, the prediction is in the column `.pred`. For
#' classification, the class predictions are in `.pred_class` and the
#' probability estimates are in columns with the pattern `.pred_{level}` where
#' `level` is the levels of the outcome factor vector.
#'
#' @examples
#' car_train <- mtcars[ 1:20,   ]
#' car_test  <- mtcars[21:32, -1]
#'
#' # Fit
#' mod <- TabPFN(mpg ~ cyl + log(drat), car_train)
#'
#' # Predict, with preprocessing
#' predict(mod, car_test)
#' augment(mod, car_test)
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
		colnames(res) <- paste0(".pred_", object$classes_)
		cls_ind <- apply(res, 1, which.max)
		res <- tibble::as_tibble(res)
		# TabPFN will reorder the class levels; if the original factor has levels "b"
		# and "a", object$classes_ will have c("a", "b)
		res$.pred_class <- factor(object$classes_[cls_ind], levels = levels)
	}

	res
}

#' @export
#' @rdname predict.TabPFN
augment.TabPFN  <- function(x, new_data, ...) {
 new_data <- tibble::new_tibble(new_data)
 res <- predict(x, new_data)
 res <- cbind(res, new_data)
 tibble::new_tibble(res)
}
