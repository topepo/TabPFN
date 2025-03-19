#' Predict using `AutoTabPFN`
#'
#' @param object,x A `AutoTabPFN` object.
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
#' \dontrun{
#' car_train <- mtcars[ 1:20,   ]
#' car_test  <- mtcars[21:32, -1]
#'
#' # Fit
#' mod <- AutoTabPFN(mpg ~ cyl + log(drat), car_train, device = "cpu")
#'
#' # Predict, with preprocessing
#' predict(mod, car_test)
#' augment(mod, car_test)
#' }
#'
#' @export
predict.AutoTabPFN <- function(object, new_data, ...) {
	rlang::check_dots_empty()
	forged <- hardhat::forge(new_data, object$blueprint)$predictors
	res <- predict(object$fit, forged, object$levels)
	res
}

# ------------------------------------------------------------------------------
# Implementation

#' @export
predict.tabpfn_extensions.post_hoc_ensembles.sklearn_interface.AutoTabPFNRegressor <- function(
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
predict.tabpfn_extensions.post_hoc_ensembles.sklearn_interface.AutoTabPFNClassifier <- function(
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

#' @export
#' @rdname predict.AutoTabPFN
augment.AutoTabPFN  <- function(x, new_data, ...) {
 new_data <- tibble::new_tibble(new_data)
 res <- predict(x, new_data)
 res <- cbind(res, new_data)
 tibble::new_tibble(res)
}
