#' Predict from a `TabPFN`
#'
#' @param object A `TabPFN` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param type A single character. The type of predictions to generate.
#' Valid options are:
#'
#' - `"numeric"` for numeric predictions.
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
	forged <- hardhat::forge(new_data, object$blueprint)
	predict_TabPFN_bridge(object, forged$predictors)
}

valid_TabPFN_predict_types <- function() {
	c("numeric")
}

# ------------------------------------------------------------------------------
# Bridge

predict_TabPFN_bridge <- function(model, predictors) {
	predictors <- as.matrix(predictors)

	predictions <- predict_function(model, predictors)

	hardhat::validate_prediction_size(predictions, predictors)

	predictions
}

# ------------------------------------------------------------------------------
# Implementation

predict_TabPFN_numeric <- function(model, predictors) {
	predictions <- rep(1L, times = nrow(predictors))
	hardhat::spruce_numeric(predictions)
}
