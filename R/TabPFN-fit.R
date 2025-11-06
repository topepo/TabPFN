#' Fit a TabPFN model.
#'
#' `tab_pfn()` applies data to a pre-estimated deep learning model defined by
#' Hollmann _et al_ (2025). This model emulates Bayesian inference for
#' regression and classification models.
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
#' @param num_estimators An integer for the ensemble size. Default is `8L`.
#'
#' @param softmax_temperature An adjustment factor that is a divisor in the
#' exponents of the softmax function (see Details below). Defaults to 0.9.
#'
#' @param balance_probabilities A logical to adjust the prior probabilities in
#' cases where there is a class imbalance. Default is `FALSE`. Classification
#' only.
#'
#' @param average_before_softmax A logical. For cases where
#' `num_estimators > 1`, should the average be done before using the softmax
#' function or after? Default is `FALSE`.
#'
#' @param training_set_limit An integer greater than 2L (and possibly `Inf`)
#' that can be used to keep the training data within the limits of the
#' data constraints imposed by the Python library.
#'
#' @param control A list of options produced by [control_tab_pfn()].
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @details
#'
#' **Important Note**. Due to how Python uses the OpenMP library, it is
#' important that you load your virtual Python environment prior to loading any
#' R package that also uses OpenMP. If not, a segmentation fault can occur.
#' See [this GitHub issue](https://github.com/topepo/TabPFN/issues/3).
#'
#' Predictors do not require preprocessing; missing values and factor vectors
#' are allowed.
#'
#' For the `softmax_temperature` value, the softmax terms are:
#'
#' \preformatted{
#' exp(value / softmax_temperature)
#' }
#'
#' A value of `softmax_temperature = 1` results in a plain softmax value.
#'
#' @return
#'
#' A `tab_pfn` object with elements:
#'
#'   * `fit`: the python object containing the model.
#'   * `levels`: a character string of class levels (or NULL for regression)
#'   * `training`: a vector with the training set dimensions.
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
#' Frank Hutter. "Transformers can do Bayesian inference." _arXiv preprint_
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
#' if (rlang::is_installed("recipes")) {
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
  num_estimators = 8L,
  softmax_temperature = 0.9,
  balance_probabilities = FALSE,
  average_before_softmax = FALSE,
  training_set_limit = 10000,
  control = control_tab_pfn(),
  ...
) {
  options <- control
  options$n_estimators <- num_estimators
  options$softmax_temperature <- softmax_temperature
  options$balance_probabilities <- balance_probabilities
  options$average_before_softmax <- average_before_softmax
  options <- check_fit_args(options)
  check_number_whole(training_set_limit, min = 2, allow_infinite = TRUE)

  processed <- hardhat::mold(x, y)
  tr_ind <- sample_indicies(processed, size_limit = training_set_limit)
  if (length(tr_ind) > 0) {
    processed$predictors <- processed$predictors[tr_ind, , drop = FALSE]
    processed$outcomes <- processed$outcomes[tr_ind, , drop = FALSE]
  }

  tab_pfn_bridge(processed, options, ...)
}

# XY method - matrix

#' @export
#' @rdname tab_pfn
tab_pfn.matrix <- function(
  x,
  y,
  num_estimators = 8L,
  softmax_temperature = 0.9,
  balance_probabilities = FALSE,
  average_before_softmax = FALSE,
  training_set_limit = 10000,
  control = control_tab_pfn(),
  ...
) {
  options <- control
  options$n_estimators <- num_estimators
  options$softmax_temperature <- softmax_temperature
  options$balance_probabilities <- balance_probabilities
  options$average_before_softmax <- average_before_softmax
  options <- check_fit_args(options)
  check_number_whole(training_set_limit, min = 2, allow_infinite = TRUE)

  processed <- hardhat::mold(x, y)
  tr_ind <- sample_indicies(processed, size_limit = training_set_limit)
  if (length(tr_ind) > 0) {
    processed$predictors <- processed$predictors[tr_ind, , drop = FALSE]
    processed$outcomes <- processed$outcomes[tr_ind, , drop = FALSE]
  }

  tab_pfn_bridge(processed, options, ...)
}

# Formula method

#' @export
#' @rdname tab_pfn
tab_pfn.formula <- function(
  formula,
  data,
  num_estimators = 8L,
  softmax_temperature = 0.9,
  balance_probabilities = FALSE,
  average_before_softmax = FALSE,
  training_set_limit = 10000,
  control = control_tab_pfn(),
  ...
) {
  options <- control
  options$n_estimators <- num_estimators
  options$softmax_temperature <- softmax_temperature
  options$balance_probabilities <- balance_probabilities
  options$average_before_softmax <- average_before_softmax
  options <- check_fit_args(options)
  check_number_whole(training_set_limit, min = 2, allow_infinite = TRUE)

  # No not convert factors to indicators:
  bp <- hardhat::default_formula_blueprint(
    intercept = FALSE,
    allow_novel_levels = FALSE,
    indicators = "none",
    composition = "tibble"
  )
  processed <- hardhat::mold(formula, data, blueprint = bp)
  tr_ind <- sample_indicies(processed, size_limit = training_set_limit)
  if (length(tr_ind) > 0) {
    processed$predictors <- processed$predictors[tr_ind, , drop = FALSE]
    processed$outcomes <- processed$outcomes[tr_ind, , drop = FALSE]
  }

  tab_pfn_bridge(processed, options, ...)
}

# Recipe method

#' @export
#' @rdname tab_pfn
tab_pfn.recipe <- function(
  x,
  data,
  num_estimators = 8L,
  softmax_temperature = 0.9,
  balance_probabilities = FALSE,
  average_before_softmax = FALSE,
  training_set_limit = 10000,
  control = control_tab_pfn(),
  ...
) {
  options <- control
  options$n_estimators <- num_estimators
  options$softmax_temperature <- softmax_temperature
  options$balance_probabilities <- balance_probabilities
  options$average_before_softmax <- average_before_softmax
  options <- check_fit_args(options)
  check_number_whole(training_set_limit, min = 2, allow_infinite = TRUE)

  processed <- hardhat::mold(x, data)
  tr_ind <- sample_indicies(processed, size_limit = training_set_limit)
  if (length(tr_ind) > 0) {
    processed$predictors <- processed$predictors[tr_ind, , drop = FALSE]
    processed$outcomes <- processed$outcomes[tr_ind, , drop = FALSE]
  }

  tab_pfn_bridge(processed, options, ...)
}

# ------------------------------------------------------------------------------
# Bridge

tab_pfn_bridge <- function(processed, options, ...) {
  rlang::check_dots_empty()

  predictors <- processed$predictors
  outcome <- processed$outcomes[[1]]

  check_data_constraints(predictors, outcome, options)

  res <- tab_pfn_impl(predictors, outcome, options)

  new_tab_pfn(
    fit = res$fit,
    levels = res$lvls,
    training = res$train,
    logging = res$logging,
    blueprint = processed$blueprint
  )
}

# ------------------------------------------------------------------------------
# Implementation

tab_pfn_impl <- function(x, y, opts) {
  tabpfn <- reticulate::import("tabpfn")
  cls_wrapper <- function(...) {
    tabpfn$TabPFNClassifier(...)
  }
  reg_wrapper <- function(...) {
    tabpfn$TabPFNRegressor(...)
  }

  if (is.factor(y)) {
    cls_cl <- rlang::call2("cls_wrapper", !!!opts)
    mod_obj <- rlang::eval_bare(cls_cl)
  } else if (is.numeric(y)) {
    opts <- opts[names(opts) != "balance_probabilities"]
    cls_cl <- rlang::call2("reg_wrapper", !!!opts)
    mod_obj <- rlang::eval_bare(cls_cl)
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
    logging = c(r = msgs, py = py_msg)
  )
  class(res) <- c("tab_pfn")
  res
}

#' @export
print.tab_pfn <- function(x, ...) {
  type <- ifelse(is.null(x$levels), "Regression", "Classification")
  cli::cli_inform("TabPFN {type} Model")
  cat("\n")
  cli::cli_inform("Training set\n\n")
  cli::cli_inform(c(i = "{x$training[1]} data point{?s}"))
  cli::cli_inform(c(i = "{x$training[2]} predictor{?s}"))

  if (!is.null(x$levels)) {
    cli::cli_inform(c(i = "class levels: {.val {x$levels}}"))
  }

  invisible(x)
}

check_fit_args <- function(opts, call = rlang::caller_env()) {
  check_number_whole(
    # These arg names are deliberately different
    opts$n_estimators,
    arg = "num_estimators",
    min = 1,
    call = call
  )
  opts$n_estimators <- as.integer(opts$n_estimators)

  check_number_decimal(
    opts$softmax_temperature,
    arg = "softmax_temperature",
    min = .Machine$double.eps,
    call = call
  )

  check_logical(
    opts$balance_probabilities,
    arg = "balance_probabilities",
    call = call
  )

  check_logical(
    opts$average_before_softmax,
    arg = "average_before_softmax",
    call = call
  )

  # ------------------------------------------------------------------------------
  # There have been some argument name differences in the python package versions

  arg_names <- names(opts)
  py_lib <- reticulate::import("tabpfn")
  py_arg_names <- names(formals(py_lib$TabPFNClassifier))
  if (any(py_arg_names == "n_jobs")) {
    names(opts) <- gsub("^n_preprocessing_jobs$", "n_jobs", names(opts))
  }

  # ------------------------------------------------------------------------------

  opts
}
