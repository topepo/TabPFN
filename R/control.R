#' Controlling TabPFN execution
#'
#' @param n_preprocessing_jobs An integer for the number of worker processes.
#' A value of -1L indicates all possible resources.
#' @param device A character value for the device used for torch (e.g., `"cpu"`,
#' `"cuda"`, `"mps"`, etc.). Th default is `"auto"`.
#' @param ignore_pretraining_limits A logical to bypass the default data limits
#'   on:the number of training set samples (10,000) and, the number of
#'   predictors (500). There is an unchangeable limit to the number of classes
#'   (10).
#' @param inference_precision A character value for the trade off between speed
#' and reproducibility. This can be a torch `dtype`, `"autocast"` (for torch's
#' mixed-precision autocast), or "auto".
#' @param fit_mode A character value to control how the are preprocessed and/or
#' cached. Values are `"fit_preprocessors"` (the default), `"low_memory"`,
#' `"fit_with_cache"`, and `"batched"`.
#' @param memory_saving_mode A character string to help with out-of-memory
#' errors. Values are either a logical or `"auto"`.
#' @param random_state An integer to set the random number stream.
#' @references
#' \url{https://github.com/PriorLabs/TabPFN/blob/main/src/tabpfn/classifier.py},
#' \url{https://github.com/PriorLabs/TabPFN/blob/main/src/tabpfn/regressor.py}
#' @examples
#' control_tab_pfn()
#' @export
control_tab_pfn <- function(
  n_preprocessing_jobs = 1L,
  device = "auto",
  ignore_pretraining_limits = FALSE,
  inference_precision = "auto",
  fit_mode = "fit_preprocessors",
  memory_saving_mode = "auto",
  random_state = sample.int(10^6, 1)
) {
  check_bool(ignore_pretraining_limits)
  check_string(fit_mode)
  check_string(device)
  check_string(inference_precision)
  check_number_whole(n_preprocessing_jobs)
  check_number_whole(random_state)

  fit_mode <- rlang::arg_match(
    fit_mode,
    c("fit_preprocessors", "low_memory", "fit_with_cache", "batched")
  )

  mem_msg <- "{.arg memory_saving_mode} should be a single logical or string."
  if (length(memory_saving_mode) != 1) {
    cli::cli_abort(mem_msg)
  }
  if (!is.character(memory_saving_mode) & !is.logical(memory_saving_mode)) {
    cli::cli_abort(mem_msg)
  }

  args <- list(
    n_preprocessing_jobs = as.integer(n_preprocessing_jobs),
    device = device,
    ignore_pretraining_limits = ignore_pretraining_limits,
    inference_precision = inference_precision,
    fit_mode = fit_mode,
    memory_saving_mode = memory_saving_mode,
    random_state = as.integer(random_state)
  )

  class(args) <- "control_tab_pfn"
  args
}

#' @export
print.control_tab_pfn <- function(x, ...) {
  non_default <- purrr::map2_lgl(x, control_tab_pfn(), ~ !identical(.x, .y))

  cli::cli_inform("control object for {.fn tab_pfn}")
  if (any(non_default)) {
    cat("\n")
    cli::cli_inform("non-default arguments:")
    xsub <- x[non_default]
    lst <- purrr::map2(
      names(xsub),
      xsub,
      ~ cli::format_inline("{.arg {.x}}: {.val {.y}}")
    )
    names(lst) <- rep("*", length(lst))
    cli::cli_bullets(lst)
  }
  invisible(x)
}
