msg_tabpfn_not_available <- function(cnd) {
  c(
    x = "The {.pkg tabpfn} Python package is not installed in the discovered Python installation ({.file {reticulate::py_exe()}}).",
    i = 'Allow reticulate to automatically configure an ephemeral Python environment by
         removing the Python installation from the order of discovery and restarting the R session.
         See {.href [Order of Discovery](https://rstudio.github.io/reticulate/dev/articles/versions.html#order-of-discovery)} for more info.',
    # Or set env var {.code Sys.setenv("RETICULATE_USE_MANAGED_VENV" = "yes")}',
    i = 'Or install {.pkg tabpfn} into the selected Python environment by calling
        {.code reticulate::py_install("tabpfn")}'
  )
}

check_libomp <- function() {
  os_info <- Sys.info()['sysname']
  if (os_info != "Darwin") {
    return(invisible(NULL))
  }
  vm_types <- system(paste("vmmap", Sys.getpid()), intern = TRUE)
  has_libomp <- any(grepl("libomp", vm_types))

  if (has_libomp) {
    cli::cli_abort(
      c(
        i = "We believe that an existing package has loaded {.pkg OpenMP}.",
        x = "{.pkg PyTorch} was about to do the same and would cause a segmentation fault.",
        i = "See {.url https://github.com/topepo/TabPFN/issues/3}.",
        "!" = "Try running {.code reticulate::import('torch')} in a new R session prior to loading other packages.",
        call = NULL
      )
    )
  }
  invisible(NULL)
}

check_data_constraints <- function(x, y) {
  row_limits <- 10000
  col_limits <- 500
  cls_limits <- 10

  lvls <- levels(y)

  x_dims <- dim(x)
  if (x_dims[1] > row_limits) {
    cli::cli_warn(
      call = NULL,
      c(
        i = "There are {format(x_dims[1], big.mark = ',')} rows in the training set.",
        i = "TabPFN (version 2) is intended for training set sizes <= {row_limits}.",
        i = "Consider setting the option {.arg ignore_pretraining_limits} to {.val TRUE} or subset the training size."
      )
    )
  }
  if (x_dims[2] > col_limits) {
    cli::cli_warn(
      call = NULL,
      c(
        i = "There are {format(x_dims[2], big.mark = ',')} predictors in the training set.",
        i = "TabPFN (version 2) is intended for <= {col_limits} predictors.",
        i = "Consider setting the option {.arg ignore_pretraining_limits} to {.val TRUE} or subset the training size."
      )
    )
  }

  if (!is.null(lvls) && length(lvls) > cls_limits) {
    cli::cli_abort(
      call = NULL,
      c(
        i = "There are {length(lvls)} classes in the outcome.",
        x = "TabPFN (version 2) is intended for <= {cls_limits} classes and won't work with more."
      )
    )
  }

  invisible(NULL)
}

# Sampling down the data for data constraints

sample_indicies <- function(x, size_limit = 10000) {
  num_rows <- nrow(x)
  if (num_rows <= size_limit) {
    return(integer(0))
  }

  if (is.vector(x) | is.factor(x)) {
    x <- tibble::tibble(y = x)
  }

  if (is.matrix(x)) {
    x <- tibble::as_tibble(tibble, .name_repair = "minimal")
  }
  dat <-
    x |>
    dplyr::mutate(.row_order = dplyr::row_number()) |>
    dplyr::select({{ strata }}, .row_order)

  num_strata <- ncol(dat) - 1
  strata_names <- names(dat)[names(dat) != ".row_order"]
  if (num_strata > 1) {
    cli::cli_abort(
      "Only one strata column should be selected. The columns chosen were
    {.val {strata_names}}.",
      call = NULL
    )
  }
  is_factor <- is.factor(dat[[strata_names]])
  strata_quo <- rlang::enquo(strata_names)

  if (is_factor) {
    data_subset <-
      dat |>
      dplyr::group_by(dplyr::pick({{ strata_quo }})) |>
      dplyr::group_nest(keep = TRUE) |>
      dplyr::mutate(
        size = purrr::map_int(data, nrow),
        sample_prop = size / num_rows,
        sample_num = ceiling(sample_prop * size),
        data = purrr::map2(data, sample_num, ~ dplyr::slice_sample(.x, n = .y))
      )
  } else {
    data_subset <-
      dat |>
      rlang::set_names(c("strata", ".row_order")) |>
      dplyr::mutate(quantile = dplyr::ntile(strata, n = 4)) |>
      dplyr::group_by(quantile) |>
      dplyr::group_nest(keep = TRUE) |>
      dplyr::mutate(
        size = purrr::map_int(data, nrow),
        sample_prop = size / num_rows,
        sample_num = ceiling(sample_prop * size_limit),
        data = purrr::map2(data, sample_num, ~ dplyr::slice_sample(.x, n = .y))
      )
  }

  purrr::map_dfr(data_subset$data, ~.x) |>
    dplyr::arrange(.row_order) |>
    dplyr::select(.row_order) |>
    dplyr::slice(1:size_limit)
}
