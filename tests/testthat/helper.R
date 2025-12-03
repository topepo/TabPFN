run_tests <- function() {
  pkg <- asNamespace("tabpfn")
  src_pkg <- ls(envir = pkg, pattern = "^tabpfn$")
  tabpfn_avail <- identical(src_pkg, "tabpfn")

  # Are there any virtual envir?
  venv_list <- reticulate::virtualenv_list()

  run_tests <- tabpfn_avail & length(venv_list) > 0
  if (!run_tests) {
    return(FALSE)
  }

  correct_venv <- grepl("(reticulate)|(tabpfn)", venv_list)
  correct_venv
}

exp_cls <- c("tabpfn", "hardhat_model", "hardhat_scalar")

predictors <- mtcars[, -1]
outcome <- mtcars[, 1]
