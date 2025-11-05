run_tests <- function() {
  pkg <- asNamespace("TabPFN")
  src_pkg <- ls(envir = pkg, pattern = "^tabpfn$")
  tabpfn_avail <- identical(src_pkg, "tabpfn")
  tabpfn_avail
}

exp_cls <- c("TabPFN", "hardhat_model", "hardhat_scalar")

predictors <- mtcars[, -1]
outcome <- mtcars[, 1]
