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
