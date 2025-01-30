get_versions <- function(x) {
	pkgs <- reticulate::py_list_packages(type = "virtualenv")
	list(python = reticulate::py_config(), packages = tibble::as_tibble(pkgs))
}

check_py_packages <- function() {
 res <- TabPFN:::get_versions()

 if (!any(res$packages$package == "tabpfn")) {
  cli::cli_abort(
   c(
    x = "The {.pkg tabpfn} python package is not installed at
    {.file {res$python$python}}.",
    i = 'It can be installed using
    {.code reticulate::py_require(c("numpy", "tabpfn"), python_version = "<3.12")}',
    i = "or use a different virtual environment."
    )
  )
 }

}
