.onLoad <- function(...) {
	reticulate::py_require(c("numpy", "tabpfn"), python_version = "<3.12")
}
