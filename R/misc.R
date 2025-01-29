get_versions <- function(x) {
	pkgs <- reticulate::py_list_packages(type = "virtualenv")
	list(python = reticulate::py_config(), packages = tibble::as_tibble(pkgs))
}
