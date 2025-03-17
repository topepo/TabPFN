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
