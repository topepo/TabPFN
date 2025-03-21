tabpfn <- NULL

.onLoad <- function(...) {
	reticulate::py_require("tabpfn")

	tryCatch(
		tabpfn <<- reticulate::import(
			"tabpfn",
			delay_load = list(
				on_error = function(e) {
					cli::cli_abort(msg_tabpfn_not_available(e))
				},
				# See https://github.com/topepo/TabPFN/issues/3
				before_load = function() {
					check_libomp()
				}
			)
		),

		# if reticulate has already loaded symbols from a Python installation,
		# `reticulate::import(delay_load = TRUE)` will error immediately.
		python.builtin.ModuleNotFoundError = function(e) {
			cli::cli_warn(msg_tabpfn_not_available(e))
		}
	)
}
