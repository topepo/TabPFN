tabpfn <- NULL
autotabpfn <- NULL

.onLoad <- function(...) {

 reticulate::py_require(
   packages = c(
     "tabpfn",
     paste0("tabpfn-extensions[post_hoc_ensembles,interpretability,hpo]",
            "@",
            "git+https://github.com/PriorLabs/tabpfn-extensions.git")
     ),
   # Python version constraint required by tabpfn-extensions
   # https://github.com/PriorLabs/tabpfn-extensions/issues/45
   python_version = "<3.10"
 )

  tryCatch(
    tabpfn <<- reticulate::import(
      "tabpfn",
      delay_load = list(
        before_load = function() {
          if (is_macOS()) {
            patch_libllvmlite()
          }
        },
        on_error = function(e) {
         msg <- not_available_msg("tabpfn")
         cli::cli_abort(msg)
        }
      )
    ),

    # if reticulate has already loaded symbols from a Python installation,
    # `reticulate::import(delay_load = TRUE)` will error immediately.
    python.builtin.ModuleNotFoundError = function(e) {
     msg <- not_available_msg("tabpfn")
     cli::cli_abort(msg)
    }
  )

 tryCatch(
  autotabpfn <<- reticulate::import(
   "tabpfn_extensions.post_hoc_ensembles.sklearn_interface",
   delay_load = list(
     before_load = function() {
       # force the 'tabpfn' lazy module to load and run the patch in the
       # before_load() hook
       tabpfn$`__version__`
     },
    on_error = function(e) {
     msg <- not_available_msg("tabpfn_extensions.post_hoc_ensembles.sklearn_interface")
     cli::cli_abort(msg)
    }
   )
  ),

  python.builtin.ModuleNotFoundError = function(e) {
   msg <- not_available_msg("tabpfn_extensions.post_hoc_ensembles.sklearn_interface")
   cli::cli_abort(msg)
  }
 )
}


patch_libllvmlite <- function(venv_prefix = reticulate::import("sys")$prefix) {
  libllvmlite.dylib <- unique(normalizePath(list.files(
    venv_prefix,
    pattern = "libllvmlite\\.dylib$",
    recursive = TRUE,
    full.names = TRUE
  )))
  for (lib in libllvmlite.dylib)
    suppressWarnings(system2(
      "install_name_tool",
      c("-add_rpath /usr/lib", shQuote(lib)),
      stderr = FALSE
    ))
}


is_macOS <- function() {
  Sys.info()[["sysname"]] == "Darwin"
}
