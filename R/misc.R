not_available_msg <- function(pkg) {
 c(
  x = cli::format_inline(
       "The {.pkg {pkg}} Python package is not installed in the discovered
       Python installation ({.file {reticulate::py_exe()}})."),
  i = cli::format_inline(
       'Allow reticulate to automatically configure an ephemeral Python
       environment by removing the Python installation from the order of
       discovery and restarting the R session. See
       {.href [Order of Discovery](https://rstudio.github.io/reticulate/dev/articles/versions.html#order-of-discovery)}
       for more info.'),
  i = cli::format_inline(
       'Or install {.pkg {pkg}} into the selected Python environment by calling
        {.code reticulate::py_install() for the package.}')
 )
}

