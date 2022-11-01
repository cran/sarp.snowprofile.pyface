#' Check whether python and dependencies are available on system
#'
#' @return boolean TRUE/ FALSE
#' @export
have_dependencies <- function() {

  return(
    all(reticulate::py_available(),
        reticulate::py_module_available("scipy"),
        reticulate::py_module_available("joblib"),
        reticulate::py_module_available("numpy"),
        reticulate::py_module_available("pandas")
    )
  )
}
