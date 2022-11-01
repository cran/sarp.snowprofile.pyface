#' Import python module pyunstable
#'
#' Convenience wrapper to make python module pyunstable accessible in R session
#'
#' @return attach python module `pyunstable` to use python functions therein
#' @seealso [computePunstable]
#'
#' @export
import_pyunstable <- function() {
  pypath <- system.file("python", package = "sarp.snowprofile.pyface")
  utils::assignInMyNamespace(".pyunstable", reticulate::import_from_path("pyunstable", path = pypath))
}

#' Make RFmodel available for direct python calls
#'
#' Convenience wrapper to make the python random forest model for snow layer instability 'p_unstable' accessible in R session
#'
#' @return attach python RandomForestClassifier to variable `RFmodel`
#' @seealso [computePunstable]
#'
#' @export
import_RFmodel <- function() {
  utils::assignInMyNamespace(".pyjoblib", reticulate::import("joblib", delay_load = FALSE))
  pypath <- system.file("python", package = "sarp.snowprofile.pyface")
  utils::assignInMyNamespace(".RFmodel", .pyjoblib$load(paste0(pypath, "/RF_model_pub.sav")))
}
