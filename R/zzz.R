## Hidden objects of package's own namespace
.pyjoblib <- NULL
.pyunstable <- NULL
.RFmodel <- NULL

.onLoad <- function(libname, pkgname) {

  ## configure dependency requirements of python modules
  reticulate::configure_environment(pkgname)

  ## prepare required python modules for delayed package loading
  utils::assignInMyNamespace(".pyjoblib", reticulate::import("joblib", delay_load = TRUE))
  pypath <- system.file("python", package = "sarp.snowprofile.pyface")
  utils::assignInMyNamespace(".pyunstable", reticulate::import_from_path("pyunstable", path = pypath, delay_load = TRUE))

}
