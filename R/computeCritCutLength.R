#' Compute critical crack length
#'
#' This function implements Bettina Richter's (2019) parametrization for the critical crack length for flat simulations based on
#' density, grain size, and shear strength. The parametrization also needs the mean density of the slab, which can be computed
#' automatically if a snowprofile object is provided. In case the functions gets a snowprofileLayers object it expects `slab_rho` being precomputed.
#' This acts as a safety mechanism to ensure that `slab_rho` is computed over one profile and not over a stacked layers data.frame containing multiple
#' profiles. Note that the critical crack length can be computed alongside the layer probabilities for instability `p_unstable` in [computePunstable].
#'
#' @param x [snowprofileSet], [snowprofile], or [snowprofileLayers] object
#' @return Input object is returned with `$crit_cut_length` (and potentially `$slab_rho`) appended to the layers object.
#'
#' @references Richter, B., Schweizer, J., Rotach, M. W., & Van Herwijnen, A. (2019).
#' Validating modeled critical crack length for crack propagation in the snow cover model SNOWPACK.
#' The Cryosphere, 13(12), 3353–3366. https://doi.org/10.5194/tc-13-3353-2019
#'
#' @author fherla based on the python function by smayer and brichter
#' @seealso [computePunstable]
#' @export
computeCritCutLength <- function(x) UseMethod("computeCritCutLength")

#' @describeIn computeCritCutLength for [snowprofileSet]s
#' @export
computeCritCutLength.snowprofileSet <- function(x) {
  return(snowprofileSet(lapply(x, computeCritCutLength.snowprofile)))
}

#' @describeIn computeCritCutLength for [snowprofile]s
#' @export
computeCritCutLength.snowprofile <- function(x) {
  if (!"slab_rho" %in% names(x$layers)) x <- computeSLABrho(x)
  x$layers$crit_cut_length <- computeCritCutLength.snowprofileLayers(x$layers)$crit_cut_length
  return(x)
}

#' @describeIn computeCritCutLength for [snowprofileLayers]
#' @export
computeCritCutLength.snowprofileLayers <- function(x) {

  ## --- Initialization and assertion ----
  reqprops <- c("density", "gsize", "shear_strength", "slab_rho")
  missingprops <- which(!reqprops %in% names(x))
  if (length(missingprops) > 0) {
    if ("slab_rho" %in% reqprops[missingprops]) message("slab_rho needs to be precomputed if computeCritCutLength is called with a snowprofileLayers object!")
    stop(paste("Missing critical layer properties to compute crit_cut_length:", paste0(reqprops[missingprops], collapse = ", ")))
  }

  ## --- compute rc flat ----
  ## define constants
  rho_ice <- 917. # [g m-3]
  gs_0 <- 0.00125 # [m]
  a <- 4.6e-9
  b <- -2

  ## Eprime = E' = E/(1-nu**2) ; poisson ratio nu=0.2
  eprime <- 5.07e9*(x$slab_rho/rho_ice)**5.13 / (1-0.2**2)
  ## D_sl/sigma_n = D_sl / (rho_sl*9.81*D_sl) = 1/(9.81*rho_sl)
  dsl_over_sigman <- 1 / (9.81 * x$slab_rho)
  ## parametrization from Richter (2019):
  x$crit_cut_length <- round(
    sqrt(a*(x$density/rho_ice * x$gsize*0.001/gs_0)**b) * sqrt(2*x$shear_strength*1000 * eprime*dsl_over_sigman),  # gsize [mm] -> [m]; shear_strength [kPa] -> [Pa]
    digits = 2)

  return(x)
}
