#' Compute probability of layer instability based on random forest model
#'
#' This function enables comfortable and fast R access to Stephanie Mayer's python implementation of her random forest model to estimate the probability of dry snow layer instability.
#' The routine can be run very efficiently on large [snowprofileSet]s. Layer properties required are sphericity, viscous deformation rate (10e-6 s-1), density (kg m-3), grain size (mm),
#' and the critical crack length (m) (which can be computed very efficiently automatically if shear strength (kPA) is available.) Additionally, skier penetration depth in (m) is required.
#'
#' @param x [snowprofile], [snowprofileSet], or [snowprofileLayers]
#' @param ski_pen skier penetration depth (m), one scalar for each profile in x
#' @param recompute_crit_cut_length This routine can very efficiently compute the critical crack length with [computeCritCutLength].
#' SNOWPACK often provides NA values of the critical crack length even for layers that have a real solution to it. With this flag you can conveniently
#' recompute all critical crack lengths (`TRUE`). If set to `FALSE`, it will only be computed if not all profiles already contain it.
#' Note that shear strength must be available to compute the critical crack length!
#' @param ... passed on to subsequent methods
#' @param buffer internal switch to ensure fast computation at low memory cost. Leave at `TRUE`!
#'
#' @return x is returned with `$p_unstable` (and potentially `$crit_cut_length`, `$slab_rho`, and `slab_rhogs`) appended to each profile's layers object.
#' @author fherla and smayer
#'
#' @references Mayer, S., Herwijnen, A. Van, Techel, F., & Schweizer, J. (accepted, 2022).
#' A random forest model to assess snow instability from simulated snow stratigraphy.
#' The Cryosphere Discussions. https://doi.org/10.5194/tc-2022-34
#'
#' @examples
#' ## load a handful of example profiles from a PRO file
#' profiles <- snowprofilePro(system.file("extdata/snowprofile.pro",
#'                                        package = "sarp.snowprofile.pyface"),
#'                            remove_soil = TRUE, suppressWarnings = TRUE)
#' summary(profiles)
#' names(profiles[[1]]$layers)
#' ## compute p_unstable alongside critical crack length, slab_rho, slab_rhogs:
#' if (have_dependencies()) {
#' profiles <- computePunstable(profiles)
#' names(profiles[[1]]$layers)
#' }
#'
#'
#' @export
computePunstable <- function(x, ...) {

  ## ---load RFmodel----
  if (is.null(.RFmodel) || is.null(.pyunstable)) {
    import_RFmodel()
    import_pyunstable()
  }
  UseMethod("computePunstable")
}


#' @describeIn computePunstable for [sarp.snowprofile::snowprofileSet]s
#' @export
computePunstable.snowprofileSet <- function(x, ski_pen = NA, recompute_crit_cut_length = TRUE, buffer = TRUE, ...) {

  ## ensure crit_cut_length
  if (recompute_crit_cut_length) doCritCutLength <- TRUE
  else {
    if (!all(sapply(x, function(sp) "crit_cut_length" %in% names(sp)))) doCritCutLength <- TRUE
    else doCritCutLength <- FALSE
  }

  ## FAST implementation for prebuffered snowprofileSet length
  if (!buffer) {
    ### assert and assemble features ###
    ## prepare slab_rho if appropriate
    if (doCritCutLength) {
      x <- lapply(x, computeSLABrho)
    }
    ## compute slab_rhogs
    x <- lapply(x, computeSLABrhogs)

    ## bind into large data.frame
    ## dependent on whether ski_pen provided or not
    names_x <- names(x)
    id_x <- seq(length(x))  # id for later profile separation
    if (all(is.na(ski_pen))) {
      if (!all(sapply(x, function(sp) "ski_pen" %in% names(sp)))) stop("No ski_pen provided, and none available in at least one profile!")
      lyrs <- data.table::rbindlist(lapply(id_x, function(i) {
        data.frame(x[[i]]$layers, list(id = i, ski_pen = x[[i]]$ski_pen))
      }), use.names = TRUE, fill = TRUE)
    } else {
      if (!length(ski_pen) == length(id_x)) stop("ski_pen must be of same length as x (snowprofileSet)!")
      lyrs <- data.table::rbindlist(lapply(id_x, function(i) {
        data.frame(x[[i]]$layers, list(id = i, ski_pen = ski_pen[i]))
      }), use.names = TRUE, fill = TRUE)
    }
    ## ensure gsize
    if (!"gsize" %in% names(lyrs)) {
      if ("gsize_avg" %in% names(lyrs)) lyrs$gsize <- lyrs$gsize_avg
      if ("gsize_max" %in% names(lyrs)) lyrs$gsize <- lyrs$gsize_max
      if (!"gsize" %in% names(lyrs)) stop("No gsize available in profile!")
    }
    ## ensure v_strain_rate and sphericity
    if (!all(c("v_strain_rate", "sphericity") %in% names(lyrs))) stop("No v_strain_rate and/or sphericity available in profile layers!")

    ## finally compute crit_cut_length if appropriate
    if (doCritCutLength) {
      class(lyrs) <- append("snowprofileLayers", class(lyrs))
      lyrs$crit_cut_length <- tryCatch(computeCritCutLength(lyrs)$crit_cut_length, error = function(err) {
        message(err)
        if (!"shear_strength" %in% names(lyrs)) stop("No crit_cut_length available, and computing it is impossible since shear_strength is not available either!")
        else stop("No crit_cut_length available, and computing it failed!")
      })
    }
    ## final unit checks
    vsr_median <- median(lyrs$v_strain_rate, na.rm = TRUE)
    if (vsr_median > 0) stop("Looks like your v_strain_rate is positive instead of negative!")
    if (-vsr_median < 10**(-4) && -vsr_median > 0) warning(paste0("Looks like your v_strain_rate is in units (1/s) instead of (10**-6/s)! Please check manually and recompute in case!\nv_strain_rate: ",
                                                                  paste0(lyrs$v_strain_rate, collapse = ", ")))
    if (median(lyrs$ski_pen, na.rm = TRUE) > 2) stop("Looks like your ski_pen is in units (cm) instead of (m)!")

    ## compute p_unstable
    lyrs$p_unstable <- computePunstable.snowprofileLayers(lyrs)$p_unstable
    ## reassemble snowprofileSet from stacked snowprofileLayers
    x <- snowprofileSet(lapply(id_x, function(i) {
      x[[i]]$layers[, c("crit_cut_length", "p_unstable")] <- lyrs[lyrs$id == i, c("crit_cut_length", "p_unstable")]
      x[[i]]
    }))
    names(x) <- names_x

  ## LOOP over buffered subsets of snowprofileSet
  } else {
    chunksize <- 300  # requires max 0.5--1 GB of additional RAM
    nSP <- length(x)
    if (all(is.na(ski_pen))) {
      for (chunkstep in seq(ceiling(nSP/chunksize))) {
        chunkidx <- ((chunkstep-1)*chunksize + 1):min(((chunkstep-1)*chunksize + chunksize), nSP)
        x[chunkidx] <- computePunstable.snowprofileSet(x[chunkidx], recompute_crit_cut_length = doCritCutLength, buffer = FALSE)
      }
    } else {
      for (chunkstep in seq(ceiling(nSP/chunksize))) {
        chunkidx <- ((chunkstep-1)*chunksize + 1):min(((chunkstep-1)*chunksize + chunksize), nSP)
        ski_pen_chunk <- ski_pen[chunkidx]
        x[chunkidx] <- computePunstable.snowprofileSet(x[chunkidx], recompute_crit_cut_length = doCritCutLength, ski_pen = ski_pen_chunk, buffer = FALSE)
      }
    }
  }
  return(x)
}



#' @describeIn computePunstable for [snowprofile]s
#' @export
computePunstable.snowprofile <- function(x, ski_pen = NA, recompute_crit_cut_length = TRUE, ...) {

  ### assert and assemble features ###
  ## ensure v_strain_rate and sphericity
  if (!all(c("v_strain_rate", "sphericity") %in% names(x$layers))) stop("No v_strain_rate and/or sphericity available in profile layers!")

  ## ensure ski_pen
  if (is.na(ski_pen)) {
    ski_pen <- x$ski_pen
    if (is.na(ski_pen)) stop("No ski_pen provided, and none available in profile!")
  }

  ## unit checks
  vsr_median <- median(x$layers$v_strain_rate, na.rm = TRUE)
  if (vsr_median > 0) stop("Looks like your v_strain_rate is positive instead of negative!")
  if (-vsr_median < 10**(-4) && -vsr_median > 0) warning(paste0("Looks like your v_strain_rate is in units (1/s) instead of (10**-6/s)! Please check manually and recompute in case!\nv_strain_rate: ",
                                                                paste0(x$layers$v_strain_rate, collapse = ", ")))
  if (ski_pen > 2) stop("Looks like your ski_pen is in units (cm) instead of (m)!")

  ## ensure gsize
  if (!"gsize" %in% names(x$layers)) {
    if ("gsize_avg" %in% names(x$layers)) x$layers$gsize <- x$layers$gsize_avg
    if ("gsize_max" %in% names(x$layers)) x$layers$gsize <- x$layers$gsize_max
    if (!"gsize" %in% names(x$layers)) stop("No gsize available in profile!")
  }
  ## ensure slab_rhogs
  if (!"slab_rhogs" %in% names(x$layers)) {
    x$layers$slab_rhogs <- tryCatch(computeSLABrhogs(x, implementation = "pub")$layers$slab_rhogs, error = function(err) {
      message(err)
      stop("No slab_rhogs available, and computing it failed!")
    })
  }
  ## ensure crit_cut_length
  if (!"crit_cut_length" %in% names(x$layers)) {
    x$layers$crit_cut_length <- tryCatch(computeCritCutLength.snowprofileLayers(x$layers)$crit_cut_length, error = function(err) {
      message(err)
      if (!"shear_strength" %in% names(x$layers)) stop("No crit_cut_length available, and computing it is impossible since shear_strength is not available either!")
      else stop("No crit_cut_length available, and computing it failed!")
    })
  }

  ### call subsequent method ###
  x$layers$p_unstable <- computePunstable.snowprofileLayers(x$layers, ski_pen)$p_unstable
  return(x)
}




#' @describeIn computePunstable for [snowprofileLayers]
#' @export
computePunstable.snowprofileLayers <- function(x, ski_pen = NA, ...) {
  ## !no assertions in this method at all!

  ### assemble features ###
  if (!is.na(ski_pen)) x$ski_pen <- ski_pen
  features <- x[, c("v_strain_rate", "crit_cut_length", "sphericity", "gsize", "ski_pen", "slab_rhogs")]
  names(features) <- c("viscdefrate", "rcflat", "sphericity", "grainsize", "penetrationdepth", "slab_rhogs")

  ### compute p_unstable ###
  ## call python function if not all rows contain NAs
  if (sum(rowSums(is.na(features)) > 0) < nrow(features)) {
    x$p_unstable <- round(as.double(.pyunstable$comp_rf_probability(features, .RFmodel)), digits = 2)
  } else {
    x$p_unstable <- NA
  }


  return(x)
}
