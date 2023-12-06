#' Calculate temperature sum
#'
#' @param tmean numeric - mean temperature
#' @param ldt numeric - lower development threshold aka base temperature.
#' @description
#' The function is vectorized.
#' To use it with SpatRaster-Data see ?terra::lapp.
#'
#' @returns The temperature sum over the lower development threshold.
#' @family Temperature sum methods
#' @keywords internal

calc_tsum <- function(tmean, ldt) {

  tmean[tmean <= ldt & !is.na(tmean)] <- 0
  tmean[tmean > ldt & !is.na(tmean)] <- tmean[tmean > ldt & !is.na(tmean)] - ldt

  return(tmean)
}


