#' Calculate degree days using the single sine method of Baskerville
#'
#' @param tmin min temperature - numeric
#' @param tmax max temperature - numeric
#' @param ldt lower development threshold aka base temperature - numeric
#' @returns Degree days calculated by using the single sine method of Baskerville.
#' @description
#'
#' The function is vectorized.
#' To use it with SpatRaster data see ?terra::lapp
#'
#' More Information about the Method itself:
#' Baskerville, G.L. and P. Emin. 1969. Rapid estimation of heat accumulation from maximum and minimum temperature. Ecology 50:514-517.
#' @family Temperature sum methods
#' @keywords internal

calc_baskerville <- function(tmin, tmax, ldt) {

  tmean <- (tmin + tmax) / 2

  tmax[tmax <= ldt & !is.na(tmax)] <- 0

  tmax[tmin >= ldt & !is.na(tmin)] <- tmean[tmin >= ldt & !is.na(tmin)] - ldt

  sel <- tmin < ldt & tmax > ldt & !is.na(tmin) & !is.na(tmax)

  tmax[sel] <- (((((tmax[sel] - tmin[sel]) / 2) * cos(asin(((ldt - tmean[sel]) / ((tmax[sel] - tmin[sel]) / 2))))) +
                   ((tmean[sel] - ldt) * ((pi / 2) - asin(((ldt - tmean[sel]) / ((tmax[sel] - tmin[sel]) / 2)))))) / pi)

  tmax <- round(tmax, digits = 2)

  return(tmax)
}

