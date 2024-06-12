#' Calculate last TRUE
#'
#' @param x SpatRaster - logical - with time attribute
#' @returns SpatRaster with time serial number (origin = lubridate::origin) of
#' last TRUE layer.
#' @family Calculation
#' @keywords internal
#' @description Takes multiple logical SpatRasters with time attribute and
#' returns SpatRaster with time serial number of last TRUE layer time.

calc_last <- function(x) {

  time <- terra::time(x)
  layer <- 1:terra::nlyr(x)
  ma <- matrix(c(rev(layer), time), ncol = 2, byrow = F)

  x <- terra::which.lyr(x[[rev(layer)]])
  #x <- terra::as.factor(x)
  x <- terra::classify(x, rcl = ma)
  names(x) <- "timeserialnumber"

  return(x)

}
