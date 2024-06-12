#' Calculate mean TRUE
#'
#' @param x SpatRaster list - logical - with time attribute
#' @returns SpatRaster - logical - with time attribute
#' @family Calculation
#' @keywords internal
#' @description Takes multiple logical SpatRasters with time attribute and
#' returns SpatRaster with the mean time serial number of first and last TRUE
#' value.

calc_mean <- function(x) {

  time <- get_time(x[[1]])
  #lyr <- 1:terra::nlyr(x[[1]])

  x <- lapply(x, convert_logical_to_doy)
  x <- terra::app(terra::rast(x), mean)
  x <- round(x)

  x <- convert_doy_to_logical(x, from = min(time), to = max(time))

  return(x)

}
