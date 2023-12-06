#' Convert logical to day of year (doy)
#'
#' @param x SpatRaster - logical - with time attribute
#' @param from - Date - ymd - only if SpatRaster should be subset before being transformed.
#' @param to - Date - ymd - only if SpatRaster should be subset before being transformed.
#' @returns SpatRaster - numeric - day of year
#' @family SpatRaster transformation
#' @keywords internal
#' @description Converts logical SpatRaster with time attribute and one layer per day to single layer with the day of the year (doy) where the layer is 1/TRUE first.

convert_logical_to_doy <- function(x, from = NULL, to = NULL) {

  if (is.null(from)) from <- min(terra::time(x))
  if (is.null(to)) to <- max(terra::time(x))

  if (lubridate::year(from) < lubridate::year(to)) from <- lubridate::ymd(paste0(lubridate::year(to), "01-01"))

  name <- names(x)[terra::time(x) == to]

  time_vec <- seq(from, to, by = "days")
  x <- terra::subset(x, which(terra::time(x) %in% time_vec))
  doy <- lubridate::yday(terra::time(x[[1]]))
  out <- terra::ifel(x[[1]], doy, 0)

  if (terra::nlyr(x) > 1) {
    for (i in 2:terra::nlyr(x)) {
      doy <- lubridate::yday(terra::time(x[[i]]))
      out <- terra::ifel(out == 0 & x[[i]] != 0, doy, out)
    }
  }

  terra::time(out) <- to
  names(out) <- name

  return(out)
}
