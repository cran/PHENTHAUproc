#' Convert day of year (doy) to logical
#'
#' @param x SpatRaster - numeric - value is day of year
#' @param from Date - ymd - first day of time attribute
#' @param to Date - ymd - last day of time attribute
#' @param by character - either "days", "weeks" or "months". Reduces return to
#' one day per day/week/month
#' @returns SpatRaster - logical - TRUE/FALSE before_doy/equal_after_doy
#' @family SpatRaster transformation
#' @keywords internal

convert_doy_to_logical <- function(x, from, to, by = "days") {

  time_vec <- seq(from, to, by = by)
  doy_vec <- lubridate::yday(time_vec)

  x_log <- lapply(doy_vec, function(y) {
    out <- terra::ifel(x > 0 & x <= y, 1, 0)
    terra::time(out) <- y
    return(out)
  })

  x_log <- terra::rast(x_log)
  terra::time(x_log) <- time_vec

  return(x_log)

}
