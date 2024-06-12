#' Calculate sum of effective temperatures (SET)
#'
#' @param x SpatRaster list (tmean, tmax, tmin) - numeric - with time attribute
#' @param cf_temp - character - "tmean" for cold days and "tmin" for frost days
#' @param cf_start - Date - start of cold/frost timeseries
#' @param cf_end - Date - end of cold/frost timeseries
#' @param cf_limit - numeric - threshold under which a day is defined as
#' cold/frost day
#' @param set - function - x is cold/frost days, a and b are parameter i.e.
#' function (x, a, b) a + b*x
#' @param a - numeric - parameter for set function
#' @param b - numeric - parameter for set function
#' @returns SpatRaster with sum of effective temperatures.
#' @family Models
#' @keywords internal

calc_sumefftemp <- function(x,
                            cf_temp,
                            cf_start,
                            cf_end,
                            cf_limit,
                            set,
                            a,
                            b) {

  # subset to for cold/frost important days
  x <- subset_time(x[[cf_temp]], cf_start, cf_end)

  # calculate sum of effective temperatures using the formula set and the
  # cold_frost days
  # determine if day has a temperature under the threshold cf_limit
  x <- terra::ifel(x < cf_limit, 1, 0)
  x <- cumsum(x)

  # calculating sum of effective temperatures with cold_frost days and the set
  # function

  time <- terra::time(x)
  x <- terra::app(x, set, a = a, b = b)
  terra::time(x) <- time

  # rename with time
  names(x) <- timename(x, "sumefftemp")

  return(x)

}
