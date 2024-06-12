#' Calculate degree days
#'
#' @param x SpatRaster list (tmean, tmax, tmin) - numeric - with time attribute
#' @param ts_start start of timeseries - Date
#' @param ts_end end of timeseries - Date
#' @param ldt lower development threshold - numeric
#' @param method name of degreedays/temperature sum method - character - either
#' "baskerville" or "tsum"
#' @family Models
#' @keywords internal
#' @returns SpatRaster of growing (summed up) degree days.

calc_degreedays <- function(x,
                            ts_start,
                            ts_end,
                            ldt,
                            method) {

  x <- subset_time(x, ts_start, ts_end)

  # check method
  if (!method %in% c("baskerville", "tsum")) stop("method has to be one of temperature sum methods: ", list("\nbaskerville ", "\ntsum"))
  if (method == "baskerville") f <- calc_baskerville
  if (method == "tsum") f <- calc_tsum

  # select necessary temperatures
  minmeanmax <- c("tmin", "tmean", "tmax")
  temperature <- minmeanmax[minmeanmax %in% names(formals(f))]

  x <- x[temperature]
  time <- get_time(x)
  x <- terra::sds(x)
  degree_days <- terra::lapp(x, f, usenames = T, ldt = ldt)
  terra::time(degree_days) <- time

  degree_days <- cumsum(degree_days)

  names(degree_days) <- timename(degree_days, "degreedays")

  # we need the summed up degree days for comparision with sum of effective
  # temperatures

  return(degree_days)

}







