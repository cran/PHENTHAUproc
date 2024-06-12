#' Calculate mortality
#'
#' @param x SpatRaster list / dataframe with temperature data and time
#' attribute/column
#' @param budswelling SpatRaster - logical - T/F budswelling/no_budswelling
#' @param hatch SpatRaster - logical - T/F hatch/no_hatch
#' @param mot mortality threshold - numeric - is used as lower development
#' threshold calculating degree days
#' @returns SpatRaster of starving related mortality in %.
#' @family Models
#' @keywords internal

calc_mortality <- function(x, budswelling, hatch, mot = 0) {

  check_dimension_and_time(budswelling, hatch)

  starve <- terra::ifel(hatch & !budswelling, TRUE, FALSE)

  from <- min(get_time(starve))
  to <- max(get_time(starve))

  x <- subset_time(x, from, to)
  time <- terra::time(x$tmean)

  x <- lapply(x, function(y) terra::ifel(starve, y, -999))

  x <- lapply(x, function(y) {
    terra::time(y) <- time
  return(y)
  }
  )

  dd <- calc_degreedays(x, ts_start = from, ts_end = to, ldt = mot, method = "tsum")

  mortality_tab <- matrix(
    c(
      0, 130, 0,
      130, 147, 5,
      147, 187, 10,
      187, 210, 25,
      210, 238, 50,
      238, 313, 75,
      313, 9999, 100
    ),
    ncol = 3,
    byrow = T
  )

  mortality <- terra::classify(dd, mortality_tab)
  mortality <- mortality[[terra::nlyr(mortality)]]

  return(mortality)
}
