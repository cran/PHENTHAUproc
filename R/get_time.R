#' Get time attribute
#'
#' @param x SpatRaster, data.frame, list of SpatRaster or SpatRasterDataset
#' @returns If x is a data.frame returns the time column, else it returns the
#' time attribute of a SpatRaster or the shared time attribute of multiple
#' SpatRaster.
#' @family Helper
#' @keywords internal

get_time <- function(x) {

  if (methods::is(x, "SpatRaster")) time <- terra::time(x)

  if (methods::is(x, "data.frame")) time <- lubridate::ymd(x$date, tz = "")

  if (methods::is(x, "list") | methods::is(x, "SpatRasterDataset")) {

    time <- lapply(x, terra::time)
    f <- function(x, y){return(x[x %in% y])}
    time <- Reduce(f, time)

  }

  return(time)
}

