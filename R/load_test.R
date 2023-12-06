#' Load test data
#'
#' @param type character, either day/hour/SpatRaster
#' @returns data.frame/SpatRaster list with test data
#' @family Helper
#' @export


load_test <- function(type = "SpatRaster") {

  if (type == "day") td <- load_test_day()
  if (type == "hour") td <- load_test_hour()
  if (type == "SpatRaster") td <- load_test_srl()

  return(td)
}


#' load dataframe - daily tmean tmin tmax - Freiburg 2020
#' @keywords internal

load_test_day <- function() {

  f <- system.file("extdata", "freiburg.csv", package = "PHENTHAUproc", mustWork = TRUE)

  x <- utils::read.csv(f)

  return(x)
}


#' load dataframe - hourly tmean - Freiburg 2023
#' @keywords internal

load_test_hour <- function() {

  f <- system.file("extdata", "freiburg_hour.csv", package = "PHENTHAUproc", mustWork = TRUE)

  x <- utils::read.csv(f)

  return(x)

}


#' load SpatRaster list - daily - 16 Rasterpoints around FVA 2020
#' @keywords internal

load_test_srl <- function() {

  tmean <- terra::rast(system.file("extdata", "fva_tmean.nc", package = "PHENTHAUproc", mustWork = TRUE))
  tmax <- terra::rast(system.file("extdata", "fva_tmax.nc", package = "PHENTHAUproc", mustWork = TRUE))
  tmin <- terra::rast(system.file("extdata", "fva_tmin.nc", package = "PHENTHAUproc", mustWork = TRUE))

  li <- list(tmean, tmax, tmin)
  names(li) <- c("tmean", "tmax", "tmin")

  return(li)
}





