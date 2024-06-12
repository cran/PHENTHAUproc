#' Load test data
#'
#' @param type character, either day/hour/SpatRaster/SpatRaster_hour/budswelling
#' @returns data.frame/SpatRaster list with test data
#' @family Helper
#' @export


load_test <- function(type = "SpatRaster") {

  if (type == "day") return(load_test_day())
  if (type == "hour") return(load_test_hour())
  if (type == "SpatRaster") return(load_test_srl())
  if (type == "SpatRaster_hour") return(load_test_srl_hour())
  if (type == "budswelling") return(load_test_bs())

  stop("testfile with name ", type, " not found.")

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


#' load SpatRaster - hourly - 16 Rasterpoints around FVA 2022 - 2023
#' @keywords internal

load_test_srl_hour <- function() {

  thour <- terra::rast(system.file("extdata", "fva_hour.nc", package = "PHENTHAUproc", mustWork = TRUE))

  return(thour)
}

#' load SpatRaster - budswelling - day of year - 16 Rasterpoints around FVA 2023
#' @keywords internal

load_test_bs <- function() {

  bs <- terra::rast(system.file("extdata", "fva_bs.nc", package = "PHENTHAUproc", mustWork = TRUE))

  return(bs)
}


