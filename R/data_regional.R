#' Regional Weather data
#'
#' A dataset containing daily mean, minimum and maximum temperatures in °C.
#' The dataset is a 4*4 pixel cutout centered at FVA from the "Hyras" dataset
#' available at the DWD open data center
#' (https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/hyras_de/)
#'
#' The dataset can be loaded using load_test("SpatRaster").
#'
#' Spatial resolution: 5 km x 5 km
#' Projection: ETRS89 / LCC Europe (EPSG:3034)
#' Parameter: air temperature at 2 m
#'
#' The dataset is a list with three SpatRaster as objects.
#' The time attribute for all three SpatRaster is equal.
#'
#' The list objects are:
#'
#' \itemize{
#'   \item tmean: SpatRaster with tmean daily mean temperature in °C - numeric
#'   \item tmax: SpatRaster with tmax daily mean temperature in °C - numeric
#'   \item tmin: SpatRaster with tmin daily mean temperature in °C - numeric
#' }
#'
#' @docType data
#' @name regional
#' @keywords datasets
#' @format A list of SpatRaster
NULL
