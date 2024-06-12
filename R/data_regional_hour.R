#' Regional Weather data
#'
#' A dataset containing hourly mean temperatures in °C.
#' The dataset is a 4*4 pixel cutout centered at FVA provided by the
#' Agricultural meteorology Department of the DWD.
#'
#' The dataset can be loaded using load_test("SpatRaster_hour").
#'
#' Spatial resolution: 1 km x 1 km
#' Projection: DHDN / 3-degree Gauss-Kruger zone 3 (EPSG:31467)
#' Parameter: air temperature at 2 m
#'
#' The dataset is a list with three SpatRaster as objects.
#'
#' The list objects are:
#'
#' \itemize{
#'   \item fva_hour: SpatRaster with hourly mean temperature in °C - numeric
#' }
#'
#' @docType data
#' @name regional_hour
#' @keywords datasets
#' @format netCDF
NULL
