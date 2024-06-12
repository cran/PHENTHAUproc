#' Local hourly Weather Station data from Freiburg
#'
#' A dataset containing hourly temperatures for Freiburg from 2019-09-01 to
#' 2022-12-31.
#' Downloaded from opendata.dwd.de and preprocessed.
#'
#' The dataset can be loaded using load_test("hour").
#'
#' Stations_id: 01443
#' Stationsname: Freiburg
#'
#' The variables are as follows:
#'
#' \itemize{
#'   \item date in year-month-day - character
#'   \item tmean hourly mean temperature in °C - numeric
#' }
#'
#' @docType data
#' @keywords datasets
#' @name local_hourly
#' @source https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/historical/
#' @format A data frame with 29232 rows and 3 variables
NULL
