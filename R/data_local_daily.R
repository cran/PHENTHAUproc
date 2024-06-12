#' Local daily Weather Station data from Freiburg
#'
#' A dataset containing daily mean, max and min temperatures for Freiburg from
#' 2019-09-01 to 2020-09-30.
#' Downloaded from opendata.dwd.de and preprocessed.
#'
#' The dataset can be loaded using load_test("day").
#'
#' Stations_id: 01443
#' Stationsname: Freiburg
#'
#' The variables are as follows:
#'
#' \itemize{
#'   \item date in year-month-day - character
#'   \item tmean daily mean temperature in °C - numeric
#'   \item tmax daily max temperature in °C - numeric
#'   \item tmin daily min temperature in °C - numeric
#' }
#'
#' @docType data
#' @keywords datasets
#' @name local_daily
#' @source https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/
#' @format A data frame with 396 rows and 4 variables
NULL


