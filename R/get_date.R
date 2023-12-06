#' Get Date format out of numeric year and monthday character value
#' @param year numeric year
#' @param monthday character with month and day i.e. ("-02-01" or "Feb 01")
#' @param prevyear if TRUE starts with previous year
#' @keywords internal
#' @family Helper

get_date <- function(year, monthday, prevyear = FALSE) {

  if (!is.na(prevyear) && prevyear) year <- year - 1

  day <- lubridate::as_date(
    lubridate::ymd(
      paste0(year, monthday),
      tz = NULL),
    tz = NULL)

  return(day)

}
