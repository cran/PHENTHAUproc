#' Convert dwd temperature data to PHENTHAUproc input
#'
#' @param x filepath to dwd temperature data (hourly)
#' @returns A dataframe with date, hour and tmean column.
#' @family Helper
#' @export

convert_dwd_to_phenthau <- function(x) {

  x <- utils::read.csv2(x)
  x$date <- substr(x$MESS_DATUM, 1, 8)
  x$hour <- substr(x$MESS_DATUM, 9, 10)
  x$tmean <- as.numeric(x$TT_TU)
  x <- x[ ,c("date", "hour", "tmean")]

  x <- x[order(x$date, x$hour), ]

  return(x)

}

