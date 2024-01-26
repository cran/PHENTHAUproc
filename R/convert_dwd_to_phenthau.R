#' Convert hourly DWD temperature data to PHENTHAUproc input
#'
#' @param x filepath to unzipped DWD temperature data (text file)
#' @returns A dataframe with date, hour and mean air temperature (tmean).
#' @family Helper
#' @export

convert_dwd_to_phenthau <- function(x) {

  # read DWD raw data
  x <- utils::read.csv2(x)

  # extract date, hour and tmean
  x$date <- substr(x$MESS_DATUM, 1, 8)
  x$hour <- substr(x$MESS_DATUM, 9, 10)
  x$tmean <- as.numeric(x$TT_TU)

  # set names to input names necessary for phenthau function
  x <- x[ ,c("date", "hour", "tmean")]

  x <- x[order(x$date, x$hour), ]

  return(x)

}

