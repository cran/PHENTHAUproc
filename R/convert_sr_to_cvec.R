#' Convert SpatRaster to characer vector
#'
#' @param x SpatRaster - numeric - value is timeserialnumber
#' @returns vector - Date as character
#' @family SpatRaster transformation
#' @keywords internal
#' @description
#' Transform local PHENTHAUproc results back from SpatRaster to vector format

convert_sr_to_cvec <- function(x) {

  x <- terra::values(x)
  x <- as.numeric(x)
  x <- lubridate::as_date(x)
  x <- as.character(x)

  return(x)

}

