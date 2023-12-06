#' Convert dataframe to SpatRaster list
#'
#' @param x dataframe (tmean, tmax, tmin, date)
#' @param date_col - character - name for date column which will be used to create time attribute of SpatRaster.
#' @returns SpatRaster list (tmean, tmax, tmin) - numeric - with time attribute
#' @family SpatRaster transformation
#' @keywords internal
#' @description
#' Internal function to create a SpatRaster list out of a dataframe.
#'
#' By default the dataframe should have a date column called "date".
#' The function is transferring each column into a list object and this object to a 1 col, 1 row nrow nlyr SpatRaster.

convert_df_to_srl <- function(x, date_col = "date") {

  if (!is.data.frame(x)) stop("x has to be dataframe")

  names <- names(x)[names(x) != date_col]
  lyr <- nrow(x)
  date <- x[, date_col]

  # create list and transform columns to Spatraster
  li <- list()

  for (i in names) {
    if (!is.numeric(x[, i])) stop(i, " is not numeric!")
    li[[i]] <- x[, i]
    li[[i]] <- array(li[[i]], dim = c(1, 1, lyr))
    li[[i]] <- terra::rast(li[[i]])

    terra::time(li[[i]]) <- lubridate::ymd(date)

  }

  if (length(li) == 1) li <- li[[1]]

  return(li)

}



