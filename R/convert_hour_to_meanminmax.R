#' Convert hourly to daily
#'
#' @param x SpatRaster - numeric - hourly tmean with time attribute
#' @returns SpatRaster list (tmean, tmax, tmin) - numeric  - with time attribute
#' @family SpatRaster transformation
#' @keywords internal

convert_hour_to_meanminmax <- function(x){

  # check missing values

  time <- terra::time(x)
  count <- rle(sort(as.vector(time)))[["lengths"]]
  missing <- sum(24 - count)

  if (missing != 0) warning(missing, " missing hours in sequence.")

  # summarise hourly temperature to daily tmin, tmean, tmax

  tmean <- terra::tapp(x, index = "days", fun = "mean")
  names(tmean) <- timename(tmean, "tmean")

  tmin <- terra::tapp(x, index = "days", fun = "min")
  names(tmin) <- timename(tmin, "tmin")


  tmax <- terra::tapp(x, index = "days", fun = "max")
  names(tmax) <- timename(tmax, "tmax")

  # create output

  srl <- list("tmean" = tmean,
              "tmin" = tmin,
              "tmax" = tmax)

  return(srl)

}




