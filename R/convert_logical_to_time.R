#' Convert logical to time
#'
#' @param x SpatRaster - logical - with time attribute
#' @returns SpatRaster - numeric
#' @family SpatRaster transformation
#' @keywords internal
#' @description Converts logical SpatRasters to single layer SpatRaster with timeserialnumber as value for first TRUE

convert_logical_to_time <- function(x) {

    time <- terra::time(x)
    layer <- 1:terra::nlyr(x)
    ma <- matrix(c(layer, time), ncol = 2, byrow = F)

    x <- terra::which.lyr(x)
    #x <- terra::as.factor(x)
    x <- terra::classify(x, rcl = ma)
    names(x) <- "timeserialnumber"

    return(x)


}
