#' Remove FALSE
#'
#' @param x SpatRaster/SpatRaster list - numeric - with time attribute
#' @param y SpatRaster - logical - with time attribute
#' @family Helper
#' @keywords internal
#' @description
#' Set values in SpatRaster or SpatRaster list to -999 if y is FALSE

remove_false <- function(x, y) {

  # remove x which is not covered by y
  ty <- get_time(y)
  start <- min(ty)
  end <- max(ty)

  x <- subset_time(x, start, end)

  # choose daily layers multiple times when x is hourly

  names(y) <- as.character(ty)
  tx <- as.character(get_time(x))
  y <- y[[tx]]

  # overwrite y is FALSE with -999

  x <- lapply(x, function(z) terra::ifel(y, z, -999))

  return(x)

}
