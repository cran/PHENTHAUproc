#' Check dimension and time attribute of a SpatRaster list
#'
#' @param ... SpatRaster/SpatRaster list/data.frame/vector
#' @returns No return value, called for side effects.
#' @family Helper
#' @keywords internal

check_dimension_and_time <- function(...) {

  li <- list(...)

  if (methods::is(li[[1]], "list")) {
    if (length(li) != 1) warning("Multiple list objects as input. Only first will be used!")

    li <- li[[1]]
  }

  if (methods::is(li[[1]], "SpatRaster")) {
    share_geom <- length(unique(lapply(li, terra::crs))) == 1

    if (!share_geom) stop("SpatRaster dont share same geometry!")

    time_vec <- lapply(li, terra::time)

    share_time <- length(unique(time_vec)) == 1

    if (!share_time) stop("SpatRaster dont share same time vector!")
  }

  if (methods::is(li[[1]], "vector")) {
    share_length <- length(unique(lapply(li, nrow))) == 1

    if (!share_length) stop("Vectors dont have the same length!")
  }

}
