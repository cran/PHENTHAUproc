#' Calculate first TRUE
#'
#' @param ... SpatRasters/SpatRaster list - logical - with time attribute
#' @returns SpatRaster - logical - with time attribute
#' @family Calculation
#' @keywords internal
#' @description Takes multiple logical SpatRasters with time attribute and
#' returns SpatRaster with common time and TRUE if any SpatRaster is TRUE.

calc_first <- function(...) {

  li <- list(...)

  if (methods::is(li[[1]], "list")) {

    if (length(li) == 1) li <- li[[1]] else stop("Cant calc_first of multiple lists!")

  }

  if (!methods::is(li[[1]], "SpatRaster")) stop("Input needs to be SpatRaster!")

  # take shared common time
  time <- lapply(li, terra::time)

  common <- Reduce(
    function(x, y) x[x %in% y],
    time
  )

  # subset all SpatRaster to common time
  sr <- lapply(li,
               function(x) terra::subset(x, which(terra::time(x) %in% common))
  )

  # reduce to one SpatRaster
  first <- Reduce(
    function(x, y) terra::ifel(x > y, x, y),
    sr
  )

  return(first)

}
