#' Set attributes for SpatRaster
#'
#' @param x SpatRaster - to assign attributes to
#' @param type character - type of attributes i.e. "stages"
#' @family Helper
#' @keywords internal


set_attributes <- function(x, type) {

  lev <- get_legend(type)[,c("ID", "category")]

  for (i in 1:terra::nlyr(x)) levels(x[[i]]) <- lev

  #names(x) <- timename(x, type)

  return(x)

}

