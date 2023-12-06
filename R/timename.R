#' Create a name with time stamp
#'
#' @param x SpatRaster with time attribute
#' @param model character - name of model
#' @param desc character - additional description (optional)
#' @family Helper
#' @keywords internal

timename <- function(x, model, desc = NULL) {

  time <- terra::time(x)

  if (!is.null(desc)) model <- paste(model, desc, "_")

  newname <- paste0(model, "_", gsub("-", "", paste(time)))

  return(newname)

}
