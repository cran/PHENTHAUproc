#' Call a function from PHENTHAUproc package
#'
#' @param f function
#' @param data SpatRaster list (tmean, tmax, tmin) - numeric - with time attribute
#' @param params parameter list
#' @family Helper
#' @keywords internal
#' @description calls a function from PHENTHAUproc package

call_function <- function(f, data, params) {

  fA <- methods::formalArgs(f)

  if ("data" %in% fA) params$data <- data else params <- append(data, params)

  if (is.list(params)) params <- params[which(names(params) %in% fA)]

  if (is.null(params)) warning("Parameterlist for function ", f, "is empty.")

  return(do.call(f, params))

}
