#' Check data for model requirements
#'
#' @param x SpatRaster list - numeric - with time attribute
#' @param params parameter list
#' @returns No return value, called for side effects.
#' @family Helper
#' @keywords internal

check_data_with_params <- function(x, params) {

  time <- get_time(x)

  if (!params$ts_start %in% time) stop("ts_start not in time attribute/vector!")
  if (params$cf_dependent && !params$cf_start %in% time) stop("cf_start not in time attribute/vector!")
  if (params$hatch_dependent && is.null(params$hatch)) stop("model is hatch dependent so hatch has to be provided in data")

}
