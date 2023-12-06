#' Create parameter list
#'
#' @param model type of model for phenthau function - character
#' @param parametrisation type of parametrisation - character
#' @param year year of prognosis - numeric - used to transform ts_start, ts_end, cf_start, cf_end to date format
#' @param first logical - If TRUE and parametrisation is missing takes first in list
#' @returns If model or parametrisation or year is missing returns a dataframe of available parameter settings.
#' If all are specified returns a list of parameter (used inside phenthau function).
#' @family Main
#' @description
#'
#' See all available models and parametrisations: parameter()
#'
#' @export
#' @examples
#' # return parameter list
#' parameter()
#'
#' # return all hatch model parameter
#' parameter("hatch")
#'
#' # return parameter necessary for calculation
#' parameter("hatch", "custers", 2020)

parameter <- function(model = NULL, parametrisation = NULL, year = NULL, first = TRUE) {

  # read parameter file
  p <- utils::read.csv(system.file("config", "parameter.csv", package = "PHENTHAUproc", mustWork = TRUE))

  # select one model
  if (is.null(model)) return(p)

  if (!model %in% p$model) stop(model, " has to be one of model parameter list. see parameter()")

  p <- p[model == p$model,]

  if (is.null(parametrisation)) {

    if (first) p <- p[1,] else return(p)

  } else{

    p <- p[parametrisation == p$parametrisation,]

  }

  if (nrow(p) == 0) stop("No model found for model = ", model, " and parametrisation = ", parametrisation, "!")

  if (!is.null(year)) {

    if (!is.numeric(year)) stop("year has to be numeric.")

    p$ts_start <- get_date(year, p$ts_start, p$ts_prevyear)
    p$ts_end <- get_date(year, p$ts_end)

    if (p$cf_dependent) {

      p$cf_start <- get_date(year, p$cf_start, p$cf_prevyear)
      p$cf_end <- get_date(year, p$cf_end)

    }

    p$ts_prevyear <- NULL
    p$cf_prevyear <- NULL

    }

  if (nrow(p) == 1) {

    p <- as.list(p)
    p$set <- eval(parse(text = paste0(p$set)))

  }

  return(p)

}



