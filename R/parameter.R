#' Create parameter list
#'
#' @param model type of model for phenthau function - character - Either single
#' model or model collection
#' @param parametrisation type of parametrisation - character
#' @param year year of prognosis - numeric - Default: actual year
#' @param first logical - If TRUE and parametrisation is missing first
#' parametrisation in parameter() is used
#' @returns
#' If no argument is specified returns a list of parameter (used inside phenthau
#' function).
#' Otherwise returns available parameter for given model, parametrisation and
#' year
#' @family Main
#' @description
#'
#' See all available models with parameter: parameter()
#' model can be a single model or a model collection described in "Default
#' settings".
#' Return a data.frame with all model options:
#' parameter()
#'
#' \strong{Default Settings}
#'
#' The default parameter lists for different data input are:
#' "dailymean": Regional PHENTHAUproc described in Halbig et al. 2024 for daily
#' mean temperature data
#' "hour": "Local PHENTHAUproc described in Halbig et al. 2024 for daily hourly
#' temperature data
#' "dailymeanminmax": PHENTHAUproc adapted to DWD Data for daily mean, min and
#' max temperature data
#'
#' \strong{Columns}
#'
#' model: model
#' parametrisation: parametrisation
#' method: method used to calculate effective temperatures
#' ts_start: first day to calculate effective temperatures
#' ts_end last day to calculate effective temperatures (Default 30. Sept)
#' ts_prevyear: If True calculation of effective temperatures starts in previous
#' year. (i.e. wagenhoff)
#' ldt: lower development threshold
#' cf_dependent: Is model cold/frost dependent
#' cf_start: first day to calculate cold/frost days
#' cf_end: last day to calculate cold/frost days
#' cf_prevyear:  If True calculation of cold/frost days starts in previous year.
#' cf_temp: tmean" for cold days and "tmin" for frost days
#' cf_limit: threshold for cold/frost days
#' set: formula to calculate sum of effective temperatures
#' a: parameter for set
#' b: parameter for set
#'
#' @export
#' @examples
#'
#' # Default parameter list for daily mean, min and max temperature data:
#' parameter("dailymeanminmax")
#'
#' # overview dataframe with all available parameter sets
#' parameter("all")
#'
#' # all hatch model parameter
#' parameter("hatch")
#'
#' # return parameter necessary for calculation
#' parameter("hatch", "custers", 2020)

parameter <- function(model = NULL, parametrisation = NULL, year = NULL, first = TRUE) {

  # read parameter file
  p <- readRDS(system.file("config", "parameter.rds", package = "PHENTHAUproc", mustWork = TRUE))

  # show all parameter as a dataframe
  if (is.null(model)) return(p)
  if (model == "all") return(p)

  # set year if not given
  if (is.null(year)) {

    year <- as.numeric(format(Sys.Date(), "%Y"))

  }

  # give parameter for daily mean temperature data
  if (model == "dailymean") {

    p <- rbind(
      p[p$model == "budswelling" & p$parametrisation == "quercus_robur_clone256_type1",],
      p[p$model == "leafunfolding" & p$parametrisation == "quercus_robur_clone256_type1",],
      p[p$model == "hatch" & p$parametrisation == "meurisse" & p$method == "tsum",],
      p[p$model == "L2" & p$parametrisation == "degreedays" & p$method == "tsum",],
      p[p$model == "L3" & p$parametrisation == "degreedays" & p$method == "tsum",],
      p[p$model == "L4" & p$parametrisation == "degreedays" & p$method == "tsum",],
      p[p$model == "L5" & p$parametrisation == "degreedays" & p$method == "tsum",],
      p[p$model == "L6" & p$parametrisation == "degreedays" & p$method == "tsum",],
      p[p$model == "Pp" & p$parametrisation == "degreedays" & p$method == "tsum",],
      p[p$model == "Ad" & p$parametrisation == "degreedays" & p$method == "tsum",]
    )

    # give parameter for hourly temperature data
  } else if (model == "hour") {

    p <- rbind(
      p[p$model == "budswelling" & p$parametrisation == "quercus_robur_clone256_type1",],
      p[p$model == "leafunfolding" & p$parametrisation == "quercus_robur_clone256_type1",],
      p[p$model == "hatch" & p$parametrisation == "custers",],
      p[p$model == "hatch" & p$parametrisation == "wagenhoff",],
      p[p$model == "hatch" & p$parametrisation == "meurisse",],
      p[p$model == "L2" & p$method == "tsum",],
      p[p$model == "L3" & p$method == "tsum",],
      p[p$model == "L4" & p$method == "tsum",],
      p[p$model == "L5" & p$method == "tsum",],
      p[p$model == "L6" & p$method == "tsum",],
      p[p$model == "Pp" & p$method == "tsum",],
      p[p$model == "Ad" & p$method == "tsum",]
    )

    # give parameter for daily mean min and max temperature data
  } else if (model == "dailymeanminmax") {

    p <- rbind(
      p[p$model == "budswelling" & p$parametrisation == "quercus_robur_clone256_type1" ,],
      p[p$model == "leafunfolding" & p$parametrisation == "quercus_robur_clone256_type1",],
      p[p$model == "hatch" & p$parametrisation == "custers",],
      p[p$model == "hatch" & p$parametrisation == "wagenhoff",],
      p[p$model == "hatch" & p$parametrisation == "meurisse",],
      p[p$model == "L2" & p$method == "baskerville",],
      p[p$model == "L3" & p$method == "baskerville",],
      p[p$model == "L4" & p$method == "baskerville",],
      p[p$model == "L5" & p$method == "baskerville",],
      p[p$model == "L6" & p$method == "baskerville",],
      p[p$model == "Pp" & p$method == "baskerville",],
      p[p$model == "Ad" & p$method == "baskerville",]
    )

  } else if (!model %in% p$model) {

    stop(model, " has to be one of model parameter list. see parameter(model = \"all\")")

  } else {

    p <- p[model == p$model,]

    if (is.null(parametrisation)) {

      if (first) p <- p[1,] else return(p)

    } else p <- p[parametrisation == p$parametrisation,]

  }

  if (nrow(p) == 0) stop("No model found for model = ", model, " and parametrisation = ", parametrisation, "!")

  # stupid hack because of hatchmodel appears multiple times
  listnames <- ifelse(p$model != "hatch", p$model, p$parametrisation)

  p <- split(p, listnames)

  p <- lapply(p, function(x) {

    x$ts_start <- get_date(year, x$ts_start, x$ts_prevyear)
    x$ts_end <- get_date(year, x$ts_end)

    if (x$cf_dependent) {

      x$cf_start <- get_date(year, x$cf_start, x$cf_prevyear)
      x$cf_end <- get_date(year, x$cf_end)

    } else {

      x$cf_start <- NULL
      x$cf_end <- NULL
      x$cf_temp <- NULL
      x$cf_limit <- NULL

    }

    x$ts_prevyear <- NULL
    x$cf_prevyear <- NULL

    x <- as.list(x)

    if (is.na(x$set)) x$set <- NA else x$set <- eval(parse(text = paste0(x$set)))

    return(x)

  }

  )

  if (model %in% c("dailymean", "dailymeanminmax", "hour")) {

    p$reftime <- model
    p$year <- year

  }

  return(p)

}





