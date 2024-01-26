#' Calculate PHENTHAUproc model
#'
#' @param x SpatRaster list/data.frame (tmean, tmax, tmin) - numeric - with time attribute/date column
#' @param params list with parameter. If NULL parameter set is chosen by data input
#' @param def_hatch definition of hatch - character - either "first" or "mean"
#' @returns data.frame/list with all PHENTHAUproc model outputs (see details)
#' @family Main
#' @description
#' "phenthau" implements the early warning system PHENTHAUproc created by Halbig et al. 2024 in R.
#'
#' @details
#'
#' \strong{Overview}
#'
#' phenthau function combines multiple phenology models:
#' - bud swelling & leaf unfolding of \emph{Quercus robur} or \emph{Quercus petraea}(still in development)
#' - hatch of oak processionary moth using 3 different hatch models (Custers 2003, Wagenhoff et al. 2014 and Meurisse et al. 2012)
#' - development stages of OPM (0 egg - 8 adult)
#'
#' \strong{Parametrisation}
#'
#' Additional parametrizations are provided but have not yet been tested.
#'
#' Use parameter() to return a data.frame with all possible parametrisation options or choose a model.
#' The default is dependent on the data input and correspond to the model described by Halbig et al. 2024.
#'
#' - "dailymean": Regional PHENTHAUproc how described in Halbig et al. 2024 for daily mean temperature data
#' - "hour": Local PHENTHAUproc how described in Halbig et al. 2024 for hourly temperature data
#' - "dailymeanmaxmin": PHENTHAUproc for daily mean, max and min temperature data adapted to DWD
#'
#' - def_hatch: how to calculate hatch. if "first" the first hatch prediction by any model is taken. If "mean" the mean day of all hatch models is taken.
#'
#' \strong{Output}
#'
#' Regional Output:
#' phenthau for a SpatRasterList input returns a list with all model calculations as SpatRaster objects:
#'
#' - stages: SpatRaster with one layer per day - numeric - values from 0-8 (0 egg stage - 8 adult stage).
#'  -> use get_legend("stages") to show id/cover/colors
#'
#' - custers/wagenhoff/meurisse - one layer per day - logical - TRUE/FALSE for hatch or no hatch
#' - budswelling: one layer per day - logical - TRUE/FALSE for budswelling or no budswelling
#' - leafunfolding: one layer per day - logical - TRUE/FALSE for leafunfolding or no leafunfolding
#'  -> plot the first day of a logical SpatRaster with plot_date()
#'
#' - mortality: one layer - integer - mortality in %
#' - ppa_biocide: one layer per day - numeric - 0 application of plant protection agents (PPA) and biocides not yet effective, 1 application effective, 2 appliaction not effective anymore
#'
#' Local Output:
#' phenthau for a data.frame input returns a data.frame with two columns.
#' model: name of model
#' date: date of first appearing of event
#'
#' \strong{Presentation}
#'
#' Regional Output:
#' - plot_stages is a wrapper around terra::plot to preset legend, names, colors and day to plot.
#' - plot_date is a wrapper around terra::plot to plot the date of first TRUE in multiple layered SpatRaster
#'
#' Local Output:
#' - plot_station is for local weather station data and creates a phenology wheel (ggplot2 & geomtextpath required!)
#' - plot_station_step is for local weather station data and creates a stepwise graph for the development stages
#'
#' @references
#'
#' Halbig et al. 2024: Halbig, P., Stelzer, A. S., Baier, P., Pennerstorfer, J., Delb, H., & Schopf, A. (2024). PHENTHAUproc–An early warning and decision support system for hazard assessment and control of oak processionary moth (Thaumetopoea processionea). Forest Ecology and Management, 552, 121525 \cr
#' Custers 2003: Custers, C. (2003). Climate change and trophic synchronisation. English Wageningen UR, Chairgroup Environmental Systems Analysis.\cr
#' Wagenhoff et al. 2014: Wagenhoff, E., Wagenhoff, A., Blum, R., Veit, H., Zapf, D., & Delb, H. (2014). Does the prediction of the time of egg hatch of Thaumetopoea processionea (Lepidoptera: Notodontidae) using a frost day/temperature sum model provide evidence of an increasing temporal mismatch between the time of egg hatch and that of budburst of Quercus robur due to recent global warming?. European Journal of Entomology, 111(2).\cr
#' Meurisse et al. 2012: Meurisse, N., Hoch, G., Schopf, A., Battisti, A., & Grégoire, J. C. (2012). Low temperature tolerance and starvation ability of the oak processionary moth: implications in a context of increasing epidemics. Agricultural and forest entomology, 14(3), 239-250.\cr
#'
#' @author Bachfischer Lorenz, Department of Forest Protection FVA (2024)
#'   \email{lorenz.bachfischer@@posteo.de}
#' @export
#' @examples
#'
#' srl <- load_test()
#' phen <- phenthau(srl)
#'

phenthau <- function(x,
                     params = NULL,
                     def_hatch = "first"
) {

  ### 1 check input -------------
  # We want to cover three time reference options
  # 1. daily mean temperatures -> regional phenthauproc
  # 2. hourly temperatures -> local phenthauproc
  # 3. daily mean, min, max temperatures -> improved regional phenthauproc

  # check how input is structured

  reftime <- NULL

  if (all(c("date", "hour", "tmean") %in% names(x))) {

    reftime <- "hour"

    message("Calculating PHENTHAUproc with hourly temperature data.")

  } else if (all(c("tmean", "tmin", "tmax") %in% names(x))) {

    reftime <- "dailymeanminmax"

    message("Calculating PHENTHAUproc with daily mean, min and max temperature data.")

  } else if ("tmean" %in% names(x)) {

    reftime <- "dailymean"

    message("Calculating PHENTHAUproc with daily mean temperature data.")

  } else{

    stop("names(x) not like needed. See details.")

  }


  ### 02 Convert input -----------------

  # Convert input if its a dataframe

  if (methods::is(x, "data.frame")) {

    x <- convert_df_to_srl(x[, names(x) != "hour"])

    is_dataframe <- TRUE # if input is a dataframe, output should be too.

  } else is_dataframe <- FALSE

  # Convert hourly to daily mean, min and max temperatures (needed for most of the models)
  if (reftime == "hour") {

    x_hour <- list("tmean" = x)
    x <- convert_hour_to_meanminmax(x_hour$tmean)

  }

  if (!methods::is(x, "list")) stop("x has to be either class SpatRaster or a data.frame")


  ### 03 set and check times ---------------------

  # write the times with lubridate
  x <- lapply(x, function(y) {
    terra::time(y) <- lubridate::as_date(terra::time(y), tz = NULL)
    return(y)
  })

  # check if days complete
  time <- get_time(x)
  year <- lubridate::year(max(time))
  from <- lubridate::as_date(paste0(year - 1, "-09-01"), tz = NULL)
  to <- lubridate::as_date(paste0(year, "-09-30"), tz = NULL)

  if (to > max(time)) to <- max(time)

  message("First day of calculation: ", from)
  message("Last day of calculation: ", to)

  sequence <- seq(from, to, by = "days")

  missing <- sum(!sequence %in% time)

  if (missing > 0) warning(missing, " missing days in sequence.")


  ### 04 Create parameter list -------------------

  # create empty parameter list
  if (is.null(params)) params <- parameter(model = reftime, year = year)

  ### 05 Calculate Quercus phenology ------------------

  budswelling <- calc_phenology(x, params$budswelling)
  leafunfolding <- calc_phenology(x, params$leafunfolding)


  ### 06 Calculate hatch ---------------

  par_hatch <- c("custers", "wagenhoff", "meurisse")[c("custers", "wagenhoff", "meurisse") %in% names(params)]

  hatch <- list()
  hatch[par_hatch] <- lapply(params[par_hatch], function(y) calc_phenology(x, y))

  hatch <- subset_time(hatch, from = params$L2$ts_start, to = params$L2$ts_end)

  # If first, the model which calculates the earliest hatch is taken
  # If mean the mean day of the different models is taken

  # calculate hatch
  if (def_hatch == "first") L1 <- calc_first(hatch)
  if (def_hatch == "mean") L1 <- calc_mean(hatch)


  ### 07 L1 feeding start and ppa/biocide application ----------------------------

  # feeding starts with budswelling if hatch is not later
  L1_feeding_start <- terra::ifel(budswelling & L1, 1, 0)

  larvae <- c("L2", "L3", "L4", "L5", "L6", "Pp", "Ad")

  # add data to parameter lists
  # The development of the larvae starts with L1_feeding_start.
  params[larvae] <- lapply(params[larvae], function(y) {
    y$hatch <- L1_feeding_start
    return(y)
  })


  ### 08 calculate larval stages -------------------

  # calculate hatch-dependent models
  stages <- list("L1" = L1)

  temp_larvae <- if (reftime == "hour") x_hour else x

  stages[larvae] <- lapply(params[larvae], function(y) calc_phenology(temp_larvae, y))


  ### 09 mortality & PPA/biocide application --------------------------

  mortality <- calc_mortality(x, budswelling, L1)
  names(mortality) <- timename(mortality, "mortality")

  ppa_biocide <- terra::ifel(leafunfolding, 1, 0)
  ppa_biocide <- terra::ifel(stages$L4, 2, ppa_biocide)
  names(ppa_biocide) <- timename(ppa_biocide, "ppa_biocide")


  ### 10 Output -------------------------

  # for dataframes
  if (is_dataframe) {

    out <- c(
      hatch,
      "budswelling" = budswelling,
      "leafunfolding" = leafunfolding,
      "L1_feeding_start" = L1_feeding_start,
      "ppa_biocide_start" = NA,
      "ppa_biocide_end" = NA,
      stages)

    last_day <- max(get_time(stages))

    out$ppa_biocide_start <- ppa_biocide == 1
    out$ppa_biocide_end <- ppa_biocide == 2

    out <- lapply(out, convert_logical_to_time)

    out <- c(out, "mortality" = mortality)

    out <- lapply(out, function(x) as.numeric(terra::values(x)))

    out <- data.frame("model" = names(out), "day" = as.character(out), row.names = NULL)

    out[out$model != "mortality", "day"] <- as.character(lubridate::as_date(as.numeric(out[out$model != "mortality", "day"])))

    out <- rbind(out, data.frame(model = "last_day", day = as.character(last_day)))

  } else {

    # for SpatRaster

    stages <- Reduce(f = "+", stages)

    names(stages) <- timename(stages, "phenthau")

    out <- c(hatch,
             "budswelling" = budswelling,
             "leafunfolding" = leafunfolding,
             "ppa_biocide" = ppa_biocide,
             "mortality" = mortality,
             "stages" = stages)

  }

  return(out)
}



