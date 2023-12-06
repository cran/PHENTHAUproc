#' Calculate PHENTHAUproc model
#'
#' @param x SpatRaster list/dataframe (tmean, tmax, tmin) - numeric - with time attribute/date column
#' @param par_budswelling parametrisation for bud swelling - character
#' @param par_leafunfolding parametrisation for leaf unfolding - character
#' @param par_hatch parametrisation for hatch - character
#' @param def_hatch definition of hatch - character - either "first" or "mean"
#' @returns dataframe/list with all PHENTHAUproc model outputs (see details)
#' @family Main
#' @description
#' "phenthau" implements Halbigs et al. 202x pre-warning system 'PHENTHAUproc' in R
#'
#' @details
#'
#' \strong{Overview}
#'
#' phenthau function combines multiple phenology models:
#' - buds welling & leaf unfolding of Quercus robur (or Quercus petraea)
#' - hatch of oak processionary moth using 3 different hatch models (Custers 2003, Wagenhoff et al. 2014 and Meurisse et al. 2012)
#' - development stages of OPM (0 Egg - 8 Adult)
#'
#' \strong{Parametrisation}
#'
#' Use parameter() to return a dataframe with all possible parametrisation options.
#' - par_budswelling: until now only Quercus robur
#' - par_leafunfolding: Quercus robur and Quercus petraea. Should be chosen dependent on dominant oak species in region
#' - par_hatch: used hatch models
#' - def_hatch: how to calculate hatch. if "first" the first hatch which is predicted by any hatchmodel is taken. If "mean" the mean day of all hatch models is taken.
#'
#' \strong{Output}
#'
#' phenthau returns a list with all model calculations as list objects:
#' - stages: SpatRaster with one layer per day. values from 0-8 0 Egg state 8 Adult stage.
#'  -> use get_legend("stages") to show id/cover/colors
#' - custers/wagenhoff/meurisse - logical - TRUE/FALSE hatch/no_hatch
#' - budswelling: - logical - TRUE/FALSE budswelling/no_budswelling
#' - leafunfolding: - logical - TRUE/FALSE leafunfolding/no_leafunfolding
#'
#'   -> plot first day of a logical SpatRaster with plot_date
#'
#' - mortality - integer - mortality in %
#' - ppa_biocide - numeric - 0 application for PPA/biocides not useful yet, 1 application possible, 2 application not useful anymore
#'
#' \strong{Presentation}
#'
#' - plot_stages is a wrapper around terra::plot to preset legend, names, colors and day to plot.
#' - plot_date is a wrapper around terra::plot to plot the date of first TRUE in multiple layered SpatRaster
#' - plot_station is for local weatherstation data and creates a Phenology wheel (ggplot2 & geomtextpath required!)
#' - plot_station_step is for local weatherstation data and creates a stepwise graph for the development stages
#'
#' @references Halbig, P. (2021), Model development for hazard assessment of oak
#'   processionary moth (Thaumetopoea processionea). Dissertation, University of Natural Resources and Life Sciences Vienna. 326 S.
#' @author Bachfischer Lorenz, Department forest protection FVA (2023)
#'   \email{lorenz.bachfischer@@posteo.de}
#' @export
#' @examples
#'
#' srl <- load_test()
#' phen <- phenthau(srl)
#'

phenthau <- function(x,
                     par_budswelling = "quercus_robur_type1",
                     par_leafunfolding = "quercus_robur_type1",
                     par_hatch = c("custers", "wagenhoff", "meurisse"),
                     def_hatch = "first"
) {

  ### 1 check and convert input -------------


  is_hour <- NULL

  if (all(c("date", "hour", "tmean") %in% names(x))) {
    is_hour <- TRUE
  } else if (all(c("tmean", "tmin", "tmax") %in% names(x))) {
    is_hour <- FALSE
  } else {
    stop("names(x) not like needed. See details.")
  }

  # convert data.frame into list of SpatRaster (srl)
  if (methods::is(x, "data.frame")) {

    x <- convert_df_to_srl(x[, names(x) != "hour"])

    is_dafr <- TRUE # if input is a dataframe, output should be too.

  } else is_dafr <- FALSE

  # transfer hourly to daily minmeanmax (needed for most of the models)
  if (is_hour) {

    x_hour <- list("tmean" = x)
    x <- convert_hour_to_meanminmax(x)

  }

  # write the times with lubridate
  x <- lapply(x, function(y) {
    terra::time(y) <- lubridate::as_date(get_time(y), tz = NULL)
    return(y)
  })


  # check if days complete
  time <- get_time(x)
  year <- lubridate::year(max(time))
  from <- lubridate::as_date(paste0(year - 1, "-09-01"), tz = NULL)
  to <- lubridate::as_date(paste0(year, "-09-30"), tz = NULL)

  if (to > max(time)) to <- max(time)

  sequence <- seq(from, to, by = "days")

  missing <- sum(!sequence %in% time)

  if (missing > 0) warning(missing, " missing days in sequence.")


  ### 2 create/read parameter -------------------

  # create empty parameter list
  params <- list()
  params$budswelling <- parameter("budswelling", par_budswelling, year)
  params$leafunfolding <- parameter("leafunfolding", par_leafunfolding, year)
  params[par_hatch] <- lapply(par_hatch, function(x) parameter("hatch", x, year))

  larvae <- c("L2", "L3", "L4", "L5", "L6", "Pp", "Ad")

  par_larvae <- if (is_hour) "hour" else "degreedays"
  params[larvae] <- lapply(larvae, parameter, parametrisation = par_larvae, year = year)


  ### 3 quercus phenology ------------------

  budswelling <- calc_phenology(x, params$budswelling)
  leafunfolding <- calc_phenology(x, params$leafunfolding)


  ### 4 hatch ---------------

  hatch <- list()
  hatch[par_hatch] <- lapply(params[par_hatch], function(y) calc_phenology(x, y))

  hatch <- subset_time(hatch, from = params$L2$ts_start, params$L2$ts_end)

  # calculate hatch
  # first is first hatch which is calculated by any model is taken as hatch
  # mean is the mean of different hatch models is taken as hatch

  # calculate hatch
  if (def_hatch == "first") L1 <- calc_first(hatch)
  if (def_hatch == "mean") L1 <- calc_mean(hatch)

  ### 5 feeding start and ppa/biocide application ----------------------------

  # feeding starts with budswelling if hatch is not later
  L1_feeding_start <- terra::ifel(budswelling & L1, 1, 0)

  # add data to parameter lists
  params[larvae] <- lapply(params[larvae], function(y) {
    y$hatch <- L1_feeding_start # confusing. should be changed! hatch here is not L1 but describes starting point of calculation for function calc_degreedays.
    return(y)
  })


  ### 6 Larvae, Puppe and Adult stages -------------------

  # calculate hatch-dependent models
  stages <- list("L1" = L1)

  temp_larvae <- if (is_hour) x_hour else x

  stages[larvae] <- lapply(params[larvae], function(y) calc_phenology(temp_larvae, y))


  ### 7 Mortality & PPA/biocide application --------------------------

  mortality <- calc_mortality(x, budswelling, L1)
  names(mortality) <- timename(mortality, "mortality")

  ppa_biocide <- terra::ifel(leafunfolding, 1, 0)
  ppa_biocide <- terra::ifel(stages$L4, 2, ppa_biocide)
  names(ppa_biocide) <- timename(ppa_biocide, "ppa_biocide")


  ### 8 Output -------------------------

  # for dataframes
  if (is_dafr) {

    dafr <- c(
      hatch,
      "budswelling" = budswelling,
      "leafunfolding" = leafunfolding,
      "L1_feeding_start" = L1_feeding_start,
      "ppa_biocide_start" = NA,
      "ppa_biocide_end" = NA,
      stages)

    last_day <- max(get_time(stages))

    dafr$ppa_biocide_start <- ppa_biocide == 1
    dafr$ppa_biocide_end <- ppa_biocide == 2

    dafr <- lapply(dafr, convert_logical_to_time)

    dafr <- c(dafr, "mortality" = mortality)

    dafr <- lapply(dafr, function(x) as.numeric(terra::values(x)))

    dafr <- data.frame("model" = names(dafr), "day" = as.character(dafr), row.names = NULL)

    dafr[dafr$model != "mortality", "day"] <- as.character(lubridate::as_date(as.numeric(dafr[dafr$model != "mortality", "day"])))

    dafr <- rbind(dafr, data.frame(model = "last_day", day = as.character(last_day)))

    return(dafr)

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

    return(out)
  }
}



