#' Calculate phenological events
#'
#' @param x SpatRaster list/dataframe (tmean, tmax, tmin) - numeric - with time attribute/date column
#' @param model name of model - character
#' @param parametrisation name of parametrisation - character
#' @param year year for prognosis - numeric
#' @param hatch SpatRaster - logical - with time attribute TRUE/FALSE hatch/no_hatch
#' @param return_date returns start of phenological event with time serial number
#' @param ... parameter to change default values. (i.e. ldt = 3.5)
#' @returns If return_date is TRUE returns single layered SpatRaster with time serial number (first occurence of phenological event).
#' If return_date is FALSE returns a one layer per day SpatRaster type logical with phenological event occurred/not TRUE/FALSE.
#' @family Main
#' @description Using daily mean or min and max temperature data, the function calculates the temperature-dependent development stages of OPM or the bud stages (bud swelling and leaf unfolding) of its host tree \emph{Quercus robur}.
#'
#' The default settings correspond to the model described by Halbig et al. 2024.
#' Additional parametrizations are provided but have not yet been tested.
#'
#'  Halbig et al. 2024
#'   It follows 4 different steps:
#'   - a) Calculating and summing up cold days or frost days.
#'   (Cold days are defined as days with a mean temperature below ldt (lower development threshold), while frost days are all days with a min temperature below ldt).
#'      Hatch dependent development stages need a hatch raster (hatch happened 1 or not 0) for each day
#'   - b) Calculating degree days with the
#'   single sine method of Baskerville & Emin, 1969 or simple summing up tmean temperatures over ldt.
#'   - c) Calculating the needed
#'   sum of effective temperatures for the development stage
#'   - d) Comparing degree days with the needed sum of effective temperatures
#'
#' @references
#'
#' Halbig et al. 2014: Halbig, P., Stelzer, A. S., Baier, P., Pennerstorfer, J., Delb, H., & Schopf, A. (2024). PHENTHAUproc–An early warning and decision support system for hazard assessment and control of oak processionary moth (Thaumetopoea processionea). Forest Ecology and Management, 552, 121525
#' Baskerville & Emin 1969: Baskerville, G. L., & Emin, P. (1969). Rapid estimation of heat accumulation from maximum and minimum temperatures. Ecology, 50(3), 514-517. (<doi:10.2307/1933912>)
#' Menzel 1997: Menzel, A. (1997). Phänologie von Waldbäumen unter sich ändernden Klimabedingungen: Auswertung der Beobachtungen in den internationalen phänologischen Gärten und Möglichkeiten der Modellierung von Phänodaten. Frank.
#'
#' @author Bachfischer Lorenz, Department of Forest Protection FVA (2024)
#'   \email{lorenz.bachfischer@@posteo.de}
#' @export
#' @examples
#' ## SpatRaster
#' srl <- load_test()
#'
#' # Calculating bud swelling for our raster example
#' budswelling <- phenology(srl, "budswelling", "quercus_robur_clone256_type1", year = 2020)
#'


phenology <- function(x,
                      model,
                      parametrisation = NULL,
                      year = NULL,
                      hatch = NULL,
                      return_date = TRUE,
                      ...
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


  ### 02 create parameter list ---------------------------

  params <- parameter(model = model,
                      parametrisation = parametrisation,
                      year = year)[[1]]

  if (!is.null(hatch)) params$hatch <- hatch

  my_params <- list(...)
  params <- replace(params, names(my_params), my_params)


  ### 03 calculate phenology ------------

  out <- calc_phenology(x, params)

  names(out) <- timename(out, model)

  ### 04 create output -----------------

  if (return_date) out <- convert_logical_to_time(out)

  if (is_dafr) {

    out <- convert_sr_to_cvec(out)
    names(out) <- model

  }

  return(out)
}
