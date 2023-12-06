#' Calculating starvation related mortality of Thaumetopoea processionea
#'
#' @param x SpatRaster list (tmean, tmax, tmin) - numeric - with time attribute
#' @param par_budswelling character - parametrisation for bud swelling see parameter()
#' @param par_hatch character - parametrisation for hatch see parameter()
#' @param def_hatch definition of hatch - character - either "first" or "mean"
#' @param last If TRUE returns only the last day
#' @returns SpatRaster with mortality in %.
#' @description Calculating starvation related mortality rate, dependent on degree days between the first hatch and feeding start (budswelling).
#' @family Main
#' @export
#' @examples
#' \donttest{
#' srl <- load_test()
#' mortality(srl)
#'}

mortality <- function(x,
                      par_budswelling = "quercus_robur_type1",
                      par_hatch = c("custers", "wagenhoff", "meurisse"),
                      def_hatch = "first",
                      last = TRUE) {

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
    terra::time(y) <- lubridate::ymd(get_time(y))
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

  ### 02 Calculate Phenology models ------------------------------

  budswelling <- phenology(x,
                           model = "budswelling",
                           parametrisation = par_budswelling,
                           year = year,
                           return_date = FALSE
                           )

  hatchmodels <- lapply(par_hatch, function(y) phenology(x, "hatch", y, year, return_date = FALSE))

  # calculate hatch
  if (def_hatch == "first") hatch <- calc_first(hatchmodels)
  if (def_hatch == "mean") hatch <- calc_mean(hatchmodels)

  mortality <- calc_mortality(x, budswelling, hatch)

  if (last) mortality <- mortality[[terra::nlyr(mortality)]]

  if (is_dafr) {

    mortality <- as.numeric(terra::values(mortality))
    names(mortality) <- "mortality"

  }

  return(mortality)
}
