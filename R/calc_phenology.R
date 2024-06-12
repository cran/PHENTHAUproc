#' Calculate phenological models
#'
#' @param x SpatRaster list - with tmean, tmin, tmax and time attribute
#' @param params parameter list
#' @returns SpatRaster - logical - with event occurred/not TRUE/FALSE
#' @family Models
#' @keywords internal
#' @description This function is checking the dependencies of the model and
#' dependent on them:
#'
#' 1 Calculate the needed sum of effective (SET) temperatures for the
#' phenological event to happen
#' 2 Calculate the summed degree days (depending on hatch or not)
#' 3 Check if the summed degree days have reached the needed sum of effective
#' temperatures

calc_phenology <- function(x,
                           params) {

  ### 01 check parameter list for data input dependencies ----------------------

  check_data_with_params(x, params)

  # check dependencies
  cf_dependent <- params$cf_dependent
  hatch_dependent <- params$hatch_dependent

  ### 02 calculate sum of effective temperatures -------------------------------

  if (cf_dependent) {

    args <- get_formalArgs(params, calc_sumefftemp)
    args$x <- x
    sumefftemp <- do.call(calc_sumefftemp, args)
    sumefftemp <- subset_time(sumefftemp, params$ts_start, params$ts_end)
    rm(args)

  } else{

    sumefftemp <- params$a

  }

  ### 03 calculate degree days -------------------------------------------------

  args <- get_formalArgs(params, calc_degreedays)

  if (hatch_dependent) {

    args$x <- remove_false(x, params$hatch)

  } else args$x <- x

  degreedays <- do.call(calc_degreedays, args)

  ### 04 check if phenological event happened ----------------------------------

  event <- terra::ifel(degreedays > sumefftemp, TRUE, FALSE)

  # summarise hourly data if necessary
  event <- terra::tapp(event, index = "days", fun = "any")

  # create layernames
  names(event) <- timename(event, params$model)

  return(event)

}
