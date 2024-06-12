#' Plot regional PHENTHAUproc - Stages
#'
#' @param x SpatRaster stages output of phenthau - numeric
#' @param time day to plot - Date or character year-month_day i.e.("2020-05-01")
#' @param ... arguments passed along to terra::plot
#' @returns A plot of the PHENTHAUproc stages of the last/chosen time with
#' preset levels and colors.
#' @family Plot
#' @description
#' A wrapper to plot the development stages of OPM with assigned names and
#' colors
#'
#' @details phenthau returns a list of SpatRasters. The stages object describes
#' the development stages of oak processionary moth.
#' This function plots the stages SpatRaster with the right names and colors.
#' To get IDs, caregories and colors use get_legend("stages").
#'
#' @export

plot_stages <- function(x,
                        time = NULL,
                        ...) {

  if (is.null(time)) time <- max(terra::time(x))
  if (terra::nlyr(x) > 1) x <- terra::subset(x, which(terra::time(x) == time))

  leg <- get_legend("stages")

  levels(x) <- leg[,c("ID", "category")]
  terra::coltab(x) <- leg[,c("ID", "colors")]

  terra::plot(x,
              all_levels = TRUE,
              ...)

}

