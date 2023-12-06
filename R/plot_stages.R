#' Plot regional 'PHENTHAUproc' - Stages
#'
#' @param x SpatRaster stages output of phenthau - numeric
#' @param time day to plot - Date or ymd character
#' @param ... arguments passed along to terra::plot
#' @returns A plot of the 'PHENTHAUproc' stages of the last/chosen time with preset levels and colors.
#' @family Plot
#' @description
#' wrapper to plot stages of OPM with assigned names and colors
#'
#' @details phenthau returns a list of SpatRasters. The stages object is describing the development stages of oak processionary moth.
#' This function plots the stages SpatRaster with the right names and colors. to see the assignment check get_legend("stages")
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

