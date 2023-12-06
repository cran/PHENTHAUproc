#' Plot SpatRaster with date serial number/time attribute
#'
#' @param x SpatRaster - numeric - value is serial number or SpatRaster - logical - with time attribute
#' @param breaks number of breaks in the legend - numeric
#' @param ... arguments for terra::plot function, except (type, breaks, col or plg)
#' @returns A plot of a phenological event by day with legend.
#' @description A wrapper around terra::plot to show the time serial number as a character date in the legend.
#' @family Plot
#' @export
#' @examples
#'
#' data <- load_test()
#' budswelling <- phenology(data, "budswelling", "quercus_robur_type1", 2020)
#' plot_date(budswelling)

plot_date <- function(x,
                      breaks = NULL,
                      ...) {

  if (terra::nlyr(x) > 1) x <- convert_logical_to_time(x)

  first <- terra::minmax(x)["min", ]
  last <- terra::minmax(x)["max", ]

  if (is.null(breaks)) breaks <- last - first
  while (breaks > 20) breaks <- round(breaks / 2)

  days <- round(seq(first, last, length.out = breaks))
  days <- lubridate::as_date(days)


  terra::plot(x,
    type = "interval",
    breaks = breaks,
    plg = list(legend = days),
    ...
  )
}


