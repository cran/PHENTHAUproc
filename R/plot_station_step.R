#' Plot local PHENTHAUproc in a step plot
#'
#' @param x output of phenthau - dataframe - local PHENTHAUproc
#' @returns A plot showing local PHENTHAUproc results in a step plot.
#' @description Plots the development stages of OPM.
#' @importFrom rlang .data
#' @family Plot
#' @export
#' @examples
#'
#' fr_df <- load_test("day")
#' fr <- phenthau(fr_df)
#'
#' plot_station_step(fr)

plot_station_step <- function(x) {

  # required packages
  rlang::check_installed("ggplot2")

  # legend
  le <- get_legend("stages")
  le$id_col <- le$ID
  le$ID <- NULL

  # mortality
  # mortality <- as.numeric(x[x$model == "mortality", "day"])
  # x <- x[x$model != "mortality",]

  # limits and year
  x$day <- lubridate::ymd(x$day, quiet = TRUE)
  year <- unique(lubridate::year(na.omit(x$day)))
  from <- lubridate::ymd(paste0(year, "-03-01"))
  to <- lubridate::ymd(paste0(year, "-09-30"))
  last_day <- x[x$model == "last_day", "day"]

  # stages
  stages <- x[x$model %in% c("L1", "L2", "L3", "L4", "L5", "L6", "Pp", "Ad"), ]
  stages <- merge(le, stages, by.x = "category", by.y = "model", all.x = TRUE)
  stages <- stages[order(stages$id_col),]

  stages$xmin <- stages$day
  stages$xmin[1] <- from
  stages$xmax <- c(stages$xmin[-1], to)
  stages$ymin <- 0
  stages$ymax <- stages$id_col
  stages[!is.na(stages$xmin) & is.na(stages$xmax), "xmax"] <- last_day

  stages <- stages[!is.na(stages$xmin),]

  #ppa_biocide
  bio_start <- x[x$model == "ppa_biocide_start", "day"]
  bio_end <- x[x$model == "ppa_biocide_end", "day"]

  if (is.na(bio_end) & !is.na(bio_start)) bio_end <- last_day

  ## Plot
  # build boxes for stages
  g <- ggplot2::ggplot(data = stages) +

    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$xmin,
        xmax = .data$xmax,
        ymin = .data$ymin,
        ymax = .data$ymax),
      fill = stages$colors,
      alpha = 0.5
    ) +

    # build box for ppa_biocide
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = bio_start,
        xmax = bio_end,
        ymin = 0.25,
        ymax = 0.75,
        alpha = 0.5),
      colour = "green",
      fill = "green",
      show.legend = FALSE
    ) +

    # set x and y axis
    ggplot2::scale_y_continuous(
      breaks = 0:8,
      labels = c("Egg", "L1", "L2", "L3", "L4", "L5", "L6", "Pp", "Ad"),
      limits = c(0,8)
    ) +

    ggplot2::scale_x_date(
      date_breaks = "1 month",
      #date_labels = "%B",
      labels = \(x) month.name[lubridate::month(x)],
      limits = c(from, to)
    ) +

    # draw step line
    ggplot2::geom_step(
      ggplot2::aes(x = .data$xmin, y = .data$ymax),
      linewidth = 1,
      direction = "hv"
    ) +

    # set last segment of stepline
    ggplot2::geom_segment(
      data = stages[nrow(stages),],
      ggplot2::aes(
        x = .data$xmin,
        xend = .data$xmax,
        y = .data$id_col,
        yend = .data$id_col
      ),
      linewidth = 1
    ) +

    # text
    ggplot2::labs(
      x = "Month",
      y = "Stage",
      title = "Oak processionary moth",
      subtitle = paste0("Development - ", year)
    ) +

    # theme
    ggplot2::theme(
      legend.position = "none"
    ) +

    ggplot2::theme_bw()


  return(g)
}
