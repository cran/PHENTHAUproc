#' Plot local PHENTHAUproc in a circle plot
#'
#' @param x output of phenthau - dataframe - local PHENTHAUproc
#' @param main title - character
#' @returns A plot showing local PHENTHAUproc results in a circle plot presentation.
#' @family Plot
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @export
#' @references
#'
#' Allan Cameron 2022: Allan Cameron (2022), Answer to the question "Is it possible to create a circular timeline plot with ggplot?" on Stackoverflow, (<https://stackoverflow.com/questions/71023419/is-it-possible-to-create-a-circular-timeline-plot-with-ggplot>)
#'
#' @examples
#'
#' # load daily test data from Freiburg
#' fr_df <- load_test("day")
#' fr <- phenthau(fr_df)
#'
#' plot_station(fr)


plot_station <- function(x, main = "PHENTHAUproc") {

  ### 00 check packages ------------------------

  rlang::check_installed("ggplot2")
  rlang::check_installed("geomtextpath")

  # create decimal function for circle plot
  decimal <- function(x) lubridate::decimal_date(x) %% 1

  ### 01 preprepare data --------------------

  # set colors
  colrs <- get_legend("stages")[,c("category", "colors")]
  x <- merge(x, colrs, by.x = "model", by.y = "category", all = TRUE, sort = FALSE)

  x[x$model == "budswelling", "colors"] <- "brown"
  x[x$model == "leafunfolding", "colors"] <- "darkgreen"
  x[x$model == "L1_feeding_start", "colors"] <- "lightgreen"

  # hatch
  hatch <- x[x$model == "L1",]
  hatch$model <- "hatch"
  hatch$colors <- "orange"
  x <- rbind(x, hatch)

  # days
  x$day <- lubridate::ymd(x$day, quiet = TRUE)
  year <- unique(lubridate::year(stats::na.omit(x$day)))
  last_day <- x[x$model == "last_day", "day"]

  # categories
  # divide by type of plot
  stages <- c("L1", "L2", "L3", "L4", "L5", "L6", "Pp", "Ad")
  events <- c("budswelling", "leafunfolding", "hatch")

  # ppa/biocide
  kill_start <- x[x$model == "ppa_biocide_start", "day"]
  kill_end <- x[x$model == "ppa_biocide_end", "day"]

  if (is.na(kill_end) & !is.na(kill_start)) kill_end <- last_day

  # prepare stages
  Stages <- x[x$model %in% stages,]
  Stages$start <- decimal(Stages$day)
  Stages$end <- decimal(c(Stages$day[-1], lubridate::ymd(paste0(year, "-10-01"))))
  Stages$end[is.na(Stages$end & !is.na(Stages$start))] <- decimal(last_day)
  Stages$middle <- (Stages$end - Stages$start)/2 + Stages$start

  Stages <- Stages[!is.na(Stages$start),]

  # prepare events
  Events <- x[x$model %in% events,]
  Events$x <- decimal(Events$day)
  Events$name <- c("bud swelling", "leaf unfolding", "hatch")

  # ppa_biocide
  Pesticides <- data.frame(start = decimal(kill_start),
                           end = decimal(kill_end))

  # prepare Months
  months <- month.name
  Months <- data.frame(months = toupper(months))
  Months$start <- lubridate::ymd(paste(year, month.name, 01, sep = "-"))
  Months$end <- lubridate::ceiling_date(Months$start, "month")
  Months$start <- decimal(Months$start)
  Months$end <- decimal(Months$end)
  Months$end[12] <- 1
  Months$middle <- (Months$end - Months$start)/2 + Months$start


  ### 03 Plotting ---------------

  g <- ggplot2::ggplot() +

    # add background colors for stages
    ggplot2::geom_rect(
      data = Stages,
      ggplot2::aes(
        xmin = .data$start,
        xmax = .data$end,
        ymin = 0,
        ymax = 0.95
      ),
      fill = Stages$colors,
      col = Stages$colors
    ) +

    # set limits
    ggplot2::scale_x_continuous(
      limits = c(0, 1)
    ) +

    # add text for Stages
    geomtextpath::geom_textpath(
      data = Stages,
      ggplot2::aes(
        x = .data$middle,
        y = 0.9,
        label = .data$model
      ),
      fontface = 2
    ) +

    # add circle lines and box
    ggplot2::geom_hline(
      yintercept = c(0.95, 1.05)
    ) +

    # outlines
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = 1.05,
        ymax = 1.07
      ),
      colour = "black",
      fill = "black",
      show.legend = FALSE
    ) +

    # add pesticide bar
    ggplot2::geom_rect(
      data = Pesticides,
      ggplot2::aes(
        xmin = .data$start,
        xmax = .data$end,
        ymin = 0.125,
        ymax = 0.25
      ),
      colour = "green",
      fill = "green",
      show.legend = FALSE
    ) +

    # add seperators between months
    ggplot2::geom_segment(
      data = Months,
      ggplot2::aes(
        x = .data$start,
        xend = .data$start,
        y = 0.95,
        yend = 1.05)
    ) +

    # add months
    geomtextpath::geom_textpath(
      data = Months,
      ggplot2::aes(
        x = .data$middle,
        y = 1,
        label = .data$months
      ),
      fontface = 2
    ) +

    # add vertical event lines
    ggplot2::geom_segment(
      data = Events,
      ggplot2::aes(
        x = .data$x,
        xend = .data$x,
        y = 0,
        yend = 0.9
      ),
      color = Events$colors
    ) +

    # add event text
    geomtextpath::geom_textsegment(
      data = Events,
      ggplot2::aes(
        x = .data$x,
        xend = .data$x,
        y = 0,
        yend = 0.95,
        label = .data$name
      ),
      vjust = -0.1,
      color = Events$colors
    ) +

    # add starttime of stages
    geomtextpath::geom_textpath(
      data = Stages,
      ggplot2::aes(
        x = .data$start,
        y = 0.8,
        label = format(.data$day, "%d. %b")
      ),
      vjust = 1.1,
      hjust = 0.95,
      size = 4,
      angle = 90,
      color = "black"
    ) +

    # clear and circle it
    ggplot2::coord_polar() +
    ggplot2::theme_void() +

    # add main
    ggplot2::geom_text(
      ggplot2::aes(
        x = 0,
        y = 0.5,
        label = paste0(main, "\n", year)
      ),
      size = 5,
      fontface = "bold",
      color = "black"
    )

  return(g)

}


