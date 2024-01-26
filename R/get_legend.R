#' Get legend for PHENTHAUproc models
#'
#' @param x legend name - character - Available legends: "stages", "mortality", "ppa_biocide"
#' @returns dataframe with ID, category and colors
#' @family Main
#' @export
#' @examples
#' # return legend for development stages
#' get_legend("stages")
#'

get_legend <- function(x = "stages") {

  av_leg <- c("stages", "mortality", "ppa_biocide")

  if (!x %in% av_leg) message("Available legends:\n", paste(av_leg, "\n"))

  if (x == "stages") {

    legend <- data.frame(
      "ID" = c(0:8),
      "category" = c("Egg", "L1", "L2", "L3", "L4", "L5", "L6", "Pp", "Ad"),
      "colors" = c(NA, "grey", "dimgrey", "lightyellow", "yellow", "orange", "red", "lightgrey", "darkgrey")
    )

  }

  if (x == "mortality") {

    legend <- data.frame(
      "ID" = c(0, 5, 10, 25, 50, 75, 100),
      "category" = as.character(c(0, 5, 10, 25, 50, 75, 100)),
      "colors" = rev(grDevices::hcl.colors(7, "Reds 2"))
    )

  }

  if (x == "ppa_biocide") {

    legend <- data.frame(
      "ID" = c(0, 1, 2),
      "category" = c("not yet effective", "possible", "not effective anymore"),
      "colors" = c("white", "forestgreen", "yellow")
    )

  }

  return(legend)

}
