#' Subset data.frame/SpatRaster/List within from and end time
#'
#' @param x data.frame with time column, SpatRaster with time attribute or List of SpatRasters with same time attributes
#' @param from first day of subset
#' @param to last day of subset
#' @family Helper
#' @keywords internal


subset_time <- function(x, from, to) {

  from <- lubridate::ymd(from)
  to <- lubridate::ymd(to)

  # create sequence
  sequence <- seq(from, to, by = "days")

  # choose subset depending on type
  if (methods::is(x, "data.frame")) x <- x[lubridate::ymd(x$time) %in% sequence, ]

  # create spatraster subset function
  f <- function(y)  terra::subset(y, which(terra::time(y) %in% sequence))

  if (methods::is(x, "SpatRaster")) x <- f(x)

  if (methods::is(x, "list")) x <- lapply(x, f)

  if (methods::is(x, "SpatRasterDataset")) {

    names <- names(x)

    x <- lapply(x, f)
    x <- terra::sds(x)

    names(x) <- names

  }

  return(x)
}


