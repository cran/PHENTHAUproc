% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_hour_to_meanminmax.R
\name{convert_hour_to_meanminmax}
\alias{convert_hour_to_meanminmax}
\title{Convert hourly to daily}
\usage{
convert_hour_to_meanminmax(x)
}
\arguments{
\item{x}{SpatRaster - numeric - hourly tmean with time attribute}
}
\value{
SpatRaster list (tmean, tmax, tmin) - numeric  - with time attribute
}
\description{
Convert hourly to daily
}
\seealso{
Other SpatRaster transformation: 
\code{\link{convert_df_to_srl}()},
\code{\link{convert_doy_to_logical}()},
\code{\link{convert_logical_to_doy}()},
\code{\link{convert_logical_to_time}()},
\code{\link{convert_sr_to_cvec}()}
}
\concept{SpatRaster transformation}
\keyword{internal}
