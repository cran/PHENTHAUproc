% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_sr_to_cvec.R
\name{convert_sr_to_cvec}
\alias{convert_sr_to_cvec}
\title{Convert SpatRaster to characer vector}
\usage{
convert_sr_to_cvec(x)
}
\arguments{
\item{x}{SpatRaster - numeric - value is timeserialnumber}
}
\value{
vector - Date as character
}
\description{
Transform local PHENTHAUproc results back from SpatRaster to vector format
}
\seealso{
Other SpatRaster transformation: 
\code{\link{convert_df_to_srl}()},
\code{\link{convert_doy_to_logical}()},
\code{\link{convert_hour_to_meanminmax}()},
\code{\link{convert_logical_to_doy}()},
\code{\link{convert_logical_to_time}()}
}
\concept{SpatRaster transformation}
\keyword{internal}
