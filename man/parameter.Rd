% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameter.R
\name{parameter}
\alias{parameter}
\title{Create parameter list}
\usage{
parameter(model = NULL, parametrisation = NULL, year = NULL, first = TRUE)
}
\arguments{
\item{model}{type of model for phenthau function - character}

\item{parametrisation}{type of parametrisation - character}

\item{year}{year of prognosis - numeric - used to transform ts_start, ts_end, cf_start, cf_end to date format}

\item{first}{logical - If TRUE and parametrisation is missing takes first in list}
}
\value{
If model or parametrisation or year is missing returns a dataframe of available parameter settings.
If all are specified returns a list of parameter (used inside phenthau function).
}
\description{
See all available models and parametrisations: parameter()
}
\examples{
# return parameter list
parameter()

# return all hatch model parameter
parameter("hatch")

# return parameter necessary for calculation
parameter("hatch", "custers", 2020)
}
\seealso{
Other Main: 
\code{\link{get_legend}()},
\code{\link{mortality}()},
\code{\link{phenology}()},
\code{\link{phenthau}()}
}
\concept{Main}
