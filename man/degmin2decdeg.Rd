% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/degmin2decdeg.R
\name{degmin2decdeg}
\alias{degmin2decdeg}
\title{Degrees decimal minutes to decimal degrees}
\usage{
degmin2decdeg(degmin)
}
\arguments{
\item{degmin}{N-by-2 matrix of coordinates with each column representing
degrees and minutes.
e.g., \code{matrix(c(30, 29.2020), nrow = 1, ncol = 2)}
e.g., \code{matrix(c(30, 29.2020,
               -155, 36.8973),
             nrow = 2, ncol = 2, byrow = TRUE)}}
}
\value{
N-by-1 vector of coordinates in decimal degrees
}
\description{
Utility to convert latitude and longitude coordinates from
degrees minutes to decimal degrees
}
\examples{
# single input coordinates
degmin <- matrix(c(30, 29.2020), nrow = 1, ncol = 2)
decdeg <- degmin2decdeg(degmin)

# multiple input coordinates
degmin <- matrix(c(30, 29.2020,
                     -155, 36.8973),
                   nrow = 2, ncol = 2, byrow = TRUE)
decdeg <- degmin2decdeg(degmin)

}
\author{
Selene Fregosi
}
