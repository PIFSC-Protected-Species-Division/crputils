% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/saveInstalledPackages.R
\name{saveInstalledPackages}
\alias{saveInstalledPackages}
\title{saveInstalledPackages}
\usage{
saveInstalledPackages(fileName)
}
\arguments{
\item{fileName}{filename to save to (can be full file path with name or if
just a name is specified then will save in the current working directory use
\code{getwd()} to check current working directory).}
}
\value{
installedPkgs a dataframe of effort tracks with date and lat/lon
}
\description{
Save a vector of currently installed R packages as a RDS named
fileName.Run this before updating R, then update R, then run
\code{installSavedPackages} to re-install all packages
}
\author{
Yvonne Barkley
}
