#' saveInstalledPackages
#'
#' @description Save a vector of currently installed R packages as a RDS named
#' fileName.Run this before updating R, then update R, then run
#' \code{installSavedPackages} to re-install all packages
#'
#' @author Yvonne Barkley
#'
#' @param fileName filename to save to (can be full file path with name or if
#' just a name is specified then will save in the current working directory use
#' \code{getwd()} to check current working directory).
#'
#' @returns installedPkgs a dataframe of effort tracks with date and lat/lon
#'
#' @export
#'
# examples
# #save installed packages to the desktop
# saveInstalledPackages('C:/Users/selene.fregosi/Desktop/installedPackages.rda')
#

saveInstalledPackages <- function(fileName){

  #find all packages
  tmp <- installed.packages()
  #convert to vector
  installedPkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
  #save it (as RDS for best practice)
  saveRDS(installedPkgs, file = fileName)

  return(installedPkgs)

}
