#' installSavedPackages
#'
#' @description Re-install a set of saved packages (created with
#' \code{saveInstalledPackages()}) after updating R
#'
#' @author Yvonne Barkley and Selene Fregosi
#'
#' @param fileName filename of saved packages. This can be the full file path
#' with filename or if no path is specified it will look for the file in your
#' current working directory. You can use \code{getwd()} to check current
#' working directory)
#'
#' @export
#' @importFrom utils installed.packages install.packages update.packages
#'
# examples
# #re-install R packages that were saved to the desktop
# installSavedPackages('C:/Users/selene.fregosi/Desktop/installedPackages.rda')
#

installSavedPackages <- function(fileName){

  #check the file exists and choose one if it doesn't
  if (!file.exists(fileName)) {
    fileName <- file.choose()
  }
  #read in the file
  installedPkgs <- readRDS(fileName)
  #get a list of what packages are installed (with the new R version, should be
  #fewer packages, just the basics)
  tmp <- utils::installed.packages() # this should have fewer packages, just the basics
  installedPkgs_new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
  #figure out which are missing (old vs new)
  missing <- setdiff(installedPkgs, installedPkgs_new)
  #install and update the missing packages
  utils::install.packages(missing)
  utils::update.packages()

#for testing
  # return(missing)

}
