#' workflow to save and re-install packages when updating R
#'
#' Before updating R, run saveInstalledPackages, then after updating R, run
#' installSavedPackages to re-install all your previously installed packages
#'
#' Code first developed by Yvonne Barkley and functionized by S. Fregosi
#'
#' author: Selene Fregosi selene.fregosi [at] noaa.gov
#' date: 28 February 2025
#'

# install crputils package
install.packages('crputils',
                 repos=c('https://pifsc-protected-species-division.r-universe.dev',
                         'https://cloud.r-project.org'))


# set a filename/path to save your list of packages
pkgFile <- file.path('C:/Users/User.Name/Desktop/installedPackages.rda')

# save current packages
saveInstalledPackages(pkgFile)

# Update R!

# re-install saved packages
installSavedPackages(pkgFile)
