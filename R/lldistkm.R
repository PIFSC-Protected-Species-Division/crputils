#' Calculate distance in km between to lat/lons
#'
#' @description Utility to calculate the distance (in kilometers) between a pair
#' of latitude/longitude coordinates. It returns two distances: d1km which is
#' calculated using the Haversine formula which accounts for the shape of the
#' earth and d2km based on Pythagoras' theorem and is not accurate over long
#' distances. Over short distances d1km and d2km will be very similar.
#'
#' @author Selene Fregosi
#'
#' @param latlon1 1-by-2 numeric vector of lat/lon coordinates (in decimal
#' degrees) with latitude first, longitude second
#' e.g., \code{c(21, -157)}\cr
#' @param latlon2 1-by-2 numeric vector of lat/lon coordinates (in decimal
#' degrees) with latitude first, longitude second
#' e.g., \code{c(21, -157)}\cr
#'
#' @returns list with d1km (distance calculated by haversine formula) and d2km
#' (distance calculated based on Pythagorean theorem)
#'
#' @export
#'
#' @examples
#' #short distance
#' #create input coordinates
#' ll1 <- c(21, -157)
#' ll2 <- c(22, -158)
#' #calculate distance
#' dkm <- lldistkm(ll1, ll2)
#' dkm
#' # $d1km
#' # [1] 151.8795
#' #
#' # $d2km
#' # [1] 151.8809
#'
#' #long distance
#' #calculate distance
#' dkm <- lldistkm(c(21, -157), c(13, 144))
#' dkm
#' # $d1km
#' # [1] 6304.003
#' #
#' # $d2km
#' # [1] 32019.57
#' #d2km not acccurate at longer distances

lldistkm <- function(latlon1, latlon2){

  radius <- 6371 # radius of the earth

  #separate and convert to radians
  lat1 <- latlon1[1]*pi/180
  lat2 <- latlon2[1]*pi/180
  lon1 <- latlon1[2]*pi/180
  lon2 <- latlon2[2]*pi/180

  #find difference
  deltaLat <- lat2-lat1
  deltaLon <- lon2-lon1

  #Haversine distance
  a <- sin((deltaLat)/2)^2 + cos(lat1)*cos(lat2) * sin(deltaLon/2)^2
  c <- 2*atan2(sqrt(a), sqrt(1-a))
  d1km <- radius*c

  #Pythagorean distance
  x <- deltaLon*cos((lat1+lat2)/2)
  y <- deltaLat
  d2km <- radius*sqrt(x*x + y*y)

  dkm <- list(d1km = d1km, d2km = d2km)

}
