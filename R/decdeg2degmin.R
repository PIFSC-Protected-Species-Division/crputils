decdeg2degmin <- function(decdeg){

  #' decdeg2degmin
  #'
  #' @description Utility to convert latitude and longitude coordinates from
  #' decimal degrees to degrees decimal minutes
  #'
  #' author: Selene Fregosi selene.fregosi at noaa.gov
  #' date: 28 July 2023
  #'
  #' @param decdeg N-by-1 vector of coordinates in decimal degrees
  #' e.g., 30.48667
  #' e.g., c(30.48667, -155.61496)
  #'
  #' @usage decdeg2degmin(decdeg)
  #'
  #' @return degmin N-by-2 data.frame of coordinates with columns 'deg' for
  #' degrees and 'min' for decimal minutes
  #'
  #' @export
  #'
  #' @examples
  #' # # single input coordinates
  #' decdeg = 30.48667
  #' degmin = decdeg2degmin(decdeg)
  #'
  #' # # multiple input coordinates
  #' decdeg = c(30.48667, -155.61496)
  #' degmin = decdeg2degmin(decdeg)
  #'
  #' ######################################################################

  # check input type
  # if just a value, convert to matrix
  if (!is.matrix(decdeg)){
    ddl = length(decdeg)
    decdeg = matrix(decdeg, nrow = ddl, ncol = 1, byrow = TRUE)
  }
  # otherwise it should already be a matrix?

  # set up empty output
  degmin = data.frame(deg = matrix(NA, nrow = ddl), min = matrix(NA, nrow = ddl))

  # loop through inputs and convert
  for (f in 1:nrow(decdeg)){
    deg = trunc(decdeg[f])
    dec = abs(decdeg[f] - deg)
    min = dec*60

    degmin$deg[f] = deg
    degmin$min[f] = min
  }

  return(degmin)
}
