decdeg2degminsec <- function(decdeg){
  #' decdeg2degminsec
  #'
  #' @description Utility to convert latitude and longitude coordinates from
  #' degrees minutes to degrees minutes second
  #'
  #' @param decdeg N-by-1 vector of coordinates in decimal degrees
  #' e.g., 30.48667
  #' e.g., c(30.48667, -155.61496)
  #' @usage decdeg2degminsec(decdeg)
  #' @return degminsec N-by-3 data.frame of coordinates with columns 'deg' for
  #' degrees, 'min' for minutes, and 'sec' for decimal seconds
  #' @export
  #' @examples
  #' # # single input coordinates
  #' decdeg = 30.48667
  #' degminsec = decdeg2degminsec(decdeg)
  #'
  #' # # multiple input coordinates
  #' decdeg = c(30.48667, -155.61496)
  #' degminsec = decdeg2degminsec(decdeg)
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
  degminsec = data.frame(deg = matrix(NA, nrow = nrow(decdeg)),
                         min = matrix(NA, nrow = nrow(decdeg)),
                         sec = matrix(NA, nrow = nrow(decdeg)))

  # loop through inputs and convert
  for (f in 1:nrow(decdeg)){
    deg = trunc(decdeg[f])
    dec = abs(decdeg[f] - deg)
    decmin = dec*60
    min = trunc(decmin)
    dec = abs(decmin - min)
    sec = dec*60

    degminsec$deg[f] = deg
    degminsec$min[f] = min
    degminsec$sec[f] = sec
  }

  return(degminsec)
}
