decdeg2degminsec <- function(decdeg){
  #' decdeg2degminsec
  #' 
  #' @description Utility to convert latitude and longitude coordinates from
  #' degrees minutes to degrees minutes second
  #' 
  #' author: Selene Fregosi selene.fregosi [at] noaa.gov
  #' date: 28 July 2023
  #'
  #' @param decdeg N-by-1 vector of coordinates in decimal degrees 
  #' e.g., 30.48667
  #' e.g., matrix(c(30.48667, -155.61496),
  #'              nrow = 2, ncol = 1, byrow = TRUE)
  #' @usage decdeg2degminsec(decdeg)
  #' @return degminsec N-by-3 matrix of coordinates with each column 
  #' representing degrees, minutes, and seconds
  #'
  #' @examples
  #' # # single input coordinates
  #' decdeg = 30.48667
  #' degminsec = decdeg2degminsec(decdeg)
  #' 
  #' # # multiple input coordinates
  #' decdeg = matrix(c(30.48667, -155.61496),
  #'                 nrow = 2, ncol = 1, byrow = TRUE)
  #' degminsec = decdeg2degminsec(decdeg)
  #' 
  #' ######################################################################
  
  # set up empty output 
  degminsec = matrix(nrow = nrow(decdeg), ncol = 3)
  
  # loop through inputs and convert
  for (f in 1:nrow(decdeg)){
    deg = trunc(decdeg[f])
    dec = abs(decdeg[f] - deg)
    decmin = dec*60
    min = trunc(decmin)
    dec = abs(decmin - min)
    sec = dec*60
    
    degminsec[f,1:3] = c(deg, min, sec)
  }
  
  return(degminsec)
}