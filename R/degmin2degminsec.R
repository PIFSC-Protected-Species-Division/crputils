degmin2degminsec <- function(degmin){
  #' degmin2degminsec
  #' 
  #' @description Utility to convert latitude and longitude coordinates from
  #' degrees minutes to degrees minutes seconds
  #' 
  #' author: Selene Fregosi selene.fregosi [at] noaa.gov
  #' date: 28 July 2023
  #'
  #' @param degmin N-by-2 matrix of coordinates with each column representing
  #' degrees and minutes
  #' e.g., matrix(c(30, 29.2020), nrow = 1, ncol = 2)
  #' e.g., matrix(c(30, 29.2020,
  #'                -155, 36.8973), 
  #'              nrow = 2, ncol = 2, byrow = TRUE)
  #' @usage degmin2degminsec(degmin)
  #' @return degminsec N-by-3 matrix of coordinates with each column 
  #' representing degrees, minutes, and seconds
  #'
  #' @examples
  #' # single input coordinates
  #' degmin = matrix(c(30, 29.2020), nrow = 1, ncol = 2)
  #' degminsec = degmin2degminsec(degmin)
  #' 
  #' # multiple input coordinates
  #' degmin = matrix(c(30, 29.2020,        
  #'                      -155, 36.8973), 
  #'                    nrow = 2, ncol = 2, byrow = TRUE)
  #' degminsec = degmin2degminsec(degmin)
  #'
  #' ######################################################################
  
  # set up empty output 
  degminsec = matrix(nrow = nrow(degmin), ncol = 3)
  
  # loop through inputs and convert
  for (f in 1:nrow(degmin)){
    deg = degmin[f,1]
    min = trunc(degmin[f,2])
    dec = abs(degmin[f,2] - min)
    sec = dec*60
    degminsec[f,1:3] = c(deg, min, sec)
  }

  return(degminsec)
}