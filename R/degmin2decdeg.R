degmin2decdeg <- function(degmin){
  #' Degrees decimal minutes to decimal degrees
  #' 
  #' @description Utility to convert latitude and longitude coordinates from
  #' degrees minutes to decimal degrees
  #' 
  #' author: Selene Fregosi selene.fregosi at noaa.gov
  #' date: 28 July 2023
  #'
  #' @param degmin N-by-2 matrix of coordinates with each column representing
  #' degrees and minutes
  #' e.g., matrix(c(30, 29.2020), nrow = 1, ncol = 2)
  #' e.g., matrix(c(30, 29.2020,
  #'                -155, 36.8973), 
  #'              nrow = 2, ncol = 2, byrow = TRUE)
  #' @usage degmin2decdeg(degmin)
  #' @return decdeg  N-by-1 vector of coordinates in decimal degrees 
  #'
  #' @examples
  #' # # single input coordinates
  #' degmin = matrix(c(30, 29.2020), nrow = 1, ncol = 2)
  #' decdeg = degmin2decdeg(degmin)
  #' 
  #' # # multiple input coordinates
  #' degmin = matrix(c(30, 29.2020,        
  #'                      -155, 36.8973), 
  #'                    nrow = 2, ncol = 2, byrow = TRUE)
  #' decdeg = degmin2decdeg(degmin)
  #'
  #'#######################################################################
  
  
  # set up empty output 
  decdeg = data.frame(decdeg = matrix(nrow = nrow(degmin), ncol = 1))
  
  # loop through inputs and convert
  for (f in 1:nrow(degmin)){
    dec = degmin[f, 2]/60
    deg = degmin[f, 1]
    if (deg >= 0){
      decdeg[f,1] = deg + dec
    } else if (deg < 0){
      decdeg[f,1] = deg - dec
    }
  }
  
  return(decdeg)
}