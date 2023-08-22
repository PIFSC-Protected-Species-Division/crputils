degminsec2degmin <- function(degminsec){
  #' degminsec2degmin
  #'
  #' @description Utility to convert latitude and longitude coordinates from
  #' degrees minutes seconds to degrees decimal minutes
  #'
  #' @param degminsec N-by-3 matrix of coordinates with each column representing
  #' degrees, minutes, and seconds
  #' e.g., matrix(c(30, 29, 12), nrow = 1, ncol = 3)
  #' e.g., matrix(c(30, 29, 12,
  #'                -155, 36, 53.838),
  #'              nrow = 2, ncol = 3, byrow = TRUE)
  #' @usage degminsec2degmin(degminsec)
  #' @return N-by-2 data.frame of coordinates with columns 'deg' for degrees and
  #' 'min for minutes
  #' @export
  #' @examples
  #' # single input coordinates
  #' degminsec = matrix(c(30, 29, 12), nrow = 1, ncol = 3)
  #' degmin = degminsec2degmin(degminsec)
  #'
  #' # multiple input coordinates
  #' degminsec = matrix(c(30, 29, 12,
  #'                      -155, 36, 53.838),
  #'                    nrow = 2, ncol = 3, byrow = TRUE)
  #' degmin = degminsec2degmin(degminsec)
  #'
  #' ######################################################################

  # set up empty output
  degmin = matrix(nrow = nrow(degminsec), ncol = 2)

  # loop through inputs and convert
  for (f in 1:nrow(degminsec)){
    secDec = degminsec[f,3]/60
    min = degminsec[f,2] + secDec
    deg = degminsec[f, 1]

    degmin$deg[f] = deg
    degmin$min[f] = min
  }

  return(degmin)
}
