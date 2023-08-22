degminsec2decdeg <- function(degminsec){
  #' degminsec2decdeg
  #'
  #' @description Utility to convert latitude and longitude coordinates from
  #' degrees minutes seconds to decimal degrees
  #'
  #' @param degminsec N-by-3 matrix of coordinates with each column representing
  #' degrees, minutes, and seconds
  #' e.g., matrix(c(30, 29, 12), nrow = 1, ncol = 3)
  #' e.g., matrix(c(30, 29, 12,
  #'                -155, 36, 53.838),
  #'              nrow = 2, ncol = 3, byrow = TRUE)
  #' @usage degminsec2decdeg(degminsec)
  #' @return N-by-1 vector of coordinates in decimal degrees
  #' @export
  #' @examples
  #' # single input coordinates
  #' degminsec = matrix(c(30, 29, 12), nrow = 1, ncol = 3)
  #' decdeg = degminsec2decdeg(degminsec)
  #'
  #' # multiple input coordinates
  #' degminsec = matrix(c(30, 29, 12,
  #'                      -155, 36, 53.838),
  #'                    nrow = 2, ncol = 3, byrow = TRUE)
  #' decdeg = degminsec2decdeg(degminsec)
  #'


  # set up empty output
  decdeg = matrix(nrow = nrow(degminsec), ncol = 1)

  # loop through inputs and convert
  for (f in 1:nrow(degminsec)){
    secDec = degminsec[f,3]/60
    dec = (degminsec[f,2] + secDec)/60
    deg = degminsec[f, 1]
    if (deg >= 0){
      decdeg[f,1] = deg + dec
    } else if (deg < 0){
      decdeg[f,1] = deg - dec
    }
  }

  return(decdeg)
}
