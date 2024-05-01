#' degmin2degminsec
#'
#' @description Utility to convert latitude and longitude coordinates from
#' degrees minutes to degrees minutes seconds
#'
#' @author Selene Fregosi
#'
#' @param degmin N-by-2 matrix of coordinates with each column representing
#' degrees and minutes
#' e.g., \code{matrix(c(30, 29.2020), nrow = 1, ncol = 2)}\cr
#' e.g., \code{matrix(c(30, 29.2020,
#'              -155, 36.8973),
#'              nrow = 2, ncol = 2, byrow = TRUE)}
#'
#' @returns N-by-3 data.frame of coordinates with columns 'deg for degrees and
#' 'min' for minutes and 'sec' for decimal seconds
#'
#' @export
#'
#' @examples
#' # single input coordinates
#' degmin <- matrix(c(30, 29.2020), nrow = 1, ncol = 2)
#' degminsec <- degmin2degminsec(degmin)
#'
#' # multiple input coordinates
#' degmin <- matrix(c(30, 29.2020,
#'                    -155, 36.8973),
#'                    nrow = 2, ncol = 2, byrow = TRUE)
#' degminsec <- degmin2degminsec(degmin)
#'

degmin2degminsec <- function(degmin){

  # set up empty output
  degminsec <- data.frame(deg = matrix(NA, nrow = nrow(degmin)),
                          min = matrix(NA, nrow = nrow(degmin)),
                          sec = matrix(NA, nrow = nrow(degmin)))

  # loop through inputs and convert
  for (f in 1:nrow(degmin)){
    deg <- degmin[f,1]
    min <- trunc(degmin[f,2])
    dec <- abs(degmin[f,2] - min)
    sec <- dec*60

    degminsec$deg[f] <- deg
    degminsec$min[f] <- min
    degminsec$sec[f] <- sec
  }

  return(degminsec)
}
