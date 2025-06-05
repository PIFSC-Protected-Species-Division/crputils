#' er_plotWhistleMedFreqHist
#'
#' @description Plot histogram median frequency of whistles
#'
#' @param wl list returned by whistleSummary() that has a elements wh
#'
#' @export
#' @importFrom graphics hist
#'
#' @examples
#' \dontrun{
#' er_plotWhistleMedFreqHist(wl)
#' }

er_plotWhistleMedFreqHist <- function(wl){

  hist(wl$wh$freqMedian/1000,
       breaks = seq(0, ceiling(max(wl$wh$freqMedian/1000)) + 1, 1),
       main = 'Histogram of whistle median frequency',
       xlab = 'Median frequency (kHz)')

}


