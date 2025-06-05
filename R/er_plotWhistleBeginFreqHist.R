#' er_plotWhistleBeginFreqHist
#'
#' @description Plot histogram begin frequency of whistles
#'
#' @param wl list returned by whistleSummary() that has a elements wh
#'
#' @export
#' @importFrom graphics hist
#'
#' @examples
#' \dontrun{
#' er_plotWhistleBeginFreqHist(wl)
#' }

er_plotWhistleBeginFreqHist <- function(wl){

  hist(wl$wh$freqBeg/1000,
       breaks = seq(0, ceiling(max(wl$wh$freqBeg/1000)) + 1, 1),
       main = 'Histogram of whistle begin frequency',
       xlab = 'Begin frequency (kHz)')
}


