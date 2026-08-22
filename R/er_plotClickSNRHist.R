#' er_plotClickSNRHist
#'
#' @description Plot histogram of click SNR for all clicks with a vertical
#' dashed line at the specified cutoff
#'
#' last updated: 22 August 2026
#'
#' @param spec output of [PAMpal::calculateAverageSpectra()] that includes
#' variable snrVals
#' @param snrThreshold SNR in dB to plot as the threshold for 'good' clicks.
#' Default is 15 dB
#'
#'#' @seealso
#' [er_clickSummary()]
#' [er_plotClickDurationHist()]
#' [er_plotSpectra()]
#' [er_plotConcatSpec()]
#'
#' @examples
#' \dontrun{
#' er_plotClickSNRHist(spec, snrThreshold = 15)
#' }
#'
#' @importFrom graphics hist abline
#' @export

er_plotClickSNRHist <- function(spec, snrThreshold = 15){

  xMax <- max(c(snrThreshold,
                ceiling(max(spec$snrVals[is.finite(spec$snrVals)])) + 2)) # whichever is bigger
  hist(spec$snrVals, breaks = seq(from = floor(min(spec$snrVals)),
                            to = xMax, by = 2),
       main = 'Click SNR', sub = '(all filtered clicks)', xlab = 'SNR (dB)')
  abline(v = snrThreshold, lty = 2, lwd = 2, col = 'red4')

}


