#' er_plotClickSNRHist
#'
#' @description Plot histogram of click SNR for all clicks with a vertical
#' dashed line at the specified cutoff
#'
#' last updated: 21 August 2026
#'
#' @param cl list returned by clickSummary() that has element spec that has
#' element snrVals
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
#' er_plotClickSNRHist(cl, 15)
#' }
#'
#' @importFrom graphics hist abline
#' @export

er_plotClickSNRHist <- function(cl, snrThreshold = 15){

  xMax <- max(c(snrThreshold,
                ceiling(max(cl$spec$snrVals[is.finite(cl$spec$snrVals)])) + 2)) # whichever is bigger
  hist(cl$spec$snrVals, breaks = seq(from = floor(min(cl$spec$snrVals)),
                            to = xMax, by = 2),
       main = 'Click SNR', sub = '(all filtered clicks)', xlab = 'SNR (dB)')
  abline(v = snrThreshold, lty = 2, lwd = 2, col = 'red4')

}


