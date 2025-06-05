#' er_plotClickSNRHist
#'
#' @description Plot histogram of click SNR for all clicks with a vertical
#' dashed line at the specified cutoff
#'
#' @param cl list returned by clickSummary() that has a elements spec and snr
#' @param snrThreshold SNR in dB to plot as the threshold for 'good' clicks
#'
#' @export
#' @importFrom graphics hist abline
#'
#' @examples
#' \dontrun{
#' er_plotClickSNRHist(cl, 15)
#' }

er_plotClickSNRHist <- function(cl, snrThreshold){

  xMax <- max(c(snrThreshold, ceiling(max(cl$snr[is.finite(cl$snr)])) + 2)) # whichever is bigger
  hist(cl$snr, breaks = seq(from = floor(min(cl$snr)),
                            to = xMax, by = 2),
       main = 'Click SNR', sub = '(all filtered clicks)', xlab = 'SNR (dB)')
  abline(v = snrThreshold, lty = 2, lwd = 2, col = 'red4')

}


