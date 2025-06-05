#' er_plotClickDurationHist
#'
#' @description Plot histogram of click durations of high SNR clicks only
#'
#' @param cl list returned by clickSummary() that has a elements spec and snr
#'
#' @export
#' @importFrom graphics hist abline legend
#' @importFrom stats median
#'
#' @examples
#' \dontrun{
#' er_plotClickDurationHist
#' }

er_plotClickDurationHist <- function(cl){
  subStr <- paste0('(high SNR clicks, n=', cl$nGoodClicks, ')')
  hist(cl$goodClicks$duration,
       breaks = seq(from = 0, to = max(cl$goodClicks$duration) + 100,
                    by = 100), main = 'Click duration', sub = subStr,
       xlab = expression(paste('duration (', mu, 's)')))
  abline(v = median(cl$goodClicks$duration), lty = 2, lwd = 2, col = 'black')
  legend('topright', legend = 'median', lty = 2, lwd = 2, col = 'black')
}
