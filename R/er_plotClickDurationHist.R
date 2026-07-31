#' er_plotClickDurationHist
#'
#' @description Plot histogram of click durations of high SNR clicks only
#'
#' last updated: 31 July 2026
#'
#' @param cl list returned by er_clickSummary() containing elements
#'   goodClicks (data frame with column duration) and nGoodClicks (count)
#'
#' @return creates a plot
#'
#' @author Selene Fregosi \email{selene.fregosi@noaa.gov}
#'
#' @seealso [er_clickSummary()]
#'
#' @examples
#' \dontrun{
#' cl <- er_clickSummary(acSt)
#' er_plotClickDurationHist(cl)
#' }
#'
#' @importFrom graphics hist abline legend
#' @importFrom stats median
#' @export

er_plotClickDurationHist <- function(cl){
  subStr <- paste0('(high SNR clicks, n=', cl$nGoodClicks, ')')
  hist(cl$goodClicks$duration,
       breaks = seq(from = 0, to = max(cl$goodClicks$duration) + 100,
                    by = 100), main = 'Click duration', sub = subStr,
       xlab = expression(paste('duration (', mu, 's)')))
  abline(v = median(cl$goodClicks$duration), lty = 2, lwd = 2, col = 'black')
  legend('topright', legend = 'median', lty = 2, lwd = 2, col = 'black')
}
