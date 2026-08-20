#' er_plotSpectra
#'
#' @description Plot spectra of all clicks - mean, median, noise, and any
#' specified reference spectra
#'
#' last updated: 20 August 2026
#'
#' @param snrThreshold SNR in dB to plot as the threshold for 'good' clicks
#' @param cl list returned by clickSummary() that has element spec which is the
#' output of PAMpal::calculateAverageSpectra and includes varaibles freq,
#' avgSpec, avgNoise, allSpec, and snrVals
#' @param refSpecs optional argument with list of reference spectra to include
#' as overlays in the plot
#' @param xLims optional argument for frequency limits. Default is 0 to 100.
#' Define with format c(0, 100)
#' @param snrThreshold optional argument for SNR threshold (in dB) for filtering
#' clicks used in calculation of median spectrum. Default is 15
#'
#' @seealso
#' [er_clickSummary()]
#' [er_plotClickDurationHist()]
#' [er_plotClickSNRHist()]
#'
#' @examples
#' \dontrun{
#' er_plotSpectra(cl, refSpecs = refSpecs, xLims = c(0,100), snrThreshold = 15)
#' }
#'
#' @importFrom graphics lines abline legend
#' @importFrom stats median
#' @export


er_plotSpectra <- function(cl, refSpecs = NULL, xLims = c(0, 100), snrThreshold = 15){

  # Peak freq as calculated by calculateAvgerageSpectra -for subtitle
  peakFreq <- round(cl$spec$freq[which.max(cl$spec$avgSpec)]/1000, 2)

  plot(1, type = 'n', xlim = xLims, ylim = c(min(cl$spec$avgSpec), 0),
       xlab = 'Frequency (kHz)', ylab = 'Normalized Magnitude (dB)',
       main = 'Average Spectrum', sub = paste0('Peak: ', peakFreq, 'kHz'))

  # add grid lines for frequency at 10 kHz intervals
  for (iline in ((0:15)*10)) {
    lines(c(iline, iline), c(min(cl$spec$avgSpec) - 20, 10), col="gray")}

  # add template spectra
  if (!is.null(refSpecs)){
    rsCols <- refSpecs$rsPalette[1:length(refSpecs)]
    for (rs in 1:length(refSpecs$refSpecsList)){
      rsTmp <- refSpecs$refSpecList[rs]
      rsNorm <- refSpecs[[rsTmp]]
      rsMax <- max(rsNorm$dB)
      rsNorm$dBNorm <- rsNorm$dB - rsMax
      lines(rsNorm$frq, rsNorm$dBNorm, col = rsCols[rs], lwd = 2)
    }
  }

  # plot noise
  lines(cl$spec$freq/1000, cl$spec$avgNoise, lty = 3, lwd = 2)
  # plot avg spectrum
  lines(cl$spec$freq/1000, cl$spec$avgSpec, lty = 2, lwd = 3)
  # also plot median spectrum
  # have to manually trim to just high SNR
  loudSpec <- cl$spec$allSpec[, is.finite(cl$spec$snrVals) &
                                cl$spec$snrVals >= snrThreshold, drop = FALSE]
  medSpec <- 20*log10(apply(loudSpec, 1, function(y) {
    median(10^(y/20), na.rm = TRUE)}))
  medSpecNorm <- medSpec - max(medSpec, na.rm = TRUE)
  lines(cl$spec$freq/1000, medSpecNorm, lty = 1, lwd = 3)

  # add legend
  if (!is.null(refSpecs)){
    legend(x = 'topright', c(refSpecs$refSpecSp, 'Avg.', 'Med.', 'Noise'),
           lty = c(rep(1, length(refSpecs$refSpecSp)), 2, 1, 3),
           lwd = c(rep(1, length(refSpecs$refSpecSp)), 2, 3, 2),
           col = c(rsCols, 'black', 'black', 'black'), cex = 0.8)
  } else if (is.null(refSpecs)){
    legend(x = 'topright', c('Avg.', 'Med.', 'Noise'),
           lty = c(2, 1, 3), lwd = c(2, 3, 2),
           col = c('black', 'black', 'black'), cex = 0.8)
  }

}


