#' er_plotSpectra
#'
#' @description Plot spectra of all clicks - mean, median, noise, and any
#' specified reference spectra
#'

#' @param snrThreshold SNR in dB to plot as the threshold for 'good' clicks
#' @param avgSpec list output of PAMpal::calculateAvereageSpectra that includes
#' variables freq, avgSpec, avgNoise
#' @param cl list returned by clickSummary() that has a elements spec and snr
#' (if SNR is added as an output to calculateAverageSpectra this can be removed)
#' @param refSpecs optional argument with list of reference spectra to include
#' as overlays in the plot
#' @param xLims optional argument for frequency limits. Default is 0 to 100.
#' Define with format c(0, 100)
#' @param snrThreshold optional argument for SNR threshold (in dB) for filtering
#' clicks used in calculation of median spectrum. Default is 15
#'
#' @export
#' @importFrom graphics lines abline legend
#' @importFrom stats median
#'
#' @examples
#' \dontrun{
#' er_plotSpectra(avgSpec, cl, refSpecs = refSpecs, xLims = c(0,100), snrThreshold = 15)
#' }
#'

er_plotSpectra <- function(avgSpec, cl, refSpecs = NULL, xLims = c(0, 100),
                           snrThreshold = 15){

  # Peak freq as calculated by calculateAvgerageSpectra -for subtitle
  peakFreq <- round(avgSpec$freq[which.max(avgSpec$avgSpec)]/1000, 2)

  plot(1, type = 'n', xlim = xLims, ylim = c(min(avgSpec$avgSpec), 0),
       xlab = 'Frequency (kHz)', ylab = 'Normalized Magnitude (dB)',
       main = 'Average Spectrum', sub = paste0('Peak: ', peakFreq, 'kHz'))

  # add grid lines for frequency at 10 kHz intervals
  for (iline in ((0:15)*10)) {
    lines(c(iline, iline), c(min(avgSpec$avgSpec) - 20, 10), col="gray")}

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
  lines(avgSpec$freq/1000, avgSpec$avgNoise, lty = 3, lwd = 2)
  # plot avg spectrum
  lines(avgSpec$freq/1000, avgSpec$avgSpec, lty = 2, lwd = 3)
  # also plot median spectrum
  # have to manually trim to just high SNR
  loudSpec <- avgSpec$allSpec[, is.finite(cl$snr) &
                                cl$snr >= snrThreshold, drop = FALSE]
  medSpec <- 20*log10(apply(loudSpec, 1, function(y) {
    median(10^(y/20), na.rm = TRUE)}))
  medSpecNorm <- medSpec - max(medSpec, na.rm = TRUE)
  lines(avgSpec$freq/1000, medSpecNorm, lty = 1, lwd = 3)

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


