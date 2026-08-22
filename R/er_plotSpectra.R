#' er_plotSpectra
#'
#' @description Plot spectra of all clicks - mean, median, noise, and any
#' specified reference spectra
#'
#' last updated: 22 August 2026
#'
#' @param spec output of [PAMpal::calculateAverageSpectra()] and includes
#' variables freq, avgSpec, avgNoise, allSpec, and snrVals
#' @param refSpecs optional argument with list of reference spectra to include
#' as overlays in the plot
#' @param flim optional argument for frequency limits. Default is full frequency
#' range of recordings. Define with format \code{c(0, 100)}, specify in kHz
#' @param snrThreshold optional argument for SNR threshold (in dB) for filtering
#' clicks used in calculation of median spectrum. Default is 15
#'
#' @seealso
#' [er_clickSummary()]
#' [er_plotClickDurationHist()]
#' [er_plotClickSNRHist()]
#' [er_plotConcatSpec()]
#'
#' @examples
#' \dontrun{
#' er_plotSpectra(spec, refSpecs = refSpecs, flim = c(0,100), snrThreshold = 15)
#' }
#'
#' @importFrom graphics lines abline legend
#' @importFrom stats median
#' @export


er_plotSpectra <- function(spec, refSpecs = NULL, flim = NULL, snrThreshold = 15){

  # define some stuff
  unitLab <- "Frequency (kHz)"
  unitFactor <- 1/1000
  plotTitle <- "Average Spectrum"

  # set frequency limits
  if (is.null(flim)) {
    plotFreq <- rep(TRUE, length(spec$freq))
  } else {
    plotFreq <- spec$freq >= flim[1]/unitFactor & spec$freq <= flim[2]/unitFactor
  }

  # Peak freq as calculated by calculateAvgerageSpectra - for subtitle
  peakFreq <- round(spec$freq[which.max(spec$avgSpec)]*unitFactor, 2)

  # set up plot
  plot(1, type = 'n', xlim = range(spec$freq[plotFreq])*unitFactor,
       ylim = c(min(spec$avgSpec), 0), xlab = unitLab,
       ylab = 'Normalized Magnitude (dB)', main = plotTitle,
       sub = paste0('Peak: ', peakFreq, 'kHz'))

  # add grid lines for frequency at 10 kHz intervals
  for (iline in ((0:15)*10)) {
    lines(c(iline, iline), c(min(spec$avgSpec) - 20, 10), col="gray")}

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
  lines(spec$freq*unitFactor, spec$avgNoise, lty = 3, lwd = 2)
  # plot avg spectrum
  lines(spec$freq*unitFactor, spec$avgSpec, lty = 2, lwd = 3)
  # also plot median spectrum
  # have to manually trim to just high SNR
  loudSpec <- spec$allSpec[, is.finite(spec$snrVals) &
                             spec$snrVals >= snrThreshold, drop = FALSE]
  medSpec <- 20*log10(apply(loudSpec, 1, function(y) {
    median(10^(y/20), na.rm = TRUE)}))
  medSpecNorm <- medSpec - max(medSpec, na.rm = TRUE)
  lines(spec$freq*unitFactor, medSpecNorm, lty = 1, lwd = 3)

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


