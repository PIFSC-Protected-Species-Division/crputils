#' er_plotConcatSpec
#'
#' @description Plot a concatenated spectrogram of all clicks with snr above
#' snrThreshold, sorted by peak frequency. This plotting code has been extracted
#' from [PAMpal::calculateAverageSpectra] to be run on outputs of that same
#' function.
#'
#' last updated: 21 August 2026
#'
#' @param spec output of [PAMpal::calculateAverageSpectra()] and includes
#' variables freq, avgSpec, avgNoise, allSpec, and snrVals
#' @param flim optional argument for frequency limits. Default is 0 to 100.
#' Define with format \code{c(0, 100)}
#' @param snrThreshold optional argument for SNR threshold (in dB) for filtering
#' which clicks to include in plot. Default is 15
#' @param sort logical flag to sort concatenated spectrogram by peak frequency.
#' Default is \code{TRUE} (this is different than
#' [PAMpal::calculateAverageSpectra()]'s default.)
#' @param filterfrom_khz frequency in kHz of highpass filter to apply, or the
#' lower bound of a bandpass filter if \code{filterto_khz} is not NULL. Default is 0.
#' @param filterto_khz if a bandpass filter is desired, set this as the upper
#' bound. If only a highpass filter is desired, leave as the default NULL value.
#' Currently only highpass and bandpass filters are supported, so if
#' \code{filterfrom_khz} is left as zero then this parameter will have no effect.
#' @param q lower and upper quantiles to remove for scaling concatenated
#' spectrogram. Or if a single value, then quantiles \code{q} and \code{q} will
#' be used. Ex. if \code{q=0.01} (default), then the bottom 1% and top 1% of
#' values are truncated before plotting the image. This is done purely for
#' cosmetic reasons, no output data is affected
#' @param brightness value from -255 to 255, positive values increase
#' brightness, negative values decrease brightness of concatenated spectrogram
#' image
#' @param contrast value from -255 to 255, positive values increase contrast,
#' negative values decrease contrast of concatenated spectrogram image
#' @param cmap colors to use for concatenated click spectrogram, either a
#' palette function or vector of colors
#' @param showBreaks logical flag to show lines separating events when plotting
#' multiple events. This DOES not apply for event summary reports (as they plot)
#' event-by-event but it is included here just in case. Default is \code{FALSE}
#'
#' @seealso
#' [er_clickSummary()]
#' [er_plotClickDurationHist()]
#' [er_plotClickSNRHist()]
#' [er_plotSpectra()]
#'
#' @examples
#' \dontrun{
#' cl <- er_clickSummary(detsFilt, eventUID)
#' }
#'
#' @importFrom PAMpal getClickData calculateAverageSpectra is.AcousticEvent
#' @importFrom stats median quantile setNames
#' @export

er_plotConcatSpec <- function(spec, flim = c(0, 100), snrThreshold = 15,
                              sort = TRUE, filterfrom_khz = 0, filterto_khz = NULL,
                              q = 0.01, brightness = 0, contrast = 0,
                              cmap = hcl.colors("YlOrRd", rev = TRUE),
                              showBreaks = FALSE){


  function (x, evNum = 1, calibration = NULL, wl = 512, channel = 1:2,
            filterfrom_khz = 0, filterto_khz = NULL, sr = NULL, snr = 0,
            norm = TRUE, plot = TRUE, noise = FALSE, decimate = 1, sort = FALSE,
            mode = "spec", title = NULL, ylim = NULL, flim = NULL, cmap = hcl.colors(30,
                                                                                     "YlOrRd", rev = TRUE),
            brightness = 0, contrast = 0,
            q = 0.01, showBreaks = TRUE, ...)


  # define some stuff
  unitLab <- "Frequency (kHz)"
  unitFactor <- 1/1000
  plotTitle <- "Concatenated Click Spectrogram"
  maxZ <- 3

  # set frequency limits
  if (is.null(flim)) {
    plotFreq <- rep(TRUE, length(spec$freq))
  } else {
    plotFreq <- spec$freq >= flim[1] & spec$freq <= flim[2]
  }
  freqPretty <- pretty(seq(from = min(spec$freq[plotFreq]) *
                             unitFactor, to = max(spec$freq[plotFreq]) * unitFactor,
                           length.out = 5), n = 5)

  # get good clicks above snrThreshold
  snrKeep <- snrVals >= snrThreshold

  # set up plot
  plotMat <- spec$allSpec[plotFreq, snrKeep, drop = FALSE]

  # clean up any filtered frequencies
  goodFreq <- spec$freq[plotFreq] >= filterfrom_khz * 1000
  if (!is.null(filterto_khz)) {
    goodFreq <- (spec$freq[plotFreq] <= filterto_khz *
                   1000) & goodFreq
  }
  plotMat[plotMat < min(plotMat[goodFreq, ])] <- min(plotMat[goodFreq,
  ])

  # cosmetic clean up - get ride of loud/quiet clicks, set brightness, contrast
  if (length(q) == 1) {
    q <- c(q, 1 - q)
  }
  lim <- quantile(plotMat, q)
  plotMat[plotMat < lim[1]] <- lim[1]
  plotMat[plotMat > lim[2]] <- lim[2]

  plotMat <- plotMat/diff(range(plotMat, na.rm = TRUE)) *
    255
  plotMat <- plotMat - min(plotMat, na.rm = TRUE)
  plotMat[plotMat > 255] <- 255
  plotMat[plotMat < 0] <- 0

  # sort by peak frequency if TRUE
  if (sort) {
    sortIx <- sort(apply(plotMat, 2, which.max),
                   index.return = TRUE)$ix
    plotMat <- plotMat[, sortIx, drop = FALSE]
  }

  # set brightness/contrast if specified
  plotMat <- doContrast(doBrightness(plotMat, brightness),
                        contrast)

  # set colormap if specified
  if (is.function(cmap)) {
    cmap <- cmap(30)
  }

  # actually plot it!
  image(x = 1:ncol(plotMat), y = seq(from = min(freq[plotFreq] *
                                                  unitFactor), to = max(freq[plotFreq] * unitFactor),
                                     length.out = nrow(plotMat)), z = t(plotMat),
        xaxt = "n", yaxt = "n", ylab = unitLab, xlab = "Click Number",
        useRaster = TRUE, col = cmap, breaks = seq(from = 0,
                                                   to = 255, length.out = 31))

  # add labels
  title(plotTitle)
  xPretty <- pretty(1:ncol(plotMat), n = 5)
  axis(1, at = xPretty, labels = xPretty)
  axis(2, at = freqPretty, labels = freqPretty)
  if (isTRUE(showBreaks) & isFALSE(sort) & length(evNum) >
      1) {
    evBreaks <- table(clickData$eventId)[sapply(events(ev),
                                                id)]
    evStarts <- cumsum(evBreaks)
    for (b in 1:(length(evStarts) - 1)) {
      lines(x = rep(evStarts[b] + 0.5, 2), y = c(0,
                                                 max(freq[plotFreq] * unitFactor)), lwd = 2)
    }
}


}
