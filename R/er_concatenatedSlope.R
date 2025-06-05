#' er_concatenatedSlope
#'
#' @description Model the slope of peak frequencies of clicks sorted by peak
#' peak frequency as displayed in a concatenated spectrogram using linear
#' regression. This function models the slope in two ways (1) for all clicks and
#' (2) for just clicks within the 25-75th percentiles of the mean.
#'
#' @param cl list returned by clickSummary() that has a elements spec and snr
#' @param quantLimit optional argument to limit the clicks to a particular
#' quantile range e.g., only clicks in the 25-75th quantiles, 5-95th, etc.
#' Default is NULL (all clicks), input in format c(.25, .75)
#'
#' @return 'cs' the linear regression model output
#'
#' @export
#' @importFrom stats quantile lm coef rstandard
#' @importFrom graphics points text abline legend
#'
#' @examples
#' # all clicks
#' \dontrun{cs <- concatenatedSlope(cl)}
#' # 25-75th quantile only
#' \dontrun{cs <- concatenatedSlope(cl, c(0.25, 0.75))}

er_concatenatedSlope <- function(cl, quantLimit = NULL){

  # extract spec data for just good SNR clicks
  goodSpec <- cl$spec$allSpec[, cl$spec$UID %in% cl$goodClicks$UID]

  # need at least 2 clicks to run the linear regression (although results will
  # be pretty meaningless with only 2 clicks...)
  if (cl$nGoodClicks <= 1){
    cat('Insufficient clicks to calculate slope\n')
    return(NULL)
  }

  # find and sort peaks
  peaksIdx <- apply(goodSpec, 2, which.max)
  peaks <- cl$spec$freq[peaksIdx]/1000
  sorted <- sort(peaks)
  clickNum <- seq_along(sorted)

  # if limiting by quantile, find limits
  if (!is.null(quantLimit)){
    lwrQuant <- quantile(sorted, probs = quantLimit[1])
    uprQuant <- quantile(sorted, probs = quantLimit[2])
    peakFreq <- sorted[which(sorted >= lwrQuant & sorted <= uprQuant)]
    index <- clickNum[which(sorted >= lwrQuant & sorted <= uprQuant)]
    plotTitle <- paste0('Slope of peak frequencies\n', quantLimit[1], ' to ',
                        quantLimit[2], ' quantile only')
  } else {
    plotTitle <- paste0('Slope of peak frequencies\nAll clicks')
    peakFreq <- sorted
    index <- clickNum
  }


  # df <- data.frame(index = index, sorted = sorted)

  # check that enough clicks remain within quantile limits if used
  if (length(peakFreq) <= 1 | length(index) <= 1){
    cat('Insufficient clicks to calculate slope\n')
    return(NULL)
  }

  # run linear regression
  cs <- lm(peakFreq ~ index)
  summary(cs)

  # if running on all clicks
  # if (is.null(quantLimit)){
  # remove outliers - standardized residuals > 2 or < -2
  stdResid <- rstandard(cs)
  outlierIdx <- which(abs(stdResid) > 2)
  if (length(outlierIdx) > 0){
    peakFreqClean <- peakFreq[-outlierIdx]
    indexClean <- index[-outlierIdx]
    # save the old model (only for testing?)
    csOg <- cs
    # fit new model without outliers
    cs <- lm(peakFreqClean ~ indexClean)
    summary(cs)
  }

  # extract intercept and slope (in Hz)
  # cs$intercept <- coef(m)[1]
  # cs$slope <- coef(m)[2]
  # # and some output of fit
  # cs$rsqd <- summary(m)$r.squared
  # # add full model info
  # cs$m <- m

  # plots for testing
  # plot residuals
  # plot(residuals(cs), type = "h", col = "darkgray",
  #      xlab = "Spectrogram Slice Index", ylab = "Residual",
  #      main = "Residuals of Linear Model")
  # abline(h = 0, col = "red", lty = 2)

  # plot trend line
  plot(sorted, type = "p", col = "grey50", pch = 16,
       xlab = "Click (sorted by peak frequency)", ylab = "Peak frequency (kHz)",
       main = plotTitle)
  if (!is.null(quantLimit)){
    points(index, peakFreq, type = "p", col = "grey80", pch = 16, cex = 0.5)
  }
  # if (is.null(quantLimit)){
  points(index[outlierIdx], peakFreq[outlierIdx], col = "grey20", pch = 4, cex = 0.5)
  # }
  abline(cs, col = "coral2", lwd = 2)
  text(x = length(sorted)*.8, y = min(sorted)*1.05,
       labels = paste("slope = ", round(coef(cs)[2]*1000, 2), "Hz"),
       col = "coral2", cex = 1)
  if (!is.null(quantLimit)){
    legend("topleft", legend = c("Raw data", "Within quantile", "Outliers", "Linear fit"),
           col = c("grey50", "grey80", "grey20", "coral2"), pch = c(16, 16, 4, NA),
           lty = c(NA, NA, NA, 1), lwd = c(NA, NA, NA, 2))
  } else {
    legend("topleft", legend = c("Raw data", "Outliers", "Linear fit"),
           col = c("grey50", "grey20", "coral2"), pch = c(16, 4, NA),
           lty = c(NA, NA, 1), lwd = c(NA, NA, 2))
  }

  # # with confidence interval
  # pred <- predict(cs, interval = "confidence")
  # plot(sorted, pch = 16, col = "darkgrey",
  #      xlab = "Spectrogram Slice Index", ylab = "Peak Frequency",
  #      main = "Linear Fit with 95% Confidence Interval")
  # lines(pred[, "fit"], col = "coral", lwd = 2)
  # lines(pred[, "lwr"], col = "lightcoral", lty = 2)
  # lines(pred[, "upr"], col = "lightcoral", lty = 2)
  # legend("topright", legend = c("Fit", "95% CI"),
  #        col = c("coral", "lightcoral"), lty = c(1, 2), lwd = c(2, 1))

  return(cs)
}
