#' er_clickSummary
#'
#' @description Summarize clicks for a single AcousticStudy event, to be used
#' within the event_summary_report Rmd. Summarizes total number of clicks,
#' calculates SNR and defines 'good clicks' as those with SNR > 15 dB, and
#' calculates some summary statistics (peak and center frequency, 3 and 10 dB
#' bandwidths, and click duration) output into a nicely formatted table.
#'
#' last updated: 20 August 2026
#'
#' @param acStF AcousticStudy that has been filtered to remove noise
#' @param eventUID ID of a single acoustic event
#' @param snrThreshold threshold for "good" clicks with snr equal to or above
#' this threshold (in dB). Default is 15
#'
#' @return 'cl' list with 7 elements: total number of clicks, PAMpal click data
#' for all clicks, PAMpal spec data for all clicks (output from
#' PAMpal::calculateAverageSpectra()), the number of clicks above the specified
#' SNR threshold (default 15 dB if not specified), PAMpal click data for these
#' 'good' clicks only, and 'mt', a formatted table of median summary statistics
#'
#' @seealso
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

er_clickSummary <- function(acStF, eventUID, snrThreshold = 15){

  # set up output list
  cl <- list()

  # pull just click data for this event
  # allow for single event input vs acoustic study
  if(is.AcousticEvent(acStF)) {
    clicks <- getClickData(acStF)
    eventUID <- 1
  } else {
    clicks <- PAMpal::getClickData(acStF[[eventUID]])
  }
  if (!is.null(clicks)){
    # Order clicks by time
    clicks <- clicks[order(clicks$UTC),]
    # Remove duplicate clicks as in calculateAverageSpectra
    clicks <- clicks[!duplicated(clicks$UID),]
  }

  # add to output list
  cl$nClicks <- length(clicks$UTC)
  cl$clicks <- clicks

  # find just high SNR clicks
  spec <- NULL
  snr <- NULL
  goodClicks <- NULL

  if (!is.null(clicks)){
    # run specs for SNR calcs
    spec <- PAMpal::calculateAverageSpectra(acStF, evNum = eventUID, wl = 256,
                                            channel = 1, norm = TRUE,
                                            noise = TRUE, snr = snrThreshold,
                                            plot = FALSE)

    # # follows snr calc approach of calculateAvereageSpectra but done manually to
    # # save the output
    # if no clicks are above SNR this will be skipped
    if (!is.null(spec)){
    #   snr <- array(NA, length(spec$UID))
    #   for (iClick in 1:length(spec$UID)) {
    #
    #     wherePeak <- which.max(spec$allSpec[1:nrow(spec$allSpec), iClick])
    #     if (length(wherePeak) == 0) {
    #       snr[iClick] <- NA
    #       next
    #     }
    #     if (is.na(spec$allNoise[wherePeak, iClick])) {
    #       snr[iClick] <- Inf
    #       next
    #     }
    #     snr[iClick] <- spec$allSpec[wherePeak, iClick] -
    #       spec$allNoise[wherePeak, iClick]
    #   }
    #   # # remove an Infs or NAs
    #   # snr <- snr[is.finite(snr)]

      # pull out the high snr clicks
      # for summary stats - only use clicks with valid snr > 15 dB
      goodUIDs <- spec$UID[is.finite(spec$snrVals) & spec$snrVals >= snrThreshold]
      goodClicks <- clicks[clicks$UID %in% goodUIDs, , drop = FALSE]
    } else if (is.null(spec)){
      cat(paste0('No clicks above SNR threshold of ', snrThreshold, ' dB.\n'))
    }
  }

  # add to output
  cl$spec <- spec
  # cl$snr <- snr
  cl$nGoodClicks <- length(goodClicks$UID)
  cl$goodClicks <- goodClicks


  # if there are still valid clicks remaining...
  if (cl$nGoodClicks > 0) {
    # calculate a bunch of medians nicely formatted
    MedianPeak         <- formatC(median(goodClicks$peak, na.rm = TRUE),
                                  digits = 3, format = 'fg')
    MedianPeak_p25     <- formatC(quantile(goodClicks$peak, probs = .25,
                                        na.rm = TRUE), digits = 3, format = 'fg')
    MedianPeak_p75     <- formatC(quantile(goodClicks$peak, probs = .75,
                                        na.rm = TRUE), digits = 3, format = 'fg')
    MedianCenter3dB    <- formatC(median(goodClicks$centerkHz_3dB, na.rm = TRUE),
                                  digits = 3, format = 'fg')
    MedianCenter10dB   <- formatC(median(goodClicks$centerkHz_10dB, na.rm = TRUE),
                                  digits = 3, format = 'fg')
    MedianLower3dB     <- formatC(median(goodClicks$fmin_3dB, na.rm = TRUE),
                                  digits = 3, format = 'fg')
    MedianUpper3dB     <- formatC(median(goodClicks$fmax_3dB, na.rm = TRUE),
                                  digits = 3, format = 'fg')
    Median3dB_BW       <- formatC(median(goodClicks$BW_3dB, na.rm = TRUE),
                                  digits = 3, format = 'fg')
    MedianLower10dB    <- formatC(median(goodClicks$fmin_10dB, na.rm = TRUE),
                                  digits = 3, format = 'fg')
    MedianUpper10dB    <- formatC(median(goodClicks$fmax_10dB, na.rm = TRUE),
                                  digits = 3, format = 'fg')
    Median10dB_BW      <- formatC(median(goodClicks$BW_10dB, na.rm = TRUE),
                                  digits = 3, format = 'fg')
    MedianDuration     <- formatC(median(goodClicks$duration, na.rm = TRUE),
                                  digits = 2, format = 'fg')
    MedianDuration_p25 <- formatC(quantile(goodClicks$duration, probs = .25,
                                           na.rm = TRUE), digits = 0, format = 'fg')
    MedianDuration_p75 <- formatC(quantile(goodClicks$duration, probs = .75,
                                           na.rm = TRUE), digits = 0, format = 'fg')

    # compile into a summary table with nice formatting
    clnms <-c('parameter', 'value')
    mt <- setNames(data.frame(matrix(ncol = length(clnms), nrow = 6)), clnms)
    mt$parameter <- c('Median Peak Frequency [kHz] (25-75 percentile)',
                      'Median 3dB Center Frequency [kHz]',
                      'Median 10dB Center Frequency [kHz]',
                      'Median 3dB Bandwidth [kHz] (lower-upper)',
                      'Median 10dB Bandwidth [kHz] (lower-upper)',
                      'Median duration [\u03BCs] (25-75 percentile)')
    mt$value <- c(paste0(MedianPeak,  ' (',
                         MedianPeak_p25, ' - ',
                         MedianPeak_p75, ')'),
                  MedianCenter3dB,
                  MedianCenter10dB,
                  paste0(Median3dB_BW, " (",
                         MedianLower3dB, " - ",
                         MedianUpper3dB, ")"),
                  paste0(Median10dB_BW, " (",
                         MedianLower10dB, " - ",
                         MedianUpper10dB, ")"),
                  paste0(MedianDuration, ' (',
                         MedianDuration_p25, ' - ',
                         MedianDuration_p75, ')'))

    # attach to output
    cl$mt <- mt

  }

  return(cl)
}
