#' er_filterClicks
#'
#' @description  filter all click detections to remove clicks that are
#' unlikely to be odontocetes and are likely noise. Filter settings were tuned
#' by Jennifer McCullough, see McCullough et al. 2021
#'
#' Filter settings:
#'    Remove clicks with 10dB bandwidth < 5 kHz
#'    Remove clicks with peak frequency below 5 kHz and above 80 kHz
#'    Remove clicks with durations of less than 2 ms or greater than 1000 ms
#'    For each individual click detector (classifier), remove clicks with peak
#'       frequencies outside the specified limits (remove erroneous
#'       miss-classifications)
#'
#' last updated: 31 July 2026
#'
#' @param acSt AcousticStudy object generated with processPgDetections()
#'
#' @return an AcousticStudy object containing only filtered detections
#'
#' @author Selene Fregosi \email{selene.fregosi@noaa.gov}
#'
#' @examples
#' \dontrun{
#' detsFiltered <- er_filterClicks(dets)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom PAMpal getClickData
#' @export

er_filterClicks <- function(acSt){


    # 'acSt' is an AcousticStudy object
    # acSt <- dets # for testing - AcousticStudy

    asF <- acSt # rename output so will be iterative but preserve original just in case

    # filters applied to ALL click detectors
    detectorList <- unique(getClickData(asF)$detectorName)
    # must loop through each click detector bc otherwise duration filter would
    # be applied to whistle and cepstrum detections (BW/peak wouldn't matter)
    for (cd in 1:length(detectorList)){
      # set detector to filter
      dtf <- detectorList[cd]
      # remove clicks with bandwidth at 10dB < 5 kHz (keep BW_10dB > 5)
      asF <- filter(asF, (detectorName != dtf) | (BW_10dB > 5))
      # remove clicks with peak below 5 and above 80 kHz
      asF <- filter(asF, (detectorName != dtf) | (peak > 5 & peak < 80))
      # remove clicks with durations less than 2 ms or greater than 1000 ms
      asF <- filter(asF, (detectorName != dtf) | (duration > 2 & duration < 1000))
    }

    # filters applied to INDIVIDUAL click detectors
    # remove clicks missclassified in certain detectors based on peak frequency
    # dtf must be defined each time bc filter condition string has length limit
    dtf = 'Click_Detector_1'
    asF = filter(asF, (detectorName != dtf) | (peak > 2 & peak < 15))
    dtf = 'Click_Detector_2'
    asF = filter(asF, (detectorName != dtf) | (peak > 15 & peak < 30))
    dtf = 'Click_Detector_3'
    asF = filter(asF, (detectorName != dtf) | (peak > 30 & peak < 50))
    dtf = 'Click_Detector_4'
    asF = filter(asF, (detectorName != dtf) | (peak > 30 & peak < 50))
    dtf = 'Click_Detector_5'
    asF = filter(asF, (detectorName != dtf) | (peak > 50 & peak < 80))

    # # for checking
    # oc_asF = getDetectorData(asF)$click
    # oc_asF_c1 = oc_asF[which(oc_asF$detectorName == 'Click_Detector_1'),]
    # oc_asF_c2 = oc_asF[which(oc_asF$detectorName == 'Click_Detector_2'),]
    # oc_asF_c3 = oc_asF[which(oc_asF$detectorName == 'Click_Detector_3'),]
    # oc_asF_c4 = oc_asF[which(oc_asF$detectorName == 'Click_Detector_4'),]
    # oc_asF_c5 = oc_asF[which(oc_asF$detectorName == 'Click_Detector_5'),]
    #
    # output AcousticStudy or AcousticEvent
    return(asF)
  }
