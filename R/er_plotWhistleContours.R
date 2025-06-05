#' er_plotWhistleContours
#'
#' @description Plot all whistle contours
#'
#' @param dets AcousticStudy containing whistle detections
#' @param eventUID unique ID for the event being processed (to reference within
#' the AcousticStudy)
#' @param xMax optional argument for upper limit of x axis, in seconds. Default
#' is 1.5 seconds
#' @param yMax optional argument for upper limit of y axis, in kHz. Default is
#' 20 kHz
#'
#' @export
#' @importFrom graphics lines
#' @importFrom grDevices rgb
#'
#' @examples
#' \dontrun{
#' er_plotWhistleContours(dets, eventUID, xMax = 1.5, yMax = 20)
#' }

er_plotWhistleContours <- function(dets, eventUID, xMax = 1.5, yMax = 20){

  # map the needed binary files
  binFiles <- dets@events[[eventUID]]@files$binaries
  wmFileIdx <- grep(pattern = '^.*WhistlesMoans_Whistle_and_Moan_Detector_Contours.*\\.pgdf$',
                    binFiles)
  wmFiles <- dets@events[[eventUID]]@files$binaries[wmFileIdx]

  # load them
  whBin <- loadMultiBinaries(wmFiles)
  # trim to just the event time
  whBinEv <- whBin[names(whBin) %in%
                     dets[[eventUID]]$Whistle_and_Moan_Detector$UID]

  #plot - ggplot version/functionized
  # pc <- plotContours(whBinEv)
  # print(pc)
  # pc + ggtitle(eventUID)
  #

  # get plot limits
  # xMax <- 0
  # yMax <- 0
  # for (wc in 1:length(names(whBinEv))){
  #   df <- data.frame(time = whBinEv[[wc]]$time - whBinEv[[wc]]$time[1],
  #                    freq = whBinEv[[wc]]$freq/1000)
  #   xMaxTmp <- max(df$time)
  #   yMaxTmp <- max(df$freq)
  #   if (xMaxTmp > xMax){xMax <- xMaxTmp}
  #   if (yMaxTmp > yMax){yMax <- yMaxTmp}
  # }


  # plot each line
  plot(1, type = 'n', xlim = c(0, round(xMax, 2)), ylim = c(0, round(yMax, -1)),
       xlab = 'Time (s)', ylab = 'Frequency (kHz)',
       main = 'Whistle Contours')
  # add grid lines for frequency at 0.125 s and 5 kHz intervals
  for (iline in (seq(0, 2, 0.125))) {lines(c(iline,iline), c(-10,yMax + 10),
                                         col="lightgray")}
  for (iline in (seq(0, 50, 5))) {lines(c(-0.5, xMax + 1), c(iline,iline),
                                      col="lightgray")}
  # loop through each contour
  for (wc in 1:length(names(whBinEv))){
    df <- data.frame(time = whBinEv[[wc]]$time - whBinEv[[wc]]$time[1],
                     freq = whBinEv[[wc]]$freq/1000)
    lines(df$time, df$freq, col = rgb(0,0,0,0.4))

  }
}


