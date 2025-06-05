#' loadMultiBinaries
#'
#' @description Load in multiple Pamguard binary files and compile the data
#' lists into a single list. For now it works with WMD data...
#'
#' @param wmBinFiles character array of whistle moan binary files to be loaded
#'
#' @return binOut a list of binary WMD data
#'
#' @export
#' @importFrom PamBinaries loadPamguardBinaryFile contourToFreq
#'
#' @examples
#' \dontrun{
#' # map the needed binary files
#' binFiles <- dets@events[[eventUID]]@files$binaries
#' wmFileIdx <- grep(pattern = '^.*WhistlesMoans_Whistle_and_Moan_Detector_Contours.*\\.pgdf$',
#'                   binFiles)
#' wmFiles <- dets@events[[eventUID]]@files$binaries[wmFileIdx]
#' # now load the binaries
#' loadMultiBinaries(wmFiles)
#' }


loadMultiBinaries <- function(wmBinFiles){

  binOut <- list() # create output that will compile across files
  for (f in 1:length(wmBinFiles)){
    whBin <- PamBinaries::loadPamguardBinaryFile(wmBinFiles[f], convertDate = TRUE)
    # get contour frequency and time info
    whBinCont <- PamBinaries::contourToFreq(whBin$data, verbose = FALSE) #changed from FALSE - JLKM (TRUE prints)
    # loop through each whistle and add to whOut
    whNames <- names(whBinCont)
    for (h in 1:length(whNames)){
      binOut[whNames[h]] <- whBinCont[whNames[h]]
    }
  }

  return(binOut)
}

