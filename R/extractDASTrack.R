#' extractDASTrack
#'
#' @description Pull effort tracks from a relatively raw daily .das file into
#' a simpler dataframe. Utilizes the package 'swfscDAS' and then cleans up those
#' outputs a bit. Generalized from cruise-maps-live's extractTrack()
#'
#' @author Selene Fregosi
#'
#' @param df_proc processed das file (with swfscDAS::das_process)
#'
#' @returns a dataframe of effort tracks with date and lat/lon
#'
#' @export
#' @importFrom swfscDAS das_effort
#'
# examples
# # extract all new tracks from a given das file, d$name
# et <- extractDASTrack(here('inputs', d$name))
#

extractDASTrack <- function(df_proc){

  # col headers throw warning in build, this fixes
  Cruise <- segnum <- stlin <- mtime <- Mode <- EffType <- avgSpdKt <- avgBft <- NULL

  # summarize effort segments. 'section' method pulls lat/lon for all 'R'
  # (resume effort) and all 'E' (end effort) entries, then calcs dist between
  if (any(df_proc$Event == 'R')){ # check for any on effort segments

    et_all <- swfscDAS::das_effort(df_proc, method = 'section',
                                   dist.method = 'greatcircle', num.cores = 1)
    # trim to just what we want
    et_seg <- et_all$segdata
    et <- subset(et_seg, select = c(Cruise, segnum, file, stlin:mtime, Mode,
                                   EffType, avgSpdKt, avgBft))

    # rename the file_das column to match tracks output
    colnames(et)[3] <- 'file_das'

    # effort types can be 'N' non-standard, 'S' standard', and 'F' fine-scale
    # could further trim by this.
  } else {
    et <- NULL
    warning('No segments to extract')
  }

  return(et)
}
