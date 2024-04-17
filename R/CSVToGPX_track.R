#' CSVToGPX_track
#'
#' @description convert a .csv of vessel track segments to a .gpx file.
#'
#' The input data.frame must have 1 column 'uid' containing a unique
#' identifier for that track/segment, 2 columns for start latitude and
#' longitude, and 2 columns for stop latitude and longitude.
#' Additional columns for cruise, date, vessel name, etc will be ignored
#'
#' This is a modified version of `trackToGPX.R` that is included in the
#' 'cruise-maps-live' repository that tries to generalize the function to
#' convert any input data.frame, not just the output from a .das file.
#'
#' @param inCSV filename of csv containing track data to be processed. Must
#' include the following columns:
#' uid, startLat, startLon, stopLat, stopLon
#' where uid is a unique track identifier, startLat/startLon is the track
#' start location and stopLat/stopLon is the track end location.
#' All locations should be in decimal degrees.
#' Can optionally include a startDateTime and stopDateTime column. Assumes
#' UTC timezone for GPX output.
#' @param outGPX fullpath filename of gpx to be written
#' example: outGPX = './crputils/exampleData/exampleVesselTrack.gpx'
#'
#' @return none, will write a file
#' @export
#'
#' @examples
#' inCSV = './exampleData/exampleVesselTrack.csv'
#' outGPX = './exampleData/exampleVesselTrack.gpx'
#' CSVToGPX_track(inCSV, outGPX)
#'

CSVToGPX_track <- function(inCSV, outGPX){

  # read in CSV
  tdf <- read.csv(inCSV)
  # Coerce tdf (track data frame) into a simplified longform format

  # trim to just essential columns
  trimCols <- c('uid', 'startLat', 'startLon', 'stopLat', 'stopLon')
  # check if dateTime columns exist, and include if they do
  if ('startDateTime' %in% colnames(tdf) && 'stopDateTime' %in% colnames(tdf)){
    trimCols <- c(trimCols, 'startDateTime', 'stopDateTime')
  }
  tt = subset(tdf, select = trimCols)

  # if any segnums are duplicated, print error message
  if (any(duplicated(tt$uid))){
    stop('Warning: duplicate unique identifiers. Correct in .csv and try again')
  }

  # reshape to long format with 'status' tag for start or end of each segment
  # if startDateTime and 2 are present, include that
  varyList <- list(lat = c('startLat', 'stopLat'), lon = c('startLon', 'stopLon'))
  varyNames <- c('lat', 'lon')
  if ('startDateTime' %in% colnames(tt) && 'stopDateTime' %in% colnames(tt)){
    varyList$dt <- c('startDateTime', 'stopDateTime')
    varyNames <- c(varyNames, 'dateTime')
  } else if ('dateTime' %in% colnames(tt)){
    varyList$dt <- c('dateTime')
    varyNames <- c(varyNames, 'dateTime')
  }else if ( 'date' %in% colnames(tt)){
    varyList$dt <- c('date')
    varyNames <- c(varyNames, 'date')
  }

  ttl <- reshape(tt, direction = 'long',
                idvar = 'uid',
                ids = tt$uid,
                varying = varyList,
                v.names = varyNames,
                timevar = 'status',
                times = c('start', 'stop'))

  # reorder by uid
  ttl <- ttl[order(ttl$uid),]

  # create datetime col with proper formatting for gpx
  # several options provided, could be expanded for other formats
  if ('dateTime' %in% colnames(ttl)){
    ttl$dateTime <- as.POSIXlt(ttl$dateTime, tz = 'UTC',
                              tryFormats = c("%Y-%m-%d %H:%M:%OS",
                                             "%Y/%m/%d %H:%M:%OS",
                                             "%Y-%m-%d %H:%M",
                                             "%Y/%m/%d %H:%M",
                                             "%Y-%m-%d",
                                             "%Y/%m/%d",
                                             '%m/%d/%Y %H:%M'))
    ttl$dt <- format(ttl$dateTime, format = "%Y-%m-%dT%H:%M:%S%z")
  }

  # set up the GPX file header info
  # this is copied from a gpx file made using an online converter
  gpxHeader_xmlVer <- '<?xml version="1.0" encoding="utf-8" standalone="yes"?>';
  # The websites and such might not be necessary...
  gpxHeader_gpxVer <- paste('<gpx version="1.1" creator="R CSVToGPX_track.R"',
                           'xmlns="http://www.topografix.com/GPX/1/1"',
                           'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                           'xsi:schemaLocation="http://www.topografix.com/GPX/1/1',
                           'http://www.topografix.com/GPX/1/1/gpx.xsd">')
  # metadata - other things could be added here.
  gpxHeader_metadata <- paste0('<metadata><bounds ',
                              'minlat="', format(min(ttl$lat), nsmall = 6), '" ',
                              'minlon="', format(min(ttl$lon), nsmall = 6), '" ',
                              'maxlat="', format(max(ttl$lat), nsmall = 6), '" ',
                              'maxlon="', format(max(ttl$lon), nsmall = 6), '"',
                              '/></metadata>')



  # start assembling the file contents, in 'o'
  o <- c(gpxHeader_xmlVer,
        gpxHeader_gpxVer,
        gpxHeader_metadata
  )

  # loop through all uids
  uidList <- unique(ttl$uid)

  # Each UID = one track made of one track segment
  for (u in 1:length(uidList)){
    uid <- uidList[u]
    uIdx <- which(ttl$uid == uid)
    # add to output string
    o <- c(o, '<trk>',
          paste0('  <name>', uidList[u], '</name>'),
          '  <trkseg>')

    for (j in 1:2){
      o <- c(o,
            paste0('    <trkpt lat="', ttl$lat[uIdx][j],
                   '" lon="', ttl$lon[uIdx][j],'">')
      )
      if ('dateTime' %in% colnames(ttl)){
        o <- c(o, paste0('      <time>', ttl$dt[uIdx][j],'</time>'))
      }
      o <- c(o, '    </trkpt>')
    }
    o <- c(o,
          '  </trkseg>',
          '</trk>'
    )

  }

  # wrap up the file
  o <- c(o, '</gpx>')

  # open the file, write it, and close it
  fileConn <- file(outGPX)
  writeLines(o, fileConn)
  close(fileConn)

}
