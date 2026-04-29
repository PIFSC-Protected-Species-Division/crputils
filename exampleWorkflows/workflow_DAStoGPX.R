# convert a DAS file to a track csv

library(swfscDAS)

dasFile <- file.path("C:/users/selene.fregosi/downloads/DASALL_SE2601_Leg1.das")

# check and process das file
df_check <- das_check(dasFile, skip = 0, print.cruise.nums = FALSE)
df_read <- das_read(dasFile, skip = 0)
df_proc <- das_process(dasFile)

# extract effort tracks
source(file.path("C:/users/selene.fregosi/documents/github/cruise-maps-live",
                 "code", "functions", "extractTrack.R"))
et <- extractTrack(df_proc)


# export to GPX file
source(file.path("C:/users/selene.fregosi/documents/github/cruise-maps-live",
                 "code", "functions", "trackToGPX.R"))
outGPX = file.path("C:/users/selene.fregosi/documents/github/glider-whiceas/outputs",
                   paste0('effortTracks_SE2601_leg1.gpx'))
trackToGPX(et, outGPX)
etTrim <- subset(et, select = c(Cruise, segnum, lat1, lon1, DateTime1,
                               lat2, lon2, DateTime2, avgBft))
