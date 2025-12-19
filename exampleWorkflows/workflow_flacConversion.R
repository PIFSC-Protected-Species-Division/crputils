
library(crputils)


# wav to flac
path_flac <- "C:/users/user.name/programs/flac-1.5.0-win/Win64/flac.exe"
# path_flac <- "" # prompt

inDir <- "C:\\users/user.name\\flac_test\\wav"
outDir <- "C:\\users/user.name\\flac_test\\flacOut"
numCh <- 3

wav2flac(path_flac, inDir, outDir, numCh)



# flac to wav
path_flac <- "C:/users/user.name/programs/flac-1.5.0-win/Win64/flac.exe"
# path_flac <- "" # prompt

inDir <- "C:\\users/user.name\\flac_test\\flac"
outDir <- "C:\\users/user.name\\flac_test\\wavOut"
numCh <- 1

flac2wav(path_flac, inDir, outDir, numCh)

