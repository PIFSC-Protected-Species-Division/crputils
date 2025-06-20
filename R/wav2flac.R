#' wav2flac
#'
#' @description Utility to convert WAV files to FLAC format using the command
#' line flac.exe encoder.
#'
#' This function converts a directory of .wav files to .flac and saves the new
#' files to the specified output directory. It requires the free FLAC
#' codec/executable which can be downloaded from:
#' \href{https://xiph.org/flac/download.html}{https://xiph.org/flac/download.html}.
#'
#' @author Selene Fregosi
#'
#' @param path_flac Character. Path to flac executable
#' e.g., \code{"C:\\flac-1.5.0-win\\Win64\\flac.exe"}
#' @param inDir Character. Path to the folder containing WAV files
#' @param outDir Character. Path to the folder where FLAC files will be saved.
#' *This path MUST end in a final slash. A check will ensure that it does.*
#' @param numCh numeric. Number of channels. Default is 1
#'
#' @return None. Writes FLAC files to disk.
#'
#' @export
#'
#' @seealso \code{\link[tcltk]{tk_choose.dir}}
#' Note: This function uses \code{tcltk::tk_choose.dir()} which may require a
#' working installation of Tcl/Tk.
#' On Linux and macOS, you may need to install Tcl/Tk separately.
#' @seealso \code{\link[crputils]{flac2wav}}
#'
#' @examples
#' # single channel data
#' \dontrun{
#' wav2flac('C:\\users\\user.name\\programs\\flac-1.5.0-win\\Win64\\flac',
#' 'F:\\wavFiles', 'F:\\flacFiles\\')
#' }
#' # multichannel data
#' \dontrun{
#' wav2flac('C:\\users\\user.name\\programs\\flac-1.5.0-win\\Win64\\flac',
#' 'F:\\wavFiles', 'F:\\flacFiles\\', 4)
#' }
#'
wav2flac <- function(path_flac, inDir, outDir, numCh = 1) {

  # Ensure path_flac points to an executable
  if (!file.exists(path_flac)) {
    # stop("flac.exe not found. Please provide a valid path to flac.exe but without '.exe'")
    message("flac.exe not found. Please provide a valid path to flac.exe but without '.exe'")

    # Use tcltk::tk_choose.dir to select the proper folder
    if (!requireNamespace("tcltk", quietly = TRUE)) {
      stop("tcltk is required for folder selection on non-Windows systems. Please install it.")
    }
    path_flac <- tcltk::tk_choose.files(caption = "Select flac.exe")

    if (is.na(path_flac) || path_flac == "") {
      stop("No flac.exe selected. Exiting.")
    }
  }
  path_flac <- normalizePath(path_flac, winslash = "/")

  # Validate input directory
  if (!dir.exists(inDir)) {
    message("Input directory not found. Please select a folder.")

    # Use tcltk::tk_choose.dir to select the proper folder
    if (!requireNamespace("tcltk", quietly = TRUE)) {
      stop("tcltk is required for folder selection on non-Windows systems. Please install it.")
    }
    inDir <- tcltk::tk_choose.dir(caption = "Select folder containing WAV files")

    if (is.na(inDir) || inDir == "") {
      stop("No folder selected. Exiting.")
    }
  }
  inDir <- normalizePath(inDir, winslash = "/")
  cat("inDir:", inDir, "\n")

  # Validate or create output directory
  if (!dir.exists(outDir)) {
    dir.create(outDir, recursive = TRUE)
  }
  outDir <- normalizePath(outDir, winslash = "/")
  # Ensure output path ends with /
  if (!grepl("[/\\\\]$", outDir)) {
    outDir <- paste0(outDir, .Platform$file.sep)
  }
  cat("outDir:", outDir, "\n")

  # Get list of .wav files
  wav_files <- list.files(inDir, pattern = "\\.wav$", full.names = TRUE)
  if (length(wav_files) == 0) {
    stop("No WAV files found in input directory.")
  }

  # Process files
  start_time <- Sys.time()
  for (i in seq_along(wav_files)) {
    in_file <- wav_files[i]
    if (numCh == 1){
      cmd <- paste0(path_flac, " --keep-foreign-metadata-if-present", " --output-prefix=",
                    outDir, " ", in_file)
    } else if (numCh > 1){
      cmd <- paste0(path_flac, " --keep-foreign-metadata-if-present", " --channel-map=none",
                   " --output-prefix=", outDir, " ", in_file)
    }
    status <- system(cmd, intern = TRUE)
    cat("Done with file", i, "of", length(wav_files), "-", basename(in_file), "\n")
  }

  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  cat("Done processing", inDir, "\nElapsed time", round(elapsed), "seconds\n")
}
