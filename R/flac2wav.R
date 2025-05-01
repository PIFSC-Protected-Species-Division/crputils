#' flac2wav
#'
#' @description Utility to decode FLAC files to WAV format using the command
#' line flac.exe encoder.
#'
#' This function converts all .flac files in a directory to .wav using the
#' flac.exe encoder.
#'
#' @author Selene Fregosi
#'
#' @param path_flac Character. Path to flac executable
#' e.g., 'C:\flac-1.5.0-win\Win64\flac.exe'
#' @param inDir Character. Path to the folder containing FLAC files
#' @param outDir Character. Path to the folder where WAV files will be saved.
#' *This path MUST end in a final slash. A check will ensure that it does.*
#'
#' @return None. Writes WAV files to disk.
#'
#' @export
#'
#' @seealso \code{\link[tcltk]{tk_choose.dir}}
#' Note: This function uses \code{tcltk::tk_choose.dir()} which may require a
#' working installation of Tcl/Tk.
#' On Linux and macOS, you may need to install Tcl/Tk separately.
#' @seealso \code{\link[crputils]{wav2flac}}
#'
#' @examples
#' flac2wav('C:\\users\\user.name\\programs\\flac-1.5.0-win\\Win64\\flac',
#' 'F:\\flacFiles', 'F:\\wavFiles\\');
#'
flac2wav <- function(path_flac, inDir, outDir) {

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
    inDir <- tcltk::tk_choose.dir(caption = "Select folder containing FLAC files")

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
  flac_files <- list.files(inDir, pattern = "\\.flac$", full.names = TRUE)
  if (length(flac_files) == 0) {
    stop("No FLAC files found in input directory.")
  }

  # Process files
  start_time <- Sys.time()
  for (i in seq_along(flac_files)) {
    in_file <- flac_files[i]
    cmd <- paste0(path_flac, " --decode", " --keep-foreign-metadata-if-present",
    " --output-prefix=", outDir, " ", in_file)
    status <- system(cmd, intern = TRUE)
    cat("Done with file", i, "of", length(flac_files), "-", basename(in_file), "\n")
  }

  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  cat("Done processing", inDir, "\nElapsed time", round(elapsed), "seconds\n")
}
