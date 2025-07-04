% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wav2flac.R
\name{wav2flac}
\alias{wav2flac}
\title{wav2flac}
\usage{
wav2flac(path_flac, inDir, outDir, numCh = 1)
}
\arguments{
\item{path_flac}{Character. Path to flac executable
e.g., \code{"C:\\flac-1.5.0-win\\Win64\\flac.exe"}}

\item{inDir}{Character. Path to the folder containing WAV files}

\item{outDir}{Character. Path to the folder where FLAC files will be saved.
\emph{This path MUST end in a final slash. A check will ensure that it does.}}

\item{numCh}{numeric. Number of channels. Default is 1}
}
\value{
None. Writes FLAC files to disk.
}
\description{
Utility to convert WAV files to FLAC format using the command
line flac.exe encoder.

This function converts a directory of .wav files to .flac and saves the new
files to the specified output directory. It requires the free FLAC
codec/executable which can be downloaded from:
\href{https://xiph.org/flac/download.html}{https://xiph.org/flac/download.html}.
}
\examples{
# single channel data
\dontrun{
wav2flac('C:\\\\users\\\\user.name\\\\programs\\\\flac-1.5.0-win\\\\Win64\\\\flac',
'F:\\\\wavFiles', 'F:\\\\flacFiles\\\\')
}
# multichannel data
\dontrun{
wav2flac('C:\\\\users\\\\user.name\\\\programs\\\\flac-1.5.0-win\\\\Win64\\\\flac',
'F:\\\\wavFiles', 'F:\\\\flacFiles\\\\', 4)
}

}
\seealso{
\code{\link[tcltk]{tk_choose.dir}}
Note: This function uses \code{tcltk::tk_choose.dir()} which may require a
working installation of Tcl/Tk.
On Linux and macOS, you may need to install Tcl/Tk separately.

\code{\link[crputils]{flac2wav}}
}
\author{
Selene Fregosi
}
