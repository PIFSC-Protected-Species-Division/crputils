% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/er_whistleSummary.R
\name{er_whistleSummary}
\alias{er_whistleSummary}
\title{er_whistleSummary}
\usage{
er_whistleSummary(asF, eventUID)
}
\arguments{
\item{asF}{AcousticStudy that has been filtered to remove noise}

\item{eventUID}{ID of a single acoustic event}
}
\value{
'wl' list with 3 elements: total number of whistles, PAMpal whistle
data for each whistle, and 'mt', the formatted table of median summary
statistics
}
\description{
Summarize whistles for a single AcousticStudy event, to be used
within the event_summary_report Rmd. Summarizes total number of whistles and
calculates some summary statistics (begin and end frequency, mean frequency,
duration, and frequency range) output into a nicely formatted table.
}
\examples{
\dontrun{
wl <- er_whistleSummary(detsFilt, eventUID)
}
}
