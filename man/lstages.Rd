% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/luna.R
\name{lstages}
\alias{lstages}
\title{Return vector of sleep stages from annotations}
\usage{
lstages()
}
\value{
a vector of sleep stage labels,
i.e. based on attached annotations. For NSRR and other data, Luna expects a
standard format for stage names, although this can be modified.
See \href{https://zzz.bwh.harvard.edu/luna/ref/hypnograms/#stage}{here} for more information.
}
\description{
Returns a vector of sleep stages based on annotation data
}
\note{
This command is identical to running:
\preformatted{
leval("STAGE")$STAGE$E$STAGE
}
}
