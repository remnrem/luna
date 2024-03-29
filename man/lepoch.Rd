% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/luna.R
\name{lepoch}
\alias{lepoch}
\title{Epochs the data}
\usage{
lepoch(dur = 30, inc = -1)
}
\arguments{
\item{dur}{the epoch duration in seconds}

\item{inc}{the epoch increment, defaults to \code{dur} (i.e. non-overlapping epochs)}
}
\value{
the number of epochs set
}
\description{
Sets epochs for the current EDF
}
\note{
\code{lepoch(30)} is identical to using the \code{\link{leval}()}
function to evaluate/execute an EPOCH command:
\preformatted{
leval("EPOCH dur=30")
}
}
\examples{
\dontrun{
> ne <- lepoch()
evaluating...
nsrr02 : 14 signals, 10 annotations, of 09:57:30 duration, 1195 unmasked 30-sec epochs, and 0 masked
> ne
[1] 1195
}
}
