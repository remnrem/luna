% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/luna.R
\name{ldata}
\alias{ldata}
\title{Extracts signal (and annotation) data from an EDF for one or more epochs}
\usage{
ldata(e, chs, annots = character(0))
}
\arguments{
\item{e}{a required vector of 1-based epoch numbers corresponding to the
current epoch numbering of the attached EDF}

\item{chs}{a required vector of channel names to be returned, i.e. as from \code{\link{lchs}()},
all of which are required to have the same sampling rate}

\item{annots}{an optional vector of annotation names to be returned, i.e. as from \code{link{lannots}()}}
}
\value{
a data frame where each row is one sample point, containing the raw
signal (and annotation) data
}
\description{
Returns a data frame of signals and annotations for requested epochs
}
\note{
The returned data frame has the following columns:
\itemize{
\item epoch number \code{E}
\item elapsed time since the start of the current first epoch \code{SEC}
\item any signals, in alphabetical order, where column/variable names are the channel labels,
potentially sanitized if need be to remove special characters with an underscore
(e.g. here \code{EEG(sec)} becomes \code{EEG_sec_})
\item any annotations, where \code{1} and \code{0} indicate the presence/absence of that annotation at that time-point
}
}
\section{WARNING}{

The following command would request all the data in an EDF (assuming that all
channels have similar sampling rates) and all associated annotation information:
\code{
ne <- lepoch()
d <- ldata(1:ne, lchs(), lannots())}
That is, for long EDFs, with a large number of channels and/or high sampling rates,
the size of the data frame returned by \code{ldata()} may be large (i.e. too large),
if many epochs, channels and/or annotations are requested.
}

\examples{
\dontrun{
## request EEG and EMG channels, for epochs 211 and 212, along with the
## arousal annotation
> d <- ldata( 211:212 , chs = c("EEG", "EMG") , annots = "arousal" )
> head(d)
    E      SEC       EEG        EMG arousal
1 211 6300.000 -4.411765 -2.8411765       0
2 211 6300.008 -5.392157  0.8647059       0
3 211 6300.016 -4.411765 -1.6058824       0
4 211 6300.024 -4.411765 -2.8411765       0
5 211 6300.032 -6.372549  1.3588235       0
6 211 6300.040 -6.372549 -0.1235294       0


## plotting the the two signals and annotation:
> par(mfcol=c(3,1),mar=c(1,4,2,1))
> plot( d$SEC , d$EEG , type="l" , lwd=0.5 , col="darkgray",ylab="EEG")
> plot( d$SEC , d$EMG , type="l" , lwd=0.5 , col="darkgray",ylab="EMG")
> plot( d$SEC , d$arousal , type="l" , col="blue" , lwd=2,ylab="Arousal")
}




}
\seealso{
If different channels have different sampling rates, use the \code{RESAMPLE}
command to resample one or more channels first, e.g.
\code{leval("RESAMPLE sig=ECG sr=125")}. Otherwise it will yield an error:
\preformatted{
Error in ldata(...) :
   requires uniform sampling rate across signals}
See \code{\link{leval}} for more details.
}
