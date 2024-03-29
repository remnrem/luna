% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/luna.R
\name{ltxttab}
\alias{ltxttab}
\title{Loads and conconcatenates multiple text-table output files}
\usage{
ltxttab(root, f = "", ids = dir(root), silent = F)
}
\arguments{
\item{root}{a required parameter, the folder containing the output}

\item{f}{(optional) the name of the file to load from each subfolder of \code{root}}

\item{ids}{(optional) to specify that only a subset of individuals are loaded
(subfolder names are expected to correspond to IDs)}

\item{silent}{(optional) if set to true makes this function runs silently
(not output to console)}
}
\value{
a \code{data.frame} containing row-concatenated data from the subfolders of \code{root}
}
\description{
Loads and concatenates text-table format \emph{Luna} (\code{-t} mode) output
}
\examples{
\dontrun{
## this luna command will make a out1 subfolder
## \% luna s.lst -t out1 -s 'MASK ifnot=NREM2 & RE & PSD spectrum sig=EEG'

## \% ls out1/
##  nsrr01  nsrr02  nsrr03

## each subfolder will contain iodentical file names.
## \% ls out1/nsrr01
##  MASK-EPOCH_MASK.txt  PSD-B,CH.txt  PSD-CH.txt  PSD-F,CH.txt  RE.txt

## get an enumeration of all files
> ltxttab("out1")
MASK-EPOCH_MASK.txt        PSD-B,CH.txt          PSD-CH.txt        PSD-F,CH.txt
                  3                   3                   3                   3
RE.txt
     3

## to load all PSD-B,CH.txt files:
> d <- ltxttab("out1", "PSD-B,CH.txt")
> table(d$ID)
nsrr01 nsrr02 nsrr03
    10     10     10
}

}
