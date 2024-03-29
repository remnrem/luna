% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/luna.R
\name{lwin}
\alias{lwin}
\title{Winsorize a vector}
\usage{
lwin(x, p = 0.05)
}
\arguments{
\item{x}{a numeric vector to be winsorized}

\item{p}{percentile, must be less than 0.5 (defaults to 0.05, implying 5th and 95th percentiles)}
}
\value{
winsorized version of \code{x}
}
\description{
Winsorizes a vector (sets values greater/less than the specified percentile to that value)
}
