% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/luna.R
\name{ltopo.xy}
\alias{ltopo.xy}
\title{LUNA plotting}
\usage{
ltopo.xy(
  c,
  x,
  y,
  z = NA,
  zlim = NA,
  f = rep(T, length(x)),
  y.symm = F,
  sz = 0.08,
  col = "black",
  lwd = 0.5,
  xline = numeric(),
  yline = numeric(),
  pch = NA,
  cex = 1,
  ylim = NULL,
  xlab = "Frequency (Hz)",
  ylab = "log(power)",
  mt = ""
)
}
\arguments{
\item{c}{character vector of channel labels}

\item{x}{nuremic vector of X-axis values}

\item{y}{numeric vector of Y-axis values}

\item{z}{optional numeric vector of Z-axis values (default NA)}

\item{zlim}{optional range for Z}

\item{f}{optional boolean vector of length(x), filter in/out each observation}

\item{y.symm}{boolean, make Y-axis symmetric around 0 (default = F)}

\item{sz}{optional numeric value: size of points (default 0.08)}

\item{col}{optional color, default 'black'}

\item{lwd}{optional numeric value for line width (default to 0.5)}

\item{xline}{optional vector of X values to plot vertical lines}

\item{yline}{optional vector of Y values to plot horizontal lines}

\item{pch}{optional point symbol (default NA)}

\item{cex}{optional point size (default 1)}

\item{ylim}{optional Y axis limit}

\item{xlab}{X axis label (default = Frequency Hz))}

\item{ylab}{Y axis label (default = log(power))}

\item{mt}{main title (default, "")}
}
\value{
plot is generated in the current graphics device; no return value
}
\description{
draw a series of X-Y scatter/line plots, one-per-channel arranged as a topoplot
}
\note{
If \code{pch} is non-\code{NULL}, then \code{z} can be a vector of values
(\code{length(x)}) and \code{col} can be a 100-element palette: in this case,
the color of each point is scaled by the percentile of \code{z};
if \code{pch} is missing, this function draws a X-Y line
plots, which must be a single color (i.e. no \code{z} values are allowed)
}
\examples{
\dontrun{
ltopo.xy(c = hj$CH, x = hj$E, y = log(hj$H1), xlab = "Epoch", ylab = "H1",
    pch=20, col=rbpal, cex = 0.2)
}

}
