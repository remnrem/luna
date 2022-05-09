# this function is not used anywhere else.
outlier <- function(x, t = 3) {
  mean(x, na.rm = T) + t * sd(x, na.rm = T)
}

#' Filter outliers
#'
#' Set outliers in a vector to \code{NA}
#'
#' @param x numeric vector
#' @param m target center of acceptable interval after outlier removal,
#' default to \code{x}'s mean
#' @param sdev target variation of acceptable interval, default to \code{x}'s
#' standard deviation
#' @param t scale of deviation, default to 3
#'
#' @return \code{x} after setting outliers to \code{NA}
#' @export
outliers <- function(x, m = mean(x, na.rm = T), sdev = sd(x, na.rm = T), t = 3) {
  lwr <- m - t * sdev
  upr <- m + t * sdev
  x[x < lwr] <- NA
  x[x > upr] <- NA
  x
}

#' Filter outliers with masking
#'
#' Set outliers in a vector to \code{NA}
#'
#' @inheritParams outliers
#'
#' @param inc boolean vector with same length as \code{x}, used for masking.
#' \code{FALSE} will set the element to \code{NA} in the corresponding position
#' in \code{x}.
#'
#' @return \code{x} after setting outliers to \code{NA}
#' @export
#'
outliers.inc <- function(x, inc = rep(T, length(x)), m = mean(x[inc], na.rm = T), sdev = sd(x[inc], na.rm = T), t = 3) {
  lwr <- m - t * sdev
  upr <- m + t * sdev
  x[x < lwr] <- NA
  x[x > upr] <- NA
  x[!inc] <- NA
  x
}

#' Check outliers
#'
#' @inheritParams outliers
#'
#' @return boolean vector with same length as \code{x}, \code{TRUE} means the corresponding
#' element in \code{x} is an outlier and is not \code{NA}
#' @export
is.outlier <- function(x, m = mean(x, na.rm = T), sdev = sd(x, na.rm = T), t = 3) {
  is.na((!is.na(x)) & outliers(x, m, sdev, t))
}

#' Check outliers with masking
#'
#' @param x numeric vector
#' @param inc boolean vector with same length as \code{x}, used for masking.
#' \code{FALSE} will exclude the element to \code{NA} in the corresponding position
#' in \code{x} from calculation.
#' @param t scale of deviation, default to 3, as in \code{\link{outliers}}
#'
#' @return boolean vector with same length as \code{x}, \code{TRUE} means the corresponding
#' element in \code{x} is an outlier and is not \code{NA}, or it is excluded by \code{inc}
#' @export
is.outlier.inc <- function(x, inc = inc, t = 3) {
  m <- mean(x[inc], na.rm = T)
  sdev <- sd(x[inc], na.rm = T)
  (!inc) | is.na((!is.na(x)) & outliers(x, m, sdev, t))
}

## Normalisation

#' Vector normalization
#'
#' @param x numeric vector
#' @param m target center of normalization, default to \code{x}'s mean
#' @param sdev target deviation of normalization, default to \code{x}'s
#' standard deviation
#'
#' @return normalized vector with same length as \code{x}
#' @export
zf <- function(x, m = mean(x, na.rm = T), sdev = sd(x, na.rm = T)) {
  (x - m) / sdev
}
