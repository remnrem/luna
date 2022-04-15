outlier <- function(x, t = 3) {
  mean(x, na.rm = T) + t * sd(x, na.rm = T)
}

outliers <- function(x, m = mean(x, na.rm = T), sdev = sd(x, na.rm = T), t = 3) {
  lwr <- m - t * sdev
  upr <- m + t * sdev
  x[x < lwr] <- NA
  x[x > upr] <- NA
  x
}

outliers.inc <- function(x, inc = rep(T, length(x)), m = mean(x[inc], na.rm = T), sdev = sd(x[inc], na.rm = T), t = 3) {
  lwr <- m - t * sdev
  upr <- m + t * sdev
  x[x < lwr] <- NA
  x[x > upr] <- NA
  x[!inc] <- NA
  x
}

is.outlier <- function(x, m = mean(x, na.rm = T), sdev = sd(x, na.rm = T), t = 3) {
  is.na((!is.na(x)) & outliers(x, m, sdev, t))
}

is.outlier.inc <- function(x, inc = inc, t = 3) {
  m <- mean(x[inc], na.rm = T)
  sdev <- sd(x[inc], na.rm = T)
  (!inc) | is.na((!is.na(x)) & outliers(x, m, sdev, t))
}

## Normalisation

zf <- function(x, m = mean(x, na.rm = T), sdev = sd(x, na.rm = T)) {
  (x - m) / sdev
}
