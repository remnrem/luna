

## collate all headers

x <- list.files(pattern = "\\.h$", path = "luna-base", recursive = TRUE)

## special case for Eigen header-only library

xeigen <- list.files(path = "luna-base/stats/Eigen", recursive = TRUE)
xeigen <- file.path("stats/Eigen", xeigen)
x <- unique(c(x, xeigen))

## copy over to the include path for lunaR build

xx <- file.path("include", x)
x <- file.path("luna-base", x)
dirs <- unique(dirname(xx))
sapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)
file.rename(x, xx)
