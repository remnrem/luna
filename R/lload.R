# --------------------------------------------------------------------------------
#
# R script to merge all Luna output into a single file
#
# --------------------------------------------------------------------------------

library(data.table)

lhead <- function(filename,
                  prefix = NULL,
                  comment.char = "#") {
  if (!is.null(prefix)) filename <- paste(prefix, filename, sep = "/")
  df <- read.table(filename, header = T, stringsAsFactors = F, sep = "\t", nrows = 1, comment.char = comment.char)
  names(df)
}

lload <- function(filename,
                  take.logs = NULL,
                  factors = NULL,
                  rows = NULL,
                  fixed = NULL,
                  variables = NULL,
                  prefix = NULL,
                  comment.char = "#") {
  # filename
  if (!is.null(prefix)) filename <- paste(prefix, filename, sep = "/")

  # load data table
  df <- read.table(filename, header = T, stringsAsFactors = F, sep = "\t", comment.char = comment.char)

  # if any variables need to be log-transformed
  df[, take.logs] <- log(df[, take.logs])

  # optionally, add any 'fixed' (level-specified) factors
  if (!is.null(fixed)) {
    for (v in names(fixed)) df[, v] <- fixed[[v]]
    factors <- c(factors, names(fixed))
  }

  # optionally, extract only a subset of variables (plus ID, (row) factors and variables)
  if (!is.null(variables)) df <- df[, c("ID", rows, factors, variables)]

  # collect variable names
  variables <- names(df)[!names(df) %in% c("ID", rows, factors)]

  # optionally, construct accompanying meta-data list (vars/factors/levels)
  if (!is.null(factors)) {
    df$tmp.sort.idx999 <- "A"
    df.meta <- unique(df[order(apply(df[, c("tmp.sort.idx999", factors), drop = F], 1, paste, collapse = "_")), factors, drop = F])
    df$tmp.sort.idx999 <- NULL
    df.meta <- data.frame(
      rep(variables, each = dim(df.meta)[1]),
      do.call("rbind", replicate(length(variables), df.meta, simplify = FALSE))
    )
    rownames(df.meta) <- NULL
    names(df.meta) <- c("BASE", factors)
    coloffset <- 1 + length(rows)
    df.meta$COL <- (1 + coloffset):(dim(df.meta)[1] + coloffset)
    df.meta$BASE <- as.character(df.meta$BASE)
  } else {
    df.meta <- NULL
  }

  # Luna output structure: ID { factors } { variables }
  if (!is.null(factors)) {
    df <- setDF(dcast(setDT(df),
      as.formula(paste(c(
        paste(c("ID", rows), sep = "+", collapse = "+"),
        "~",
        paste(factors, sep = "+", collapse = "+")
      ), collapse = " ")),
      value.var = variables
    ))
    # force base name back in if only one specified
    if (length(variables) == 1) names(df)[-(1:coloffset)] <- paste(variables, names(df)[-(1:coloffset)], sep = "_")
  }

  # sanity check if df.meta was generated
  if ((!is.null(factors)) && dim(df)[2] != dim(df.meta)[1] + coloffset) stop("internal error in lload()")

  # add full names
  if (!is.null(factors)) df.meta$VAR <- names(df)[-(1:coloffset)]

  # return compiled R data frame and meta-data  ('df' and 'mt')
  list(df = df, mt = df.meta)
}

lcols <- function(df.meta, variables = NULL, factors = list(), ret.col = "VAR") {
  # extract variables (or all variables)
  if (is.null(variables)) {
    idx <- rep(T, dim(df.meta)[1])
  } else {
    idx <- df.meta$BASE %in% variables
  }

  # extract any factors, assuming a list structure list
  for (fac in names(factors)) idx[!df.meta[, fac] %in% factors[[fac]]] <- F
  df.meta[idx, ret.col]
}
