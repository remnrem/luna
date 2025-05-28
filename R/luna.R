
####################################################
##                                                ##
## lunaR                                          ##
##                                                ##
####################################################

luna.globals <- new.env()

luna.globals$version <- "v1.2.3"
luna.globals$date <- "28-May-2025"
luna.globals$id <- ""
luna.globals$edf <- ""
luna.globals$annots <- ""
luna.globals$logmode <- 0
luna.globals$user_xy <- F

####################################################
##                                                ##
## Initialize                                     ##
##                                                ##
####################################################

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("** lunaR", luna.globals$version, luna.globals$date))
}

.onLoad <- function(libname, pkgname) {
  #library.dynam("luna", package = "luna", lib.loc = NULL)
  luna.globals$logmode <- 0
  luna.globals$xy <- ldefault.xy()
  luna.globals$xy.coh <- ldefault.coh.xy(luna.globals$xy)

  luna.globals$rbpal <- grDevices::colorRampPalette(c("navy", "blue", "white", "red", "darkred"))

  requireNamespace("plotrix", quietly = T, warn.conflicts = F)
  requireNamespace("geosphere", quietly = T, warn.conflicts = F)
  requireNamespace("shiny", quietly = T, warn.conflicts = F)
  requireNamespace("DT", quietly = T, warn.conflicts = F)
  requireNamespace("shinyFiles", quietly = T, warn.conflicts = F)
  requireNamespace("xtable", quietly = T, warn.conflicts = F)
  requireNamespace("shinydashboard", quietly = T, warn.conflicts = F)

}

.onUnload <- function(libpath) {
  library.dynam.unload("luna", libpath)
}



####################################################
##                                                ##
## Work with sample lists                         ##
##                                                ##
####################################################


#' Loads a sample list
#'
#' Imports a Luna \href{https://zzz.bwh.harvard.edu/luna/luna/args/#sample-lists}{sample-list}
#' into R
#'
#' @param file a required argument, giving the name of the sample-list file
#' @param path an optional argument, mirroring Luna's
#'   \href{https://zzz.bwh.harvard.edu/luna/luna/args/#search-paths}{path} command-line option
#'
#'
#' @return a named-list representing the sample-list
#' @export
#'
#' @examples
#' \dontrun{
#' > sl <- lsl("s.lst")
#' 3 observations in s.lst
#'
#' > names(sl)
#' [1] "nsrr01" "nsrr02" "nsrr03"
#'
#' > str(sl)
#' List of 3
#' $ nsrr01:List of 2
#' ..$ EDF  : chr "edfs/learn-nsrr01.edf"
#' ..$ ANNOT: chr "edfs/learn-nsrr01-profusion.xml"
#' $ nsrr02:List of 2
#' ..$ EDF  : chr "edfs/learn-nsrr02.edf"
#' ..$ ANNOT: chr "edfs/learn-nsrr02-profusion.xml"
#' $ nsrr03:List of 2
#' ..$ EDF  : chr "edfs/learn-nsrr03.edf"
#' ..$ ANNOT: chr "edfs/learn-nsrr03-profusion.xml"
#' }
#' @importFrom utils read.table
lsl <- function(file, path = "") {
  d <- read.table(file, header = F, fill = T, sep = "\t", stringsAsFactors = F)
  if (dim(d)[2] < 2) stop("invalid sample list")
  cat(dim(d)[1], "observations in", file, "\n")
  if (dim(d)[1] > length(unique(d[, 1]))) stop("duplicate IDs found")
  l <- list()
  for (i in 1:dim(d)[1]) {
    l[[d[i, 1]]]$EDF <- ifelse(path == "", d[i, 2], paste(path, d[i, 2], sep = "/"))
    a <- d[i, -c(1:2)]
    a <- a[a != ""]
    a <- a[a != "."]
    if (length(a) != 0) {
      a <- unlist(strsplit(a, ","))
      if (path != "") a <- sapply(a, function(x) paste(path, x, sep = "/"))
      l[[d[i, 1]]]$ANNOT <- as.character(a)
    } else {
      l[[d[i, 1]]]$ANNOT <- character(0)
    }
  }
  l
}

#' Attaches an EDF from a sample list
#'
#' Loads an EDF and any associated annotation files from a
#' \href{https://zzz.bwh.harvard.edu/luna/luna/args/#sample-lists}{sample-list}
#' loaded by \code{\link{lsl}}.
#'
#' @param sl a sample-list as loaded by \code{\link{lsl}}
#' @param idx either an integer number (in which case, it means to attach the
#' EDF/annotations specified on that row of the sample-list), or a string value
#' (in which case, it is interpreted as the ID of the individual/EDF to be attached)
#'
#' @return No explicit return value: this command sets the in-memory EDF
#' representation to reflect this EDF header/file, by calling \code{\link{ledf}}
#' @export
#'
#' @examples
#' \dontrun{
#' > lattach(sl, 2)
#' nsrr02 : 14 signals, 10 annotations, of 09:57:30 duration
#'
#' > lattach(sl, "nsrr02")
#' nsrr02 : 14 signals, 10 annotations, of 09:57:30 duration
#' }
#' @note As well as attaching that EDF, this calls \code{\link{lstat}} to display some
#' basic information about it. If the index or ID is out-of-range/not found, this
#' command will give an error
#'
#' Unlike many R functions, \code{lattach()} does not return an object that represents the
#' data (i.e. the EDF). Rather, \code{lunaR} is designed to operate on one EDF at a time;
#' attached EDFs can be displayed with the \code{\link{lstat}} function. Attaching a new EDF
#' effectively detaches any previously attached EDF.
lattach <- function(sl, idx = "") {
  id <- idx
  if (is.numeric(idx)) {
    if (idx < 1 | idx > length(sl)) stop(paste("idx out of range, expecting 1 ..", length(sl)))
    id <- names(sl)[idx]
  } else {
    idx <- which(names(sl) == id)
    if (length(idx) != 1) stop(paste("could not find index", idx))
  }
  ledf(sl[[idx]]$EDF, id, sl[[idx]]$ANNOT)
}





#' Setting variables
#'
#' Sets a (special) variable, similar to lunaC command-line options
#'
#' @param var a required parameter that is either a
#' \href{https://zzz.bwh.harvard.edu/luna/luna/args/#variables}{variable} name
#' (if \code{val} is non-\code{NULL}), or a list of variable/value pairs, or a
#' filename of a \href{https://zzz.bwh.harvard.edu/luna/luna/args/#parameter-files}{parameter file}
#' @param val if \code{val} is non-\code{NULL}, then \code{var} is interpreted
#' as the variable name, which is set to the value of \code{val}; if \code{val}
#' equals "\code{.}" this erases the variable \code{var}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## these are equivalent
#' ## % luna s.lst stage=N2 excl=arousal_standard sig=C3,C4 < cmd.txt
#'
#' > lset("stage", "N2")
#' > lset("excl", "arousal_standard")
#' > lset("sig", "C3, C4")
#' > k <- leval(lcmd("cmd.txt"))
#'
#' > lset(list(stage="N2", excl="arousal_standard", sig="C3, C4"))
#'
#' ## if these variables are specified in a file
#' > lset("files/param.txt")#'
#' }
#'
#' @note The special variables \code{sig} and \code{alias} operate slightly differently with \code{lset()},
#' in that new values are appended to the list, rather than over-writing the existing value.
#'
#' \preformatted{
#' > lset("sig", "C3")
#' > lset("sig", "C4")
#' }
#' is the same as
#' \preformatted{
#' > lset("sig", "C3, C4")
#' }
#'
#' To reset any variable (including \code{sig} and \code{alias})
#' \preformatted{
#' > lset("sig", ".")
#' }
#'
#' the \code{\link{lreset}()} function clears all variables, and resets all
#' special variables to their default values.
#'
#' @importFrom utils read.table
lset <- function(var, val = NULL) {
  if (!is.null(val)) {
    .Call("Rset_var", as.character(var), as.character(val), PACKAGE = "luna")
  } else if (is.list(var)) {
    v <- unlist(var)
    for (i in 1:length(v)) .Call("Rset_var", as.character(names(v)[i]), as.character(v[i]), PACKAGE = "luna")
  } else { # assume this is a file
    d <- read.table(var, sep = "\t", header = F, stringsAsFactors = F)
    if (dim(d)[2] != 2) stop(paste("expecting two tab-delimited columns in", v))
    n <- dim(d)[1]
    for (i in 1:n) .Call("Rset_var", as.character(d[i, 1]), as.character(d[i, 2]), PACKAGE = "luna")
  }
  invisible(1)
}

#' Getting variables
#'
#' Returns the value of a user-defined variable
#'
#' @param v a required parameter, the variable name
#'
#' @return the variable value, or \code{NULL} if the variable is not defined, or
#' if it is a special variable (e.g. \code{sig} or \code{alias})
#' @export
#'
#' @examples
#' \dontrun{
#' > lvar("stage")
#' [1] "N2"
#' }
#'
#' @note The \code{lvar()} function only applies to user-defined variables such as \code{${stage}}.
#' Special variables (e.g. \code{sig, annots}, etc) will currently return \code{NULL} here.
#' \preformatted{
#' > lvar("sig")
#' NULL
#' }
#' This doesn't mean they are not defined. Whilst not a bug exactly, this "feature"
#' is not particularly convenient; future releases of *lunaR* will be fixed to return
#' the actual values of all variables.
lvar <- function(v) {
  .Call("Rshow_var", as.character(v), PACKAGE = "luna")
}

lclear <- function(v) {
  .Call("Rset_var", as.character(v), NULL, PACKAGE = "luna")
  invisible(1)
}

#' Clears all previously lset() variables
#'
#' @export
#'
#' @note This function clears all variables (both user-defined and special variables).
#' It does not effect any currently attached EDF however. (In contrast, \code{\link{ldrop}()}
#' or \code{\link{lrefresh}()} will alter the currently-attached EDF, but do not
#' effect any variable definitions.)
lreset <- function() {
  ldrop()
  .Call("Rclear_vars", PACKAGE = "luna")
  invisible(1)
}


#' Indicate whether lunaR is already initated in this session
#'
#' @export
#'
lmoonlock <- function( s ) {
  .Call("Rmoonlock", as.character(s), PACKAGE = "luna")
}




####################################################
##                                                ##
## Attach an EDF                                  ##
##                                                ##
####################################################


#' Attaches an EDF directly
#'
#' Directly attaches an EDF
#'
#' @param edf a required filename for the to-be-attached EDF
#' @param id an optional ID that will be associated with this EDF
#' @param annots is an optional vector of one or more annotation filenames
#' (\code{.xml, .ftr, .annot} or \code{.eannot} files, as described
#' \href{https://zzz.bwh.harvard.edu/luna/ref/annotations/}{here})
#'
#' @return No explicit return value. Similar to \code{\link{lattach}}, which
#' is just a wrapper around the \code{\link{ledf}} function
#' @export
#'
#' @examples
#' \dontrun{
#' > ledf("edfs/learn-nsrr02.edf", "nsrr02", "edfs/learn-nsrr02-profusion.xml")
#' nsrr02 : 14 signals, 10 annotations, of 09:57:30 duration
#' }
ledf <- function(edf, id = ".", annots = character(0)) {
  # EDF, ID, annotations
  .Call("Rattach_edf", as.character(edf), as.character(id), as.character(annots), PACKAGE = "luna")
  lflush()
  lstat()
  luna.globals$edf <- as.character(edf)
  luna.globals$id <- as.character(id)
  luna.globals$annots <- as.character(annots)
  invisible(1)
}


#' Generates an empty EDF
#'
#' @param id an optional ID that will be associated with this EDF
#' @param rs an optional EDF record size (defaults to 1 second)
#' @param nr an optional number of records (defaults to 86400, i.e. 24 hours if rs is 1)
#' @param date an optional EDF start date format dd.mm.yy (default 01.01.85)
#' @param start an optional EDF start time format hh:mm:ss (default 00:00:00)
#'
#' @return No explicit return value
#' @export
#'
#' @examples
#' \dontrun{
#' > lempty.edf()
#' . : 0 signals, 0 annotations, of 24:00:00 duration
#' }
lempty.edf <- function( id = "." , rs = 1 , nr = 86400 , date = "01.01.85" , start = "00:00:00" )
{
  .Call("Rempty_edf", as.character(id), as.integer(rs), as.integer(nr), as.character(date), as.character(start), PACKAGE = "luna")
  lflush()
  lstat()
  luna.globals$edf <- ""
  luna.globals$id <- as.character(id)
  luna.globals$annots <- ""
  invisible(1)
}



#' Reports on the in-memory EDF
#'
#' One line description to console of the currently-attached EDF
#'
#' @return no explicit return values, other than output to console
#' @export
#'
#' @note This command is automatically called after each \code{\link{leval}} or
#' \code{\link{lattach}} command.
lstat <- function() {
  invisible(.Call("Rstat", PACKAGE = "luna"))
}

#' Describes the contents of an attach EDF
#'
#' @return list with EDF header information, including channel labels & sample rates
#' @export
#'
#' @note This command does not currently report on annotations
ldesc <- function() {
  .Call("Rdesc", PACKAGE = "luna")
}

#' Turns Luna's typical console messages on/off
#'
#' Turns on or off Luna's console message
#'
#' @param x either \code{1} or \code{0} to turn console messaging on or off, respectively
#'
#' @export
#'
#' @note By default, lunaR evaluates Luna commands more or less "silently". By
#' running \code{llog(1)}, you'll get more output, which can be useful to diagnose
#' errors, or check the progress of long-running jobs.
llog <- function(x) {
  if (length(x) != 1) stop("expecting a single 0/1")
  if (!is.numeric(x)) stop("expecting a single 0/1")
  luna.globals$logmode <- as.logical(x)
  .Call("Rlogmode", as.integer(x), PACKAGE = "luna")
  invisible(1)
}

lflush <- function() {
  if (luna.globals$logmode) .Call("Rflush_log", PACKAGE = "luna")
  invisible(luna.globals$logmode)
}

#' Epochs the data
#'
#' Sets epochs for the current EDF
#'
#' @param dur the epoch duration in seconds
#' @param inc the epoch increment, defaults to \code{dur} (i.e. non-overlapping epochs)
#'
#' @return the number of epochs set
#' @export
#'
#' @examples
#' \dontrun{
#' > ne <- lepoch()
#' evaluating...
#' nsrr02 : 14 signals, 10 annotations, of 09:57:30 duration, 1195 unmasked 30-sec epochs, and 0 masked
#' > ne
#' [1] 1195
#' }
#' @note \code{lepoch(30)} is identical to using the \code{\link{leval}()}
#' function to evaluate/execute an EPOCH command:
#' \preformatted{
#' leval("EPOCH dur=30")
#' }
lepoch <- function(dur = 30, inc = -1) {
  if (inc <= 0) inc <- dur
  k <- leval(paste("EPOCH", paste("dur", dur, sep = "="), paste("inc", inc, sep = "=")))
  invisible(k$EPOCH$BL$NE)
}

#' Returns an epoch-table with information about epochs, masks and annotations
#'
#' Returns an epoch-level data frame with time, mask and annotation information
#'
#' @param annots an optional vector of annotation class names, as returned by \code{\link{lannots}()}
#'
#' @return a data frame with at least six columns:
#' * \code{E} epoch number of the current unmasked dataset
#' * \code{SEC} elapsed seconds for the current unmasked dataset
#' * \code{E1} epoch number for the original dataset
#' * \code{SEC1} elapsed seconds based on the original data
#' * \code{HMS} clock-time for epoch start
#' * \code{M} flag to indicate whether this epoch is
#' \href{https://zzz.bwh.harvard.edu/luna/ref/masks/}{masked} \code{(1)} or not \code{(0)}
#' * any additional columns will be labeled based on the specified annotations,
#' with a flag (\code{1} or \code{0}) to indicate whether or not that epoch contains at least
#' one of that annotation class
#' @export
#'
#' @examples
#' \dontrun{
#' > d <- letable( annots=c("wake" , "NREM1" , "NREM2" ) )
#' > dim(d)
#' [1] 1195    9
#'
#' > head(d)
#'   E SEC E1 SEC1      HMS M NREM1 NREM2 wake
#' 1 1   0  1    0 21:18:06 0     0     0    1
#' 2 2  30  2   30 21:18:36 0     0     0    1
#' 3 3  60  3   60 21:19:06 0     0     0    1
#' 4 4  90  4   90 21:19:36 0     0     0    1
#' 5 5 120  5  120 21:20:06 0     0     0    1
#' 6 6 150  6  150 21:20:36 0     0     0    1
#'
#' ## using leval() to set a mask, for example, to mask everything except epochs 2 to 5
#' > leval("MASK epoch=2-5")
#' evaluating...
#' nsrr02 : 14 signals, 10 annotations, of 09:57:30 duration, 4 unmasked 30-sec epochs, and 1191 masked
#'
#' ## the M column indicates which epochs are masked
#' > head(letable())
#'    E SEC E0 SEC0      HMS M
#' 1 NA  NA  1    0 21:18:06 1
#' 2  1   0  2   30 21:18:36 0
#' 3  2  30  3   60 21:19:06 0
#' 4  3  60  4   90 21:19:36 0
#' 5  4  90  5  120 21:20:06 0
#' 6 NA  NA  6  150 21:20:36 1
#'
#' ## restructure the dataset, i.e. to actually remove masked epochs
#' > leval("RESTRUCTURE")
#' evaluating...
#' nsrr02 : 15 signals, 10 annotations, of 00:02:00 duration, 4 unmasked 30-sec epochs, and 0 masked
#'
#' ## now the entire in-memory dataset is only four epochs:
#' > letable()
#'   E SEC E0 SEC0      HMS M
#' 1 1   0  2   30 21:18:36 0
#' 2 2  30  3   60 21:19:06 0
#' 3 3  60  4   90 21:19:36 0
#' 4 4  90  5  120 21:20:06 0
#'
#' ## reset the epochs
#' > lepoch()
#' evaluating...
#' nsrr02 : 14 signals, 10 annotations, of 09:57:30 duration, 1195 unmasked 30-sec epochs, and 0 masked
#'
#' > head(letable())
#'   E SEC E1 SEC1      HMS M NREM1 NREM2 wake
#' 1 1   0  1    0 21:18:06 0     0     0    1
#' 2 2  30  2   30 21:18:36 0     0     0    1
#' 3 3  60  3   60 21:19:06 0     0     0    1
#' 4 4  90  4   90 21:19:36 0     0     0    1
#' 5 5 120  5  120 21:20:06 0     0     0    1
#' 6 6 150  6  150 21:20:36 0     0     0    1
#'
#' }
#' @seealso \href{https://zzz.bwh.harvard.edu/luna/ref/masks/#restructure}{RESTRUCTURE}
letable <- function(annots = character(0)) {
  .Call("Rmask", as.character(annots), PACKAGE = "luna")
}

#' Attaches an new annotation from an file
#'
#' Adds new annotations to the currently attached EDF from a file
#'
#' @param a a required annotation file name
#'
#' @return no explicit return value
#' @export
#'
#' @note This command is similar to specifying an annotation file in a sample-list.
#' Epoch annotation (.eannot) files must match the attached EDF in terms of the number of current epochs.
ladd.annot.file <- function(a) {
  if (!file.exists(a)) stop(paste("cannot find", a))
  .Call("Radd_annot", as.character(a), PACKAGE = "luna")
  luna.globals$annots <- c(luna.globals$annots, as.character(a))
  invisible(1)
}

#' Attaches an new annotation from an R interval list
#'
#' Adds new annotations to the currently attached EDF given an interval list
#'
#' @param annot a required annotation class name for the new annotation
#' @param intervals a required list of intervals, in the same format as returned
#' by \code{\link{lannots}(annot)}
#'
#' @return no explicit return value
#' @export
#'
#' @note Added annotations will be visible with subsequent \code{\link{lannots}()} commands.
ladd.annot <- function(annot, intervals) {
  if (!is.list(intervals)) stop("expecting a list of intervals")
  # check that each have two of each, convert to vector
  t <- unlist(intervals)
  if (length(t) != 2 * length(intervals)) stop("bad interval list format: expecting two items per list element")
  if (length(t) != 0) .Call("Radd_annot_fromR", as.character(annot), as.numeric(t), PACKAGE = "luna")
  lstat()
  invisible(1)
}

#' Converts epochs to intervals
#'
#' Returns an interval list corresponding to epochs
#'
#' @param e a required parameter, a vector of 1-based epochs
#' @param dur (optional) sets the epoch length to be used (default 30 seconds)
#' @param inc (optional) set the epoch increment to be used (default 30 seconds)
#'
#' @return an interval list, in the format used by \code{\link{lannots}(annot)} and
#' \code{\link{ldata.intervals}()}, for example
#' @export
#'
#' @examples
#' le2i(1:4)
le2i <- function(e, dur = 30, inc = 30) {
  mapply(c, (e - 1) * inc, (e - 1) * inc + dur, SIMPLIFY = F)
}

#' Returns a vector of annotation class names for the attached EDF
#'
#' Returns a vector of annotation class names, or all intervals for a given annotation class
#'
#' @param a an optional annotation name, i.e. matching one value returned by \code{lannots()}
#'
#' @return If no specific annotation is specified, this returns a vector of
#' annotation class names for the attached dataset. Alternatively, if \code{a} is a valid
#' annotation class name for that individual, this function instead returns a list
#' of intervals (two-element vectors) for each instance of that annotation class.
#' @export
#'
#' @examples
#' \dontrun{
#' ## no specific annotation
#' > lannots()
#'[1] "NREM1"             "NREM2"             "NREM3"
#'[4] "REM"               "apnea_obstructive" "arousal"
#'[7] "artifact_SpO2"     "desat"             "hypopnea"
#'[10] "wake"
#'
#' ## valid annotation class name specified
#' > head(lannots("wake"))
#' [[1]]
#' [1]  0 30

#' [[2]]
#' [1] 30 60

#' [[3]]
#' [1] 60 90

#' [[4]]
#' [1]  90 120

#' [[5]]
#' [1] 120 150

#' [[6]]
#' [1] 150 180
#'
#' }
lannots <- function(a = "") {
  if (a == "") {
    return(.Call("Rannots", PACKAGE = "luna"))
  }
  if (length(a) != 1) stop("lannots( annot ) only takes one annotation class")
  .Call("Rannot", as.character(a), PACKAGE = "luna")
}

#' Returns the EDF channel names
#'
#' Returns a vector of channel names for the attached EDF
#'
#' @return a vector of channel names for the attached EDF
#' @export
#'
#' @examples
#' \dontrun{
#' > lchs()
#' [1] "SaO2"     "PR"       "EEG(sec)" "ECG"      "EMG"      "EOG(L)"
#' [7] "EOG(R)"   "EEG"      "AIRFLOW"  "THOR RES" "ABDO RES" "POSITION"
#' [13] "LIGHT"    "OX STAT"
#' }
lchs <- function() {
  .Call("Rchannels", PACKAGE = "luna")
}

#' Detaches the current EDF
#'
#' @return no explicit return value
#' @export
#'
ldrop <- function() {
  .Call("Rdrop", PACKAGE = "luna")
  invisible(1)
}

#' Set error handler to moonlight mode
#'
#' @return no explicit return value
#' @export
#'
lmoonlight_mode <- function() {
  .Call("R_moonlight_mode", PACKAGE = "luna")
  invisible(1)
}

#' Reverts to the original attached EDF
#'
#' Reattaches the currently attached EDF
#'
#' @return no explicit return value
#' @export
#'
#' @note When \code{lrefresh()}-ing an attached EDF, any previous in-memory modifications
#' (i.e. from masking, filtering, or other manipulations) are effectively reset.
lrefresh <- function() {
  if (luna.globals$edf == "") stop("no EDF yet attached")
  lprob_clear() # clears any 'problem' flag set
  ledf(luna.globals$edf, luna.globals$id, luna.globals$annots)
}


####################################################
##                                                ##
## Evaluate a (set of) command(s)                 ##
##                                                ##
####################################################

#' Evaluates an arbitrary set of Luna commands
#'
#' Evaluates a set of Luna commands for the current attached dataset
#'
#' @param command a single string or character vector of
#' \href{https://zzz.bwh.harvard.edu/luna/ref/}{Luna commands},
#' or the return value of \code{\link{lcmd}()}
#'
#' @return a list of data-frames, in which list items are commands and sub-items
#' are output strata, i.e. a similar organization to
#' \href{https://zzz.bwh.harvard.edu/luna/luna/args/#destrat}{destrat} output
#' \href{https://zzz.bwh.harvard.edu/luna/luna/args/#destrat}{(\code{lout})} databases.
#' The \code{\link{lx}()} function is designed to facilitate working with these lists.
#' @export
#'
#' @examples
#' \dontrun{
#' ## estimate the PSD for all N2 epochs (for tutorial individual nsrr02)
#' ## this performs exactly the same set of operations as the following command-line lunaC statement
#' ## % luna s.lst 2 -o out.db -s "EPOCH & MASK ifnot=NREM2 & RE & PSD sig=EEG epoch spectrum"
#' > k <- leval( "EPOCH & MASK ifnot=NREM2 & RE & PSD epoch spectrum sig=EEG" )
#'
#' > str(k)
#' List of 4
#' $ EPOCH:List of 1
#'  ..$ BL:'data.frame':  1 obs. of  3 variables:
#'  .. ..$ DUR: num 30
#'  .. ..$ INC: num 30
#'  .. ..$ NE : num 399
#' $ MASK :List of 1
#'  ..$ EPOCH_MASK:'data.frame':  1 obs. of  7 variables:
#'  .. ..$ EPOCH_MASK  : chr "NREM2"
#'  .. ..$ N_MASK_SET  : num 0
#'  .. ..$ N_MASK_UNSET: num 0
#'  .. ..$ N_MATCHES   : num 399
#'  .. ..$ N_RETAINED  : num 399
#'  .. ..$ N_TOTAL     : num 399
#'  .. ..$ N_UNCHANGED : num 399
#' $ PSD  :List of 4
#'  ..$ CH    :'data.frame':  1 obs. of  2 variables:
#'  .. ..$ CH: chr "EEG"
#'  .. ..$ NE: num 399
#'  ..$ B_CH  :'data.frame':  10 obs. of  4 variables:
#'  .. ..$ B     : chr [1:10] "ALPHA" "BETA" "DELTA" "FAST_SIGMA" ...
#'  .. ..$ CH    : chr [1:10] "EEG" "EEG" "EEG" "EEG" ...
#'  .. ..$ PSD   : num [1:10] 15.65 5.12 108.24 3.08 5.4 ...
#'  .. ..$ RELPSD: num [1:10] 0.0709 0.0232 0.4903 0.0139 0.0245 ...
#'  ..$ CH_F  :'data.frame':  41 obs. of  3 variables:
#'  .. ..$ CH : chr [1:41] "EEG" "EEG" "EEG" "EEG" ...
#'  .. ..$ F  : num [1:41] 0 0.25 0.75 1.25 1.75 2.25 2.75 3.25 3.75 4.25 ...
#'  .. ..$ PSD: num [1:41] 15.2 77.7 70.6 45.9 37.4 ...
#'  ..$ B_CH_E:'data.frame':    3990 obs. of  5 variables:
#'  .. ..$ B     : chr [1:3990] "ALPHA" "ALPHA" "ALPHA" "ALPHA" ...
#'  .. ..$ CH    : chr [1:3990] "EEG" "EEG" "EEG" "EEG" ...
#'  .. ..$ E     : int [1:3990] 92 93 98 99 100 101 102 118 119 120 ...
#'  .. ..$ PSD   : num [1:3990] 29.5 37.5 33.5 22.7 17.8 ...
#'  .. ..$ RELPSD: num [1:3990] 0.159 0.131 0.12 0.141 0.1 ...
#'  $ RE   :List of 1
#'  ..$ BL:'data.frame':  1 obs. of  4 variables:
#'  .. ..$ DUR1: num 11970
#'  .. ..$ DUR2: num 11970
#'  .. ..$ NR1 : num 11970
#'  .. ..$ NR2 : num 11970
#' }
#' @note ach time lunaC (the command-line version of Luna) is run, it is applied
#' to a "fresh" version of the data, i.e. the EDF on disk. In contrast, \code{leval()}
#' statements within the same R session will have cumulative effects on the
#' internal EDF, which will persist until either \code{\link{lrefresh}()} is
#' called to re-attach a "fresh" version of the data, or it is dropped
#' (with \code{\link{ldrop}()}), or a different EDF is attached
#' (with \code{\link{lattach}()} or \code{\link{ledf}()}).
leval <- function(command) {
  xx <- paste0(command, collapse = " & ")
  retval <- .Call("Reval_cmd", as.character(xx), PACKAGE = "luna")
  lflush()
  lstat()
  invisible(retval)
}

lreturnless_eval <- function(x) {
  # we apply this to whomever is attached
  xx <- paste0(x, collapse = " & ")
  .Call("Reval_cmd_noreturns", as.character(xx), PACKAGE = "luna")
  invisible(1)
}

#' Evaluates an arbitrary set of Luna commands for all members of a project
#'
#' Evaluates a Luna commands for all individuals in a sample-list
#'
#' @param sl a required sample-list, as returned by \code{\link{lsl}()}
#' @param command a single string or character vector of
#' \href{https://zzz.bwh.harvard.edu/luna/ref/}{Luna commands},
#' or the return value of \code{\link{lcmd}()}
#'
#' @return the same list-of-data-frames as returned by \code{\link{leval}()},
#' except each data-frame will now contain all individuals in the project
#' specified by sample list \code{sl}.
#' @export
#'
#' @examples
#' \dontrun{
#' > k <- leval.project(lsl("s.lst") , lcmd("cmd.txt"))
#' ## this does the same as the lunaC command
#' ## % luna s.lst -o out.db < cmd.txt
#' }
#'
#' @note Unlike \code{\link{leval}()}, the \code{leval.project()} function attaches
#'  and then drops each EDF in the sample list, iteratively. This means that any
#'  changes to the internal EDFs will not persist across different runs of
#'  \code{leval.project()} in the way they do for \code{\link{leval}()}.
leval.project <- function(sl, command) {
  if (missing(sl)) stop("no sample list 'sl' specified")
  if (missing(command)) stop("no Luna commands specified")
  ids <- names(sl)
  if (length(ids) == 0) stop("no individuals in sample-list")
  .Call("Reval_init_returns", PACKAGE = "luna")
  for (id in ids) {
    lattach(sl, id)
    lreturnless_eval(command)
  }
  ldrop()
  .Call("Reval_get_returns", PACKAGE = "luna")
}

lprob <- function() {
  .Call("Rproblem", PACKAGE = "luna")
}

lprob_clear <- function(state = 0) {
  .Call("Rsetproblem", as.integer(state), PACKAGE = "luna")
}


####################################################
##                                                ##
## Attach a destrat database                      ##
##                                                ##
####################################################

#' Imports data from a lout database as an R list object
#'
#' Imports the contents of a lout database
#'
#' @param dbfile the name of a \href{https://zzz.bwh.harvard.edu/luna/luna/args/#destrat}{lout} database
#' @param ids (optional) if \code{ids} is a vector of individual/EDF IDs, then only
#' these individuals will be extracted from the database
#'
#' @return a named-list R object, in the same format as \code{\link{leval}()}
#' @export
#'
#' @examples
#' \dontrun{
#' > k <- ldb("out.db")
#' read data on 3 individuals
#'
#' > k
#' $HEADERS
#' $HEADERS$BL
#'       ID    NR NS REC.DUR TOT.DUR.HMS TOT.DUR.SEC
#' 1 nsrr01 40920 14       1    11:22:00       40920
#' 2 nsrr02 35850 14       1    09:57:30       35850
#' 3 nsrr03 40920 14       1    11:22:00       40920
#'
#' $HEADERS$CH
#'        ID       CH  DMAX   DMIN PDIM   PMAX    PMIN  SR
#' 1  nsrr01 ABDO RES   127   -128       -1.00    1.00  10
#' 2  nsrr02 ABDO RES   127   -128       -1.00    1.00  10
#' 3  nsrr03 ABDO RES   127   -128       -1.00    1.00  10
#' 4  nsrr01  AIRFLOW   127   -128       -1.00    1.00  10
#' 5  nsrr02  AIRFLOW   127   -128       -1.00    1.00  10
#' 6  nsrr03  AIRFLOW   127   -128       -1.00    1.00  10
#' 7  nsrr01      ECG   127   -128   mV   1.25   -1.25 250
#' 8  nsrr02      ECG   127   -128   mV   1.25   -1.25 250
#' 9  nsrr03      ECG   127   -128   mV   1.25   -1.25 250
#' 10 nsrr01      EEG   127   -128   uV 125.00 -125.00 125
#' 11 nsrr02      EEG   127   -128   uV 125.00 -125.00 125
#' 12 nsrr03      EEG   127   -128   uV 125.00 -125.00 125
#' ...(continued)...
#' }
#'
#' @note As noted, the returned value is in the same format as is generated by
#' \code{\link{leval}()} and can be parsed with \code{\link{lx}()} and \code{\link{lid}()}.
ldb <- function(dbfile, ids = character(0)) {
  .Call("Rdb2retval", as.character(dbfile), as.character(ids), PACKAGE = "luna")
}


####################################################
##                                                ##
## Load a text-table                              ##
##                                                ##
####################################################

#' Loads and conconcatenates multiple text-table output files
#'
#' Loads and concatenates text-table format *Luna* (\code{-t} mode) output
#'
#' @param root a required parameter, the folder containing the output
#' @param f (optional) the name of the file to load from each subfolder of \code{root}
#' @param ids (optional) to specify that only a subset of individuals are loaded
#' (subfolder names are expected to correspond to IDs)
#' @param silent (optional) if set to true makes this function runs silently
#' (not output to console)
#'
#' @return a \code{data.frame} containing row-concatenated data from the subfolders of \code{root}
#' @export
#'
#' @examples
#' \dontrun{
#' ## this luna command will make a out1 subfolder
#' ## % luna s.lst -t out1 -s 'MASK ifnot=NREM2 & RE & PSD spectrum sig=EEG'
#'
#' ## % ls out1/
#' ##  nsrr01  nsrr02  nsrr03
#'
#' ## each subfolder will contain iodentical file names.
#' ## % ls out1/nsrr01
#' ##  MASK-EPOCH_MASK.txt  PSD-B,CH.txt  PSD-CH.txt  PSD-F,CH.txt  RE.txt
#'
#' ## get an enumeration of all files
#' > ltxttab("out1")
#' MASK-EPOCH_MASK.txt        PSD-B,CH.txt          PSD-CH.txt        PSD-F,CH.txt
#'                   3                   3                   3                   3
#' RE.txt
#'      3
#'
#' ## to load all PSD-B,CH.txt files:
#' > d <- ltxttab("out1", "PSD-B,CH.txt")
#' > table(d$ID)
#' nsrr01 nsrr02 nsrr03
#'     10     10     10
#' }
#'
#' @importFrom utils read.table
ltxttab <- function(root, f = "", ids = dir(root), silent = F) {
  if (f == "") {
    return(ltxttab.dir(root, ids))
  }

  # root : folder root
  files <- paste(root, "/", ids, "/", f, sep = "")

  cnt <- 1
  for (file in files) {
    if (!silent) cat("reading", file, "\n")
    if (cnt == 1) {
      d <- read.table(file, header = T, stringsAsFactors = F)
    } else {
      d <- rbind(d, read.table(file, header = T, stringsAsFactors = F))
    }
    cnt <- cnt + 1
  }
  d
}

ltxttab.dir <- function(root, ids = dir(root)) {
  folders <- paste(root, "/", ids, "/", sep = "")
  r <- character(0)
  for (folder in folders) r <- c(r, dir(folder))
  table(r)
}


####################################################
##                                                ##
## Iterate epoch-wise or annot-wise, applying     ##
## user-defined function                          ##
##                                                ##
####################################################

#' Applies an arbitrary R function to signal data, one epoch or interval at a time
#'
#' Applies an arbitrary R function to each epoch
#'
#' @param func a required argument, which specifies a user-defined R function to
#' be evaluated per-epoch (or per interval)
#' @param chs (optional) a vector of channel names to be extracted each epoch (by default, all are)
#' @param annots (optional) a vector of annotation class names to be extracted each epoch (by default, all are)
#' @param by.annot (optional) a single annotation class name, in which case this
#' function iterates over each instance of that annotation class, rather than each epoch
#' @param w (optional) a window (in seconds) added to each annotation instance,
#' if \code{by.annot} has been specified
#' @param env (optional) R \href{https://adv-r.hadley.nz/environments.html}{environment},
#' by default, \code{\link{new.env}()},
#'
#' @return no explicit return value: it is expected that the user will craft the
#' function \code{func} in a way to capture relevant information
#' @export
#'
#' @examples
#' \dontrun{
#' ## a toy function to calculate root mean square
#' > rms <- function(x) {sqrt(mean(x^2))}
#'
#' ## create a new environment to store the output
#' > e <- new.env()
#' > e$ret <- numeric()
#'
#' ## a wrapper function
#' > f1 <- function(x) {epoch = x$E[1]; e$ret[epoch] <- rms(x$EEG - mean(x$EEG))}
#' ## The f1() function takes the epoch number as the first element in the E field,
#' ## and populates that entry of e$ret with the RMS.
#'
#' ## make sure the attached EDF is already epoched
#' ## if not, run lepoch() first
#'
#' > literate(f1, chs="EEG", annots="")
#' ........................................ 40 epochs
#' ........................................ 80 epochs
#' ........................................ 120 epochs
#' ........................................ 160 epochs
#' ........................................ 200 epochs
#' ........................................ 240 epochs
#' ........................................ 280 epochs
#' ........................................ 320 epochs
#' ...... 1195 epochs, done
#'
#' ## a new vector will pop up in the environment
#' > ls(e)
#' [1] "ret"
#'
#' > head(e$ret)
#' [1] 11.97071 15.87002 12.46824 11.34265 12.28683 15.23002#'
#' }
#'
#' @details The \code{literate()} function aims to make *lunaR* somewhat extensible, by
#' allowing users to add their own functions, in a way that can take advantage
#' of the masking, filtering and intersection with annotation data afforded by
#' the core Luna engine. This function iterates either one epoch or one interval
#' (based on annotation instances) at a time. If not based on annotation intervals
#' (with \code{by.annot}), you therefore need to set epochs (with with \code{\link{lepoch}()})
#' prior to running \code{literate()}.
#'
#' For each epoch, lunaR creates a data-frame in the same format as returned by
#' the \code{\link{ldata}()} and \code{\link{ldata.intervals}()} commands, and
#' passes it to the user-defined function specified in \code{literate()}. The
#' user's function must therefore
#' 1. know what input to expect,
#' 2. appropriately perform any calculations and
#' 3. store results as desired.
#'
literate <- function(func, chs = character(0), annots = character(0),
                     by.annot = character(0), w = 0, env = new.env()) {
  tmp <- .Call("Riterate", as.function(func), as.character(chs),
    as.character(annots), as.character(by.annot), as.numeric(w),
    env,
    PACKAGE = "luna"
  )
  invisible(tmp)
}


####################################################
##                                                ##
## Pull raw signals/annotations                   ##
##                                                ##
####################################################


#' Extracts signal (and annotation) data from an EDF for one or more epochs
#'
#' Returns a data frame of signals and annotations for requested epochs
#'
#' @param e a required vector of 1-based epoch numbers corresponding to the
#' current epoch numbering of the attached EDF
#' @param chs a required vector of channel names to be returned, i.e. as from \code{\link{lchs}()},
#' all of which are required to have the same sampling rate
#' @param annots an optional vector of annotation names to be returned, i.e. as from \code{link{lannots}()}
#'
#' @return a data frame where each row is one sample point, containing the raw
#' signal (and annotation) data
#' @export
#'
#' @examples
#' \dontrun{
#' ## request EEG and EMG channels, for epochs 211 and 212, along with the
#' ## arousal annotation
#' > d <- ldata( 211:212 , chs = c("EEG", "EMG") , annots = "arousal" )
#' > head(d)
#'     E      SEC       EEG        EMG arousal
#' 1 211 6300.000 -4.411765 -2.8411765       0
#' 2 211 6300.008 -5.392157  0.8647059       0
#' 3 211 6300.016 -4.411765 -1.6058824       0
#' 4 211 6300.024 -4.411765 -2.8411765       0
#' 5 211 6300.032 -6.372549  1.3588235       0
#' 6 211 6300.040 -6.372549 -0.1235294       0
#'
#'
#' ## plotting the the two signals and annotation:
#' > par(mfcol=c(3,1),mar=c(1,4,2,1))
#' > plot( d$SEC , d$EEG , type="l" , lwd=0.5 , col="darkgray",ylab="EEG")
#' > plot( d$SEC , d$EMG , type="l" , lwd=0.5 , col="darkgray",ylab="EMG")
#' > plot( d$SEC , d$arousal , type="l" , col="blue" , lwd=2,ylab="Arousal")
#' }
#'
#'
#'
#'
#' @note The returned data frame has the following columns:
#' * epoch number \code{E}
#' * elapsed time since the start of the current first epoch \code{SEC}
#' * any signals, in alphabetical order, where column/variable names are the channel labels,
#' potentially sanitized if need be to remove special characters with an underscore
#' (e.g. here \code{EEG(sec)} becomes \code{EEG_sec_})
#' * any annotations, where \code{1} and \code{0} indicate the presence/absence of that annotation at that time-point
#'
#' @section WARNING:
#' The following command would request all the data in an EDF (assuming that all
#' channels have similar sampling rates) and all associated annotation information:
#' \code{
#' ne <- lepoch()
#' d <- ldata(1:ne, lchs(), lannots())}
#' That is, for long EDFs, with a large number of channels and/or high sampling rates,
#' the size of the data frame returned by \code{ldata()} may be large (i.e. too large),
#' if many epochs, channels and/or annotations are requested.
#'
#' @seealso
#' If different channels have different sampling rates, use the \code{RESAMPLE}
#' command to resample one or more channels first, e.g.
#' \code{leval("RESAMPLE sig=ECG sr=125")}. Otherwise it will yield an error:
#' \preformatted{
#' Error in ldata(...) :
#'    requires uniform sampling rate across signals}
#' See \code{\link{leval}} for more details.
ldata <- function(e, chs, annots = character(0)) {
  .Call("Rmatrix_epochs", as.integer(e), as.character(chs), as.character(annots), PACKAGE = "luna")
}

#' Extracts signal (and annotation) data from an EDF for one or more intervals
#'
#' Returns raw signal and annotation data for requested intervals
#'
#' @param i a required list of intervals (in seconds), e.g. as returned by \code{\link{lannots}(annot)}
#' @param chs a required vector of channel names to be returned, i.e. as from \code{\link{lchs}()},
#' all of which are required to have the same sampling rate is a required vector of channel names
#' @param annots an optional vector of annotation names to be returned, i.e. as from \code{\link{lannots}()}
#' @param w an optional window (in seconds) the specifies whether the returned
#' region should also contain a window of up to \code{w} seconds each side of the
#' specified interval(s). For example, if the intervals represented individual
#' sleep spindles, then adding \code{w=10} would return windows of approximately 20 seconds,
#' with the spindle centered in the middle.
#'
#' @return a data frame where each row is one sample point, containing the raw
#' signal (and annotation) data. See the description of \code{\link{ldata}}'s output for more
#' information of columns
#' @export
#'
#' @note This command is effectively the same as \code{\link{ldata}()} except that the regions
#' of interest are specified as intervals rather than in terms of epoch numbers.
#' Intervals are defined as half-open intervals [a,b), meaning all sample-points
#' equal to or greater than a, but less than b.
ldata.intervals <- function(i, chs, annots = character(0), w = 0) {
  if (!is.list(i)) stop("expecting a list() for i")
  if (w > 0) {
    i <- lapply(i, function(x) {
      c(max(0, x[1] - w), x[2] + w)
    })
  }
  if (length(i) != 0) {
    .Call("Rmatrix_intervals", as.numeric(unlist(i)), as.character(chs), as.character(annots), PACKAGE = "luna")
  } else {
    stop("no intervals found")
  }
}



####################################################
##                                                ##
## Misc, helper functions                         ##
##                                                ##
####################################################

#' Cleans syntax of command, factor/level and channel names
#'
#' Replaces potentially troublesome characters with an underscore
#'
#' @param s a string to be cleaned
#'
#' @return a cleaned string
#' @export
#'
#' @examples
#' lsanitize( "C3/M1" )
#'
#' @note This function is a wrapper for the R command \code{gsub("[^[:alnum:]]", "_", s)},
#' which replaces all non-alpha-numeric characters with an underscore. See the
#' Luna lunaR \href{https://zzz.bwh.harvard.edu/luna/tut/tut4/}{tutorial} for a motivating example.
lsanitize <- function(s) {
  gsub("[^[:alnum:]]", "_", s)
}



lstrat <- function(lst, cmd = "") {
  if (cmd == "") {
    n <- names(lst)
    for (i in n) cat(i, ":", lstrat(lst, i), "\n", sep = " ")
  } else {
    t <- names(lst[[cmd]])
  }
}

#' Extracts table(s) from objects returned by \code{ldb(), leval()} or \code{leval.project()}
#'
#' Extract the levels/table(s) from an output list generated by \code{ldb(), leval()} or \code{leval.project()}
#'
#' @param lst a required named-list object, as generated by \code{\link{ldb}()},
#' \code{\link{leval}()} or \code{\link{leval.project}()}
#' @param cmd (optional) names a command (i.e. a first-level item of \code{lst})
#' @param f (optional) specify a specific strata to return (i.e. the second-level items of \code{lst})
#' @param ... all other following arguments. used together with \code{f}
#'
#' @return depending on the arguments given, either a data-frame, a list of
#' data-frames or only a message to the console
#' @export
#'
#' @examples
#' \dontrun{
#' > sl <- lsl("s.lst")
#' > lattach(sl, 2)
#' > k <- leval("EPOCH & MASK ifnot=NREM2 & RE & PSD epoch spectrum sig=EEG")
#'
#' > str(k, max.level=2)
#' List of 4
#' $ EPOCH:List of 1
#' ..$ BL:'data.frame':  1 obs. of  4 variables:
#' $ MASK :List of 1
#' ..$ EPOCH_MASK:'data.frame':  1 obs. of  8 variables:
#' $ PSD  :List of 4
#' ..$ CH    :'data.frame':  1 obs. of  3 variables:
#' ..$ B_CH  :'data.frame':  10 obs. of  5 variables:
#' ..$ CH_F  :'data.frame':  41 obs. of  4 variables:
#' ..$ B_CH_E:'data.frame':  3990 obs. of  6 variables:
#' $ RE   :List of 1
#' ..$ BL:'data.frame':  1 obs. of  5 variables:
#'
#' > lx(k)
#' ## just a message to the console
#' EPOCH : BL
#' MASK : EPOCH_MASK
#' PSD : CH B_CH CH_F B_CH_E
#' RE : BL
#'
#' > lx(k,"EPOCH")
#' $BL
#'   DUR INC   NE
#' 1  30  30 1195
#'
#' > str(lx(k, "PSD"))
#' List of 4
#' $ CH    :'data.frame': 1 obs. of  2 variables:
#'   ..$ CH: chr "EEG"
#'   ..$ NE: num 399
#' $ B_CH  :'data.frame': 10 obs. of  4 variables:
#' ..$ B     : chr [1:10] "ALPHA" "BETA" "DELTA" "FAST_SIGMA" ...
#' ..$ CH    : chr [1:10] "EEG" "EEG" "EEG" "EEG" ...
#' ..$ PSD   : num [1:10] 15.65 5.12 108.24 3.08 5.4 ...
#' ..$ RELPSD: num [1:10] 0.0709 0.0232 0.4903 0.0139 0.0245 ...
#' $ CH_F  :'data.frame':  41 obs. of  3 variables:
#' ..$ CH : chr [1:41] "EEG" "EEG" "EEG" "EEG" ...
#' ..$ F  : num [1:41] 0 0.25 0.75 1.25 1.75 2.25 2.75 3.25 3.75 4.25 ...
#' ..$ PSD: num [1:41] 15.2 77.7 70.6 45.9 37.4 ...
#' $ B_CH_E:'data.frame':    3990 obs. of  5 variables:
#' ..$ B     : chr [1:3990] "ALPHA" "ALPHA" "ALPHA" "ALPHA" ...
#' ..$ CH    : chr [1:3990] "EEG" "EEG" "EEG" "EEG" ...
#' ..$ E     : int [1:3990] 92 93 98 99 100 101 102 118 119 120 ...
#' ..$ PSD   : num [1:3990] 29.5 37.5 33.5 22.7 17.8 ...
#' ..$ RELPSD: num [1:3990] 0.159 0.131 0.12 0.141 0.1 ...
#'
#' > lx(k, "PSD", "B", "CH")
#'             B  CH        PSD     RELPSD
#' 1       ALPHA EEG  15.654212 0.07091026
#' 2        BETA EEG   5.123496 0.02320835
#' 3       DELTA EEG 108.239902 0.49030380
#' 4  FAST_SIGMA EEG   3.078775 0.01394620
#' 5       GAMMA EEG   5.403482 0.02447663
#' 6       SIGMA EEG   8.440213 0.03823237
#' 7        SLOW EEG  38.865512 0.17605253
#' 8  SLOW_SIGMA EEG   5.361438 0.02428618
#' 9       THETA EEG  31.416015 0.14230789
#' 10      TOTAL EEG 220.760887 1.00000000
#'
#' ## this is equivalent as
#' > lx(k, "PSD", "B_CH")
#'
#' ## and
#' > k$PSD$B_CH
#' }
lx <- function(lst, cmd = "", f = "", ...) {
  if (cmd == "") {
    return(lstrat(lst))
  }
  f <- paste(sort(unlist(c(f, list(...)))), sep = "", collapse = "_")
  if (f != "") {
    lst[[cmd]][[f]]
  } else if (length(lst[[cmd]]) == 1) {
    lst[[cmd]][[1]]
  } else {
    lst[[cmd]]
  }
}

#' Like lx() but for lists of returned objects
#'
#' A version of lx() designed to work with lists of returned lists
#'
#' @param k multiple returned values from \code{\link{leval}()} or similar have
#' been assembled as a list of lists
#' @param ... all parameters used in \code{\link{lx}()}, including the command
#' and strata specification
#'
#' @return same output as \code{\link{lx}()}
#' @export
#'
#' @examples
#' \dontrun{
#' > k <- list()
#' > k[[ "S1" ]] <- leval( some-commands-go-here )
#' > k[[ "S2" ]] <- leval( some-more-commands-go-here )
#'
#' > lx2(k)
#' }
lx2 <- function(k, ...) {
  do.call(rbind, lapply(k, lx, ...))
}

#' Extract a particular individual from a returned data frame
#'
#' Extracts one or more particular individual(s) from a data-frame returned by \code{\link{lx}()}
#'
#' @param d a data-frame in the format returned by \code{\link{lx}()}
#' @param id a vector of one or more string \code{ID} of the individual(s) to filter
#'
#' @return the row-subset of the data-frame for individual(s) in \code{id}
#' @export
#'
#' @examples
#' \dontrun{
#' > lid(lx(k, "EPOCH"), "nsrr02")
#'       ID DUR INC   NE
#' 2 nsrr02  30  30 1195
#'
#' > lid(lx(k, "EPOCH"), c("nsrr02", "nsrr03"))
#'       ID DUR INC   NE
#' 2 nsrr02  30  30 1195
#' 3 nsrr03  30  30 1364
#' }
lid <- function(d, id) {
  d[d$ID %in% id, ]
}

#' Return vector of sleep stages from annotations
#'
#' Returns a vector of sleep stages based on annotation data
#'
#' @return a vector of sleep stage labels,
#' i.e. based on attached annotations. For NSRR and other data, Luna expects a
#' standard format for stage names, although this can be modified.
#' See \href{https://zzz.bwh.harvard.edu/luna/ref/hypnograms/#stage}{here} for more information.
#' @export
#'
#' @note This command is identical to running:
#' \preformatted{
#' leval("STAGE")$STAGE$E$STAGE
#' }
lstages <- function() {
  leval("STAGE")$STAGE$E$STAGE
}

#' Reads and parses a Luna command script from a file
#'
#' Reads Luna commands from a file
#'
#' @param filename a required parameter, the name of the file to read
#'
#' @return a vector of Luna commands, parsed from the
#' \href{https://zzz.bwh.harvard.edu/luna/luna/args/#command-files}{command file}
#' in a manner that is suitable for \code{\link{leval}()} (and \code{\link{leval.project}()}.
#' That is, blank lines and comments (starting with \code{%}) are stripped away,
#' and multi-line statements are concatenated into a single line.
#' @export
#'
#' @examples
#' \dontrun{
#' k <- leval(lcmd("/path/to/command.txt"))
#' }
lcmd <- function(filename) {
  lines <- readLines(filename, warn = F)
  lines <- lines[which(lines != "")]
  # delete '%' comments
  for (i in 1:length(lines)) lines[i] <- gsub("%.*", "", lines[i])
  # append lines		    starting with a   space to the previous one
  prv_line <- ""
  newlines <- character(0)
  for (i in 1:length(lines))
  {
    if (substr(lines[i], 1, 1) == " ") {
      if (prv_line == "") stop("badly formed command line continuation")
      prv_line <- paste(prv_line, lines[i])
    } else {
      if (prv_line != "") newlines <- c(newlines, prv_line)
      prv_line <- lines[i]
    }
  }
  if (prv_line != "") newlines <- c(newlines, prv_line)
  newlines
}

#' Maps typical stage labels to colors (for plotting)
#'
#' Convenience function to return colors for stage labels, to be used in plotting
#'
#' @param s a required parameter, a vector of stage label, that should be in the
#' format \code{NREM1, NREM2, NREM3, NREM4, REM} and \code{wake}.
#'
#' @return a corresponding vector of colors
#' @export
#'
#' @examples

#' lstgcols(c("N1", "N2", "N3", "R", "W", "?", "L"))
#'
#' @importFrom grDevices rgb
lstgcols <- function(s) {
  as.vector(sapply(s, function(x) {
    ifelse(x == "NREM1" | x == "N1", rgb(0, 190, 250, 255, maxColorValue = 255),
      ifelse(x == "NREM2" | x == "N2", rgb(0, 80, 200, 255, maxColorValue = 255),
        ifelse(x == "NREM3" | x == "N3", rgb(0, 0, 80, 255, maxColorValue = 255),
          ifelse(x == "NREM4" | x == "N3", rgb(0, 0, 50, 255, maxColorValue = 255),
            ifelse(x == "REM" | x == "R", rgb(250, 20, 50, 255, maxColorValue = 255),
             ifelse(x == "L", rgb( 246, 243, 42, 255, maxColorValue = 255),
               ifelse(x == "wake" | x == "W", rgb(49, 173, 82, 255, maxColorValue = 255),
                rgb(100, 100, 100, 100, maxColorValue = 255)
              )
            )
          )
        )
      )
    )
   )
  }))
}



####################################################
##                                                ##
## Statistical helper functions                   ##
##                                                ##
####################################################


#' 1D total variation denoiser
#'
#' Applies a 1D total variation denoiser
#'
#' @param x a time series vector
#' @param lambda Smoothing parameter (0 to infinity)
#'
#' @return a denoised version of \code{x}
#' @export
#'
#' @note This is a wrapper around the method implemented by the \code{TV} command.
#' See \href{https://zzz.bwh.harvard.edu/luna/ref/power-spectra/#tv}{this page}
#' for a description of the method and parameters.
ldenoise <- function(x, lambda) {
  .Call("R1d_denoise", as.numeric(x), as.numeric(lambda), PACKAGE = "luna")
}

#' FIR filtering
#'
#' Applies a FIR bandpass filter
#'
#' @param x a time series vector
#' @param sr Sample rate of \code{x}
#' @param lwr Lower transition frequency
#' @param upr Upper transition frequency
#' @param tw  Transition width (Hz) (default 1 Hz)
#' @param ripple Ripple (default 0.02)
#'
#' @return a filtered version of \code{x}
#' @export
#'
#' @note This is a wrapper around the method implemented by the \code{FILTER} command.
lfilter <- function(x, sr, lwr, upr, tw=1, ripple=0.02 ) {
 cat( "filtering with " , tw, ripple , "\n" )
 if ( upr > sr/2 ) upr <- sr/2
  .Call("R_filter", as.numeric(x), as.numeric(sr), as.numeric(lwr), as.numeric(upr), as.numeric(tw), as.numeric(ripple), PACKAGE = "luna")
}


#' Linear detrending
#'
#' Detrend a time series
#'
#' @param x a time series vector
#'
#' @importFrom stats resid lm
#'
#' @return a detrended version of \code{x}
#' @export
ldetrend <- function(x) {
resid(lm(x ~ I(1:length(x))))
}

#' Splits a signal into five band-pass filtered signals
#'
#' Convenience function to generate five new band-filtered signals given a raw EEG channel
#'
#' @importFrom utils capture.output
#'
#' @param l a required parameter, which should be a single channel name, e.g.
#' matching one from \code{\link{lchs}()}
#'
#' @return if the channel \code{l} exists, this command will generate five new EDF
#' channels that are band-pass filtered versions of the original, for delta,
#' theta, alpha, sigma and beta bands.
#' @export
#'
#' @note It is simply a wrapper around a series of \href{https://zzz.bwh.harvard.edu/luna/ref/manipulations/#copy}{COPY}
#' and \href{https://zzz.bwh.harvard.edu/luna/ref/fir-filters/#filter}{FILTER} commands.
#' You'll need to edit the function definition, or supply a new one, to get
#' different bands, etc. (Just type \code{lbands} at the R command line to
#' see the function definition.)
lbands <- function(l) {
  invisible(capture.output({
    leval(paste("COPY sig=", l, " tag=delta", sep = ""))
    leval(paste("COPY sig=", l, " tag=theta", sep = ""))
    leval(paste("COPY sig=", l, " tag=alpha", sep = ""))
    leval(paste("COPY sig=", l, " tag=sigma", sep = ""))
    leval(paste("COPY sig=", l, " tag=beta", sep = ""))
    leval(paste("FILTER sig=", l, "_delta bandpass=0.5,4 tw=1 ripple=0.02", sep = ""))
    leval(paste("FILTER sig=", l, "_theta bandpass=4,8   tw=1 ripple=0.02", sep = ""))
    leval(paste("FILTER sig=", l, "_alpha bandpass=8,12  tw=1 ripple=0.02", sep = ""))
    leval(paste("FILTER sig=", l, "_sigma bandpass=12,15 tw=1 ripple=0.02", sep = ""))
    leval(paste("FILTER sig=", l, "_beta  bandpass=15,30 tw=1 ripple=0.02", sep = ""))
  }))
}



####################################################
##                                                ##
## Visualization                                  ##
##                                                ##
####################################################

#' LUNA plotting
#'
#' draws a rectangular heatmap (e.g. spectrogram)
#'
#' @param x x-axis values
#' @param y y-axis values, of \code{length(x)}
#' @param z z-axis values, of \code{length(x)}
#' @param col 100-valued color palette, defaults to \code{\link{lturbo}(100)}
#' @param mt main title (string)
#' @param f filter: boolean values of length equal to \code{length(x)}
#' @param zero boolean vector of length equal to \code{length(x)}; if T, set Z to 0
#' @param xlines draw vertical lines along the x-axis at these values (default: none)
#' @param ylines draw horizontal lines along the y-axus at these values (default: none)
#' @param zlim set range for Z axis (defaults to observed range in Z)
#' @param win winsorize Z at this percentile, e.g. 0.05
#' @param legend integer: return Z quantiles at this many uniformly-spaced positions
#'
#' @return plot is generated in the current graphics device; no return value unless legend is set
#' @export
#'
#' @examples
#' \dontrun{
#' k <- ldb("out.db")
#' d <- k$PSD$CH_E_F
#' lheatmap(d$E, d$F, 10*log10(d$PSD))
#' }
#'
#' @importFrom graphics image abline
#' @importFrom stats quantile
lheatmap <- function(x, y, z,
                     col = lturbo(100),
                     mt = "",
                     f = rep(T, length(z)),
                     zero = rep(F, length(z)),
                     xlines = NULL, ylines = NULL,
                     zlim = NULL,
                     win = NULL ,
		     legend = NULL ,
		     useRaster = T ) {
  # assumes a square matrix
  z[zero] <- 0
  x <- x[f]
  y <- y[f]
  z <- z[f]
  nx <- length(unique(x))
  ny <- length(unique(y))
  nz <- length(z)
  if (nz == 0) stop("no data to plot")
  if (nz != nx * ny) stop("requires square data")
  d <- data.frame(x, y, z)
  d <- d[order(d$y, d$x), ]
  if (!is.null(win)) {
    d$z <- lwin(d$z, win)
  }
  if (is.null(zlim)) zlim <- range(d$z,na.rm=T)

  m <- matrix(d$z, byrow = T, nrow = ny, ncol = nx)
  image(t(m[1:ny, ]), col = col, xaxt = "n", yaxt = "n", main = mt, zlim = zlim, useRaster=useRaster)
  xr <- range(x)
  yr <- range(y)
  if (!is.null(xlines)) abline(v = ((xlines - xr[1]) / (xr[2] - xr[1])))
  if (!is.null(ylines)) abline(h = ((ylines - yr[1]) / (yr[2] - yr[1])))

  # for making a legend
  if ( ! is.null( legend ) )
  {
    if ( ! is.numeric( legend ) ) stop( "legend should be an integer" )
    q  <- quantile( z , probs = seq(0,1,length.out=legend))
    return( q )
  }

}

## ------------------------------------------------------------
##
## Point-plot (cf heatmap but w/ sparse grid) 
##
## ------------------------------------------------------------

#' LUNA plotting
#'
#' draws a sparse heatmap
#'
#' @param x x-axis values
#' @param y y-axis values, of \code{length(x)}
#' @param z z-axis values, of \code{length(x)}
#' @param xs x-axis size of each point
#' @param ys y-axis size of each point
#' @param xlim min/max of x-axis values
#' @param ylim min/max of x-axis values
#' @param col 100-valued color palette, defaults to \code{\link{lturbo}(100)}
#' @param mt main title (string)
#' @param xlines draw vertical lines along the x-axis at these values (default: none)
#' @param ylines draw horizontal lines along the y-axus at these values (default: none)
#' @param zlim set range for Z axis (defaults to observed range in Z)
#' @param win winsorize Z at this percentile, e.g. 0.05
#'
#' @return plot is generated in the current graphics device
#' @export
#'
#' @importFrom graphics image abline
lpointmap <- function(x, y, z,
                      xlim = range( x , na.rm=T ) , ylim=range( y , na.rm=T ),
		      xs , ys , 
                      col = lturbo(100),
                      mt = "",
                      zlim = NULL,
                      win = NULL ) {		       
  nx <- length(x);ny <- length(y);nz <- length(z)
  if (nz == 0) stop("no data to plot")
  if (nz != nx || nz != ny ) stop("x, y and z must be of similar length" )
  d <- data.frame(x, y, z)
  if (!is.null(win)) {
    d$z <- lwin(d$z, win)
  }
  if (is.null(zlim)) zlim <- range(d$z,na.rm=T)

  # scale x on xlim: plot from 0 to 1
  d$x <- ( d$x - xlim[1] ) / ( xlim[2] - xlim[1] )
  d$y <- ( d$y - ylim[1] ) / ( ylim[2] - ylim[1] )

  # scale/limit z?
  d$z <- round( 100 * ( d$z - zlim[1] ) / ( zlim[2] - zlim[1] ) )
  d$z[ d$z < 1 ] <- 1 ; d$z[ d$z > 100 ] <- 100 ; 

  # remove out of scope points
  d <- d[ d$x >= 0 & d$x <= 1 & d$y >= 0 & d$y <= 1 , ]

  # size of rects
  xp <- xs / ( xlim[2] - xlim[1] )
  yp <- ys / ( ylim[2] - ylim[1] )
  
  # canvas
  plot( c(0,1), c(0,1) , type="n" , main = mt , xlab="", ylab="", axes=F , xaxs="i", yaxs="i" ) 
  # draw
  rect( d$x , d$y , d$x + xp , d$y + yp , col = col[d$z] , border = NA )

}


## --------------------------------------------------------------------------------
##
## Standard 64-channel 2D coords
##
## --------------------------------------------------------------------------------


lremap.chs <- function(chs) {
  chs <- toupper(chs)
  chs[chs == "T3"] <- "T7"
  chs[chs == "T4"] <- "T8"
  chs[chs == "T5"] <- "P7"
  chs[chs == "T6"] <- "P8"
  chs
}

#' Generating a data frame with default 64-channel EEG positions
#'
#' @param chs an optional character vector of channel labels
#'
#' @return a data frame of channel, X and Y co-ordiantes (CH, X and Y);  if the
#' \code{chs} argument is not empty, then only channels matching an entry in \code{chs} are displayed
#' @export
#'
#' @note
#' - Label matching is case-insenstive;
#' - Currently, coordinates are only specified for standard 64-channel montages;
#' - This function is primarily used internally by other luna plotting functions;
#' - If user set by lset.xy(), just returns those instead

ldefault.xy <- function(chs = character(0)) {

# use user-defined set?

if ( luna.globals$user_xy )
{
  if (length(chs) == 0) {
    return(luna.globals$xy )
  }
  return( luna.globals$xy[ luna.globals$xy$CH %in% toupper(chs), ] )
}

# use, build something sensible (for 64-chs)

chlab <- c(
    "Fp1", "AF7", "AF3", "F1",  "F3",  "F5",  "F7",  "FT7", "FC5", "FC3", "FC1", "C1",
    "C3",  "C5",  "T7",  "TP7", "CP5", "CP3", "CP1", "P1",  "P3",  "P5",  "P7",  "P9",
    "PO7", "PO3", "O1",  "Iz",  "Oz",  "POz", "Pz",  "CPz", "Fpz", "Fp2", "AF8", "AF4",
    "AFz", "Fz",  "F2",  "F4",  "F6",  "F8",  "FT8", "FC6", "FC4", "FC2", "FCz", "Cz",
    "C2",  "C4",  "C6",  "T8",  "TP8", "CP6", "CP4", "CP2", "P2",  "P4",  "P6",  "P8",
    "P10", "PO8", "PO4", "O2"
  )

  chx <- c(
    -0.139058, -0.264503, -0.152969, -0.091616, -0.184692, -0.276864, -0.364058,
    -0.427975, -0.328783, -0.215938, -0.110678, -0.112500, -0.225000, -0.337500,
    -0.450000, -0.427975, -0.328783, -0.215938, -0.110678, -0.091616, -0.184692,
    -0.276864, -0.364058, -0.430900, -0.264503, -0.152969, -0.139058, 0.000000,
    0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.139058, 0.264503,
    0.152969, 0.000000, 0.000000, 0.091616, 0.184692, 0.276864, 0.364058,
    0.427975, 0.328783, 0.215938, 0.110678, 0.000000, 0.000000, 0.112500,
    0.225000, 0.337500, 0.450000, 0.427975, 0.328783, 0.215938, 0.110678,
    0.091616, 0.184692, 0.276864, 0.364058, 0.430900, 0.264503, 0.152969,
    0.139058
  )

  chy <- c(
    0.430423, 0.373607, 0.341595, 0.251562, 0.252734, 0.263932, 0.285114,
    0.173607, 0.162185, 0.152059, 0.148380, 0.050000, 0.050000, 0.050000,
    0.050000, -0.073607, -0.062185, -0.052059, -0.048380, -0.151562, -0.152734,
    -0.163932, -0.185114, -0.271394, -0.273607, -0.241595, -0.330422, -0.450000,
    -0.350000, -0.250000, -0.150000, -0.050000, 0.450000, 0.430423, 0.373607,
    0.341595, 0.350000, 0.250000, 0.251562, 0.252734, 0.263932, 0.285114,
    0.173607, 0.162185, 0.152059, 0.148380, 0.150000, 0.050000, 0.050000,
    0.050000, 0.050000, 0.050000, -0.073607, -0.062185, -0.052059, -0.048380,
    -0.151562, -0.152734, -0.163932, -0.185114, -0.271394, -0.273607, -0.241595,
    -0.330422
  )

  chxy <- data.frame(CH = toupper(chlab), X = chx, Y = chy)

  if (length(chs) == 0) {
    return(chxy)
  }
  chxy <- chxy[chxy$CH %in% toupper(chs), ]
  return(chxy)
}

## --------------------------------------------------------------------------------
##
## Set EEG X/Y co-ords
##
## --------------------------------------------------------------------------------


#' Pass a data frame with arbitrary 2D plot coords for EEG positions
#'
#' @param d a data frame with required columns CH, X and Y
#'
#' @return a data frame of channel, X and Y co-ordiantes (CH, X and Y);  if the
#' @export
#'
#' @note
#' - Label matching is case-insenstive;

lset.xy <- function( d ) { 

cols <- names(d)
if ( length(cols) != 3 || cols[1] != "CH" || cols[2] != "X" || cols[3] != "Y" )
 stop( "requires CH X Y" )

luna.globals$xy <- d

luna.globals$user_xy = T 

}



## --------------------------------------------------------------------------------
##
## Plot generic X/Y line plots (e.g. power spectra) with
##
## --------------------------------------------------------------------------------

#' LUNA plotting
#'
#' draw a series of X-Y scatter/line plots, one-per-channel arranged as a topoplot
#'
#' @param c character vector of channel labels
#' @param x nuremic vector of X-axis values
#' @param y numeric vector of Y-axis values
#' @param z optional numeric vector of Z-axis values (default NA)
#' @param zlim optional range for Z
#' @param f optional boolean vector of length(x), filter in/out each observation
#' @param y.symm boolean, make Y-axis symmetric around 0 (default = F)
#' @param sz optional numeric value: size of points (default 0.08)
#' @param col optional color, default 'black'
#' @param lwd optional numeric value for line width (default to 0.5)
#' @param xline optional vector of X values to plot vertical lines
#' @param yline optional vector of Y values to plot horizontal lines
#' @param pch optional point symbol (default NA)
#' @param cex optional point size (default 1)
#' @param ylim optional Y axis limit
#' @param xlab X axis label (default = Frequency Hz))
#' @param ylab Y axis label (default = log(power))
#' @param mt main title (default, "")
#'
#' @return plot is generated in the current graphics device; no return value
#' @export
#'
#' @examples
#' \dontrun{
#' ltopo.xy(c = hj$CH, x = hj$E, y = log(hj$H1), xlab = "Epoch", ylab = "H1",
#'     pch=20, col=rbpal, cex = 0.2)
#' }
#'
#' @note If \code{pch} is non-\code{NULL}, then \code{z} can be a vector of values
#' (\code{length(x)}) and \code{col} can be a 100-element palette: in this case,
#' the color of each point is scaled by the percentile of \code{z};
#' if \code{pch} is missing, this function draws a X-Y line
#' plots, which must be a single color (i.e. no \code{z} values are allowed)
#'
#' @importFrom graphics rect text lines points
ltopo.xy <- function(c, x, y, z = NA, zlim = NA,
                     f = rep(T, length(x)), y.symm = F,
                     sz = 0.08,
                     col = "black", lwd = 0.5,
                     xline = numeric(),
                     yline = numeric(),
                     pch = NA, cex = 1,
                     ylim = NULL,
                     xlab = "Frequency (Hz)", ylab = "log(power)", mt = "") {
  topo <- ldefault.xy()
  c <- lremap.chs(c)
  f[!c %in% toupper(topo$CH)] <- F
  # z is color depth: scaled 1..100 as requires that col is a 100-element pallete
  if (!is.na(z[1])) {
    if (length(z) != length(x)) stop("is z is specified, must match other long data")
    if (is.na(pch)) stop("cannot specify z with line points")
    if (length(col) != 100) stop("requires col is a 100-element palette if z is specified")
    z <- z[f]
    if (is.na(zlim[1])) zlim <- range(z, na.rm = T)
    zcol <- col[1 + round(99 * ((z - zlim[1]) / (zlim[2] - zlim[1])))]
    col <- zcol
  } else if (!is.na(pch)) {
    # single color for point plots
    col <- rep(col, length(x))
  }
  # also pair down x/y/CH to filtered set
  c <- c[f]
  x <- x[f]
  y <- y[f]
  # ranges (and symmetric Y?)
  rx <- range(x)
  if (is.null(ylim)) {
    ry <- range(y)
  } else {
    ry <- ylim
  }
  if (y.symm) ry <- c(-max(abs(ry)), max(abs(ry)))
  # pltos
  plot(c(0, 1), c(0, 1), type = "n", axes = F, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  rect(0, 0, 1, 1)
  # draw.circle( 0.5,0.5,0.5 )
  if (mt != "") text(0.025, 0.05, mt, cex = 0.8, pos = 4)
  lgd.x <- 0.88
  lgd.y <- 0.08
  lines(c(lgd.x, lgd.x + sz), c(lgd.y, lgd.y))
  lines(c(lgd.x, lgd.x), c(lgd.y, lgd.y + sz))
  text(lgd.x, lgd.y, signif(ry[1], 2), cex = 0.5, pos = 2)
  text(lgd.x, lgd.y + sz, signif(ry[2], 2), cex = 0.5, pos = 2)
  text(lgd.x, lgd.y, signif(rx[1], 2), cex = 0.5, pos = 1)
  text(lgd.x + sz, lgd.y, signif(rx[2], 2), cex = 0.5, pos = 1)
  text(lgd.x + sz / 2, lgd.y - 0.05, xlab, cex = 0.5)
  text(lgd.x - 0.05, lgd.y + sz / 2, ylab, cex = 0.5)
  # plot each channel
  for (ch in unique(c)) {
    ch.idx <- toupper(topo$CH) == ch
    px <- topo$X[ch.idx] + 0.5
    py <- topo$Y[ch.idx] + 0.5
    if (length(px) == 1) {
      ch.label <- topo$CH[ch.idx]
      x0 <- px - sz / 2
      x1 <- x0 + sz
      y0 <- py - sz / 2
      y1 <- y0 + sz
      lines(c(x0, x1), c(y0, y0), col = "gray")
      lines(c(x0, x0), c(y0, y1), col = "gray")
      xx <- x[c == ch]
      yy <- y[c == ch]
      xx <- (xx - rx[1]) / (rx[2] - rx[1])
      yy <- (yy - ry[1]) / (ry[2] - ry[1])
      for (xl in xline) lines(rep(x0 + sz * (xl - rx[1]) / (rx[2] - rx[1]), 2), c(y0, y1), col = "gray", lwd = 0.5)
      for (yl in yline) lines(c(x0, x1), rep(y0 + sz * (yl - ry[1]) / (ry[2] - ry[1]), 2), col = "gray", lwd = 0.5)
      if (!is.na(pch)) {
        points(x0 + xx * sz, y0 + yy * sz, pch = pch, cex = cex, col = col[c == ch])
      } else {
        lines(x0 + xx * sz, y0 + yy * sz, lwd = lwd, col = col)
      } # just single color for lines
      text(x0 + 0.8 * sz, y0 + 0.9 * sz, ch.label, cex = 0.5, col = ifelse(col == "black", "blue", "black"))
    }
  }
}

## --------------------------------------------------------------------------------
##
## larrow
##
## --------------------------------------------------------------------------------

# z   : sets line orientation and thickness
# z2  : sets line color (default == z), e.g. to reflect C/C differences it can be set separately

# zr  : optional range for z
# z2r : optional range for z2
# flt : optional filter of points 
# dst : only show connections that are local (i.e. < dst threshold )

larrow <- function(chs1, chs2, z, flt = NULL, zr = NULL , z2 = NULL , z2r = NULL , cex = 2, title = "", head = T , dst = 0.35 , lwd1 = 0.5,  lwd2 = 2 , directional = F )
{
    # directional (instead of z2)
    if ( directional & ! is.null( z2 ) ) stop( "cannot specify both directional and z2" )

    # z  : sets line orientation and thickness
    # z2 : sets line color (default == z) ; to reflect C/C differences can be set separately 
    has.z2 <- ! is.null( z2 ) 
    if ( is.null( z2 ) ) z2 <- z


    if ( is.null( flt ) ) flt <- rep(T,length(z))
    chs1 <- lremap.chs(chs1); chs2 <- lremap.chs(chs2)
    xy   <- ldefault.xy(unique(c(chs1, chs2)))

    conns <- character()
    for (c1 in unique( chs1[ ! is.na( chs1 ) ] ) )
    for (c2 in unique( chs2[ ! is.na( chs2 ) ] ) )	
     {
      d <- sqrt( ( xy$X[ xy$CH == c1 ] - xy$X[ xy$CH == c2 ] )^2 + ( xy$Y[ xy$CH == c1 ] - xy$Y[ xy$CH == c2 ] )^2  )
      if ( d < dst ) conns <- c( conns , toupper( paste( c1, c2 ) ) , toupper( paste( c2 , c1 ) ) )
     }
     if ( length(conns) == 0 ) return(0)
    inc <- toupper( paste( chs1 , chs2 ) ) %in% conns
    chs1 <- chs1[inc];     chs2 <- chs2[inc]
    z <- z[inc] ; z2 <- z2[inc]; flt <- flt[inc]

    # assume double-entered, only plot positives (for Z); but may keep z2 as negative
    flt[ z < 0 ] <- F
    chs1 <- chs1[flt]; chs2 <- chs2[flt]; z <- z[flt]; z2 <- z2[flt ]
    if ( length(chs1) == 0 ) return
    if ( is.null( zr ) ) zr <- range(z)
    if ( is.null( z2r ) ) z2r <- range(z2)
    if ( has.z2 ) z2r <- c( -1 * max(abs(z2r)) , max(abs(z2r)) )
   
    oz <- order(abs(z))
    chs1 <- chs1[oz];     chs2 <- chs2[oz]
    z <- z[oz] ; z2 <- z2[oz] ; 
    if (length(chs1) != length(z))  stop("bad inputs to ltopo.conn()")

    xy.coh <- xy
    z <- round(100 * (z - zr[1])/(zr[2] - zr[1]))
    z[z == 0] <- 1;    z[z > 100] <- 100
    z2 <- round(100 * (z2 - z2r[1])/(z2r[2] - z2r[1]))
    z2[z2 == 0] <- 1;    z2[z2 > 100] <- 100

    rbpal <- grDevices::colorRampPalette(c("gray", "orange", "red"))(100)
    if ( has.z2 ) rbpal <- grDevices::colorRampPalette(c("navy", "blue", "lightblue", "gray", "pink", "red", "darkred"))(100)

    dirpalD1 <- grDevices::colorRampPalette(c("lightblue","blue","navy"))(100)
    dirpalD2 <- grDevices::colorRampPalette(c("pink", "red", "darkred"))(100)
    dirpalEQL <- grDevices::colorRampPalette(c("gray", "black"))(100)

    plot(xy.coh$X, xy.coh$Y, pch = 21, cex = cex, main = title,
        lwd = 0.5, bg = NA, col = "gray", axes = F, xaxt = "n",
        yaxt = "n", xlab = "", ylab = "", ylim = c(-0.55, 0.55), xlim = c(-0.55, 0.55))

    for (j in 1:length(chs1)) {
       p1 <- unlist( xy.coh[ xy.coh$CH == chs1[j] , c( "X","Y" ) ] )
       p2 <- unlist( xy.coh[ xy.coh$CH == chs2[j] , c( "X","Y" ) ] )
       if ( ! is.na( p1[1] ) ) { 
        px <- fshorten( p1[1],p1[2],p2[1],p2[2],p=0.4)

        if ( directional ) {

          if ( p1[2] > p2[2] ) {
           arrows( px[1], px[2] , px[3] , px[4] , col = dirpalD1[z2[j]] ,  length = 0.08, angle = 10 , lwd=lwd1+lwd2*(z[j]/100) )
	  } else {           
            if ( p1[2] == p2[2] ) { 
              arrows( px[1], px[2] , px[3] , px[4] , col = dirpalEQL[z2[j]] ,  length = 0.08, angle = 10 , lwd=lwd1+lwd2*(z[j]/100) )
	     } else {
              arrows( px[1], px[2] , px[3] , px[4] , col = dirpalD2[z2[j]] ,  length = 0.08, angle = 10 , lwd=lwd1+lwd2*(z[j]/100) )
             }
	  }
         } else {	           
           arrows( px[1], px[2] , px[3] , px[4] , col = rbpal[z2[j]] ,  length = 0.08, angle = 10 , lwd=lwd1+lwd2*(z[j]/100) )
         }
      }
     }

    points(xy.coh$X, xy.coh$Y, pch = 21, cex = cex, lwd = 0.5, bg = NA, col = "gray")
    if (head) {
        draw.ellipse(0, 0, 0.5, 0.5, lwd = 0.75, border = "darkgray")
        lines(c(0,  -0.05), c(0.55, 0.5), lwd = 0.75, col = "darkgray")
        lines(c(0,   0.05), c(0.55, 0.5), lwd = 0.75, col = "darkgray")
    }
 cat( length(chs1) , "pairs, range" , zr , "\n" )

}




## --------------------------------------------------------------------------------
##
## Topo heat map
##
## --------------------------------------------------------------------------------

#' LUNA plotting
#'
#' draws a 'topoplot' using a heatmap color scale
#'
#' @param c vector of channel labels
#' @param z vector of values to plot (same length as \code{c})
#' @param sz integer: size of channel circles plotted (default = 1)
#' @param flt boolean filter of length equal to \code{length(c)}
#' @param zlab label for Z-axis
#' @param mt main title label
#' @param zlim set Z range (defaults to observed range)
#' @param th single numeric value threshold for highlighting certain channels (default NULL)
#' @param th.z vector, same length as \code{c} and \code{z}; channels with \code{th.z} \eqn{\ge} \code{th} are highlighted
#' @param show.leg boolean: plot a legend (default T)
#' @param zeroed boolean: make Z range symmetric around zero if T (default = F)
#' @param head boolean: draw simple head image (circle, default = F)
#'
#' @return plot is generated in the current graphics device; no return value
#' @export
#'
#' @examples
#' \dontrun{
#' ltopo.heat(psd$CH, psd$PSD, flt = psd$F == 15, sz = 3, zlab = "Power 15 Hz", mt = "Plot 1")
#' }
ltopo.heat <- function(c, z,
                       sz = 1,
                       flt = rep(T, length(z)),
                       zlab = "",
                       mt = "",
                       zlim = NULL,
                       th = NA,
                       th.z = z,
                       show.leg = F,
                       zeroed = F, head = F) {
  ltopo.rb(c, z, flt, sz, zlab, mt, zlim, th, th.z, show.leg, zeroed, head, col = grDevices::colorRampPalette(rev(c("red", "orange", "yellow", "cyan", "blue")))(101))
}

#' LUNA plotting
#'
#' draws a 'topoplot' using a red-blue color scale
#'
#' @inheritParams ltopo.heat
#' @param col 101-valued palette, defaults to \code{grDevices::colorRampPalette(c("blue", "white", "red"))(101)}
#' @param ring.lwd width of rings around highlighted channels (defaults to 1)#'
#'
#' @return plot is generated in the current graphics device; no return value
#' @export
#'
#' @note \code{ltopo.rb(...)} is the same as \code{\link{ltopo.heat}(...)} with
#' \code{zeroed = T} and a different color scheme (blue as negative, red as positive, white as 0)
#'
#' @importFrom plotrix draw.ellipse
#' @importFrom graphics lines points text
ltopo.rb <- function(c, z,
                     flt = rep(T, length(z)),
                     sz = 1,
                     zlab = "",
                     mt = "",
                     zlim = NULL,
                     th = NULL,
                     th.z = z,
                     show.leg = F,
                     zeroed = T,
                     head = F,
                     col = grDevices::colorRampPalette(c("blue", "white", "red"))(101),
                     ring.lwd = 1) {
  topo <- ldefault.xy()
  if (length(col) != 101) stop("col needs to be 101 length")
  c <- c[flt]
  z <- z[flt]
  th.z <- th.z[flt]
  c <- lremap.chs(c)
  f <- c %in% toupper(topo$CH)
  c <- c[f]
  z <- z[f]
  th.z <- th.z[f]
  c <- c[order(abs(z))]
  th.z <- th.z[order(abs(z))]
  z <- z[order(abs(z))]
  if (is.null(zlim)) zlim <- range(z, na.rm = T)
  if (zeroed) zlim <- c(-1, 1) * max(abs(zlim))
  plot(c(0, 1), c(0, 1), type = "n", axes = F, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  if (head) {
    draw.ellipse(0.5, 0.5, 0.5, 0.45, lwd = 0.75, border = "darkgray")
    lines(c(0.5, 0.45), c(0.99, 0.95), lwd = 0.75, col = "darkgray")
    lines(c(0.5, 0.55), c(0.99, 0.95), lwd = 0.75, col = "darkgray")
  }
  if (mt != "") text(0.025, 0.95, mt, cex = 0.8, pos = 4)
  text(0.75, 0.05, zlab, cex = 1)
  for (ch in unique(c)) {
    px <- topo$X[topo$CH == ch] + 0.5
    py <- topo$Y[topo$CH == ch] + 0.5
    if (length(px) == 1) {
      if (sum(c == ch) > 1) stop("multiple values for a single channel")
      ch.label <- topo$CH[toupper(topo$CH) == ch]
      this.z <- z[c == ch]
      this.th.z <- th.z[c == ch]
      ring <- rep("gray", length(px))
      ring.lwd <- 1
      if (!is.null(th)) ring[this.th.z >= th] <- "black"
      if (!is.null(th)) ring.lwd[this.th.z >= th] <- 2
      points(px, py,
        pch = 21, cex = sz, col = ring, lwd = ring.lwd,
        bg = col[1 + round(100 * ((this.z - zlim[1]) / (zlim[2] - zlim[1])))]
      )
      x0 <- px - sz / 2
      y0 <- py - sz / 2
      #  cat( ch , ch.label, this.z , "\n" )
    }
  }
  for (ch in unique(c)) {
    px <- topo$X[toupper(topo$CH) == ch] + 0.5
    py <- topo$Y[toupper(topo$CH) == ch] + 0.5
    if (length(px) == 1) {
      ch.label <- topo$CH[toupper(topo$CH) == ch]
      x0 <- px - sz / 2
      y0 <- py - sz / 2
      # text( px-0.005*sz,py+0.005*sz, ch.label , cex=0.6,col="blue")
    }
  }
  if (show.leg) {
    points(seq(0.05, 0.5, length.out = 101), rep(0.05, 101), col = col, pch = 20)
    text(0.05, 0.01, signif(zlim[1], 3), cex = 1)
    text(0.5, 0.01, signif(zlim[2], 3), cex = 1)
  }
}


## --------------------------------------------------------------------------------
##
## Topo coherence plots (links between electrodes)
##
## --------------------------------------------------------------------------------

# need diff. coords, so make xy.coh
#' Generating a rescaled coordinate data frame
#'
#' Rescales a coordinate data frame (e.g. from \code{\link{ldefault.xy}()})
#' such that it is appropriate for pairwise (connectivity) plots
#'
#' @param xy a data frame with columns \code{X} and \code{Y}
#'
#' @return a data frame with X and Y coordinates rescaled
#' @export
#'
#' @note
#' - Assumes the X and Y inputs are scaled for a unit circle, i.e. -0.5 to +0.5
#' - This function is only intended to be used internally
ldefault.coh.xy <- function(xy) {
  xy$X <- 100 * xy$X
  xy$Y <- (30 * xy$Y) + 10
  return(xy)
}

# helper function: draw ARC
#' @importFrom geosphere gcIntermediate
#' @importFrom graphics lines
farc <- function(c1, c2, kol, w = 4) {
  gc <- gcIntermediate(unlist(luna.globals$xy.coh[luna.globals$xy.coh$CH == c1, c("X", "Y")]),
                       unlist(luna.globals$xy.coh[luna.globals$xy.coh$CH == c2, c("X", "Y")]),
    breakAtDateLine = TRUE, n = 100
  )
  lines(gc, lwd = w + 1, col = "black")
  lines(gc, lwd = w, col = kol)
  # invisible(lapply(gc, lines, col=k, lwd=2))
}

#' @importFrom geosphere gcIntermediate
#' @importFrom graphics segments
farc.signed <- function(c1, c2, k1, k2, w = 4) {
  gc <- gcIntermediate(unlist(luna.globals$xy.coh[luna.globals$xy.coh$CH == c1, c("X", "Y")]),
                       unlist(luna.globals$xy.coh[luna.globals$xy.coh$CH == c2, c("X", "Y")]),
    breakAtDateLine = TRUE,
    n = 100
  )
  segments(gc[, 1], gc[, 2], gc[, 1], gc[, 2],
    lwd = w,
    t_cols(grDevices::colorRampPalette(c(k2, "white", k1))(101), 80)
  )
}



# palette
rbpal <- rev(rainbow(150)[1:100])
fcol <- grDevices::colorRampPalette(c("blue", "white", "red"))
rbpal <- fcol(100)


# fhead1() topo
#' @importFrom plotrix draw.ellipse
#' @importFrom graphics lines points
fhead1 <- function(chs, z, flt = T, zr = range(z, na.rm = T), cex = 4, title = "") {
  plot(luna.globals$xy.coh$X, luna.globals$xy.coh$Y,
    pch = 21, cex = cex * 0.5, bg = "white",
    axes = F, xaxt = "n", yaxt = "n", xlab = "", ylab = "", ylim = c(-2, 24), xlim = c(-55, 55), main = title
  )
  draw.ellipse(0, 9.5, 52, 12)
  lines(c(0, -8), c(23, 21), lwd = 1)
  lines(c(0, 8), c(23, 21), lwd = 1)
  if (!any(flt)) {
    return(0)
  }
  chs <- chs[flt]
  z <- z[flt]
  if (length(chs) != length(z)) stop("bad")
  z <- (z - zr[1]) / (zr[2] - zr[1])
  z <- round(z * 100)
  z[z == 0] <- 1
  z[z > 100] <- 100
  for (j in 1:length(chs)) {
    xx <- luna.globals$xy.coh$X[luna.globals$xy$CH == chs[j]]
    yy <- luna.globals$xy.coh$Y[luna.globals$xy$CH == chs[j]]
    points(xx, yy, pch = 21, cex = cex * 1.1, bg = "white", lwd = 1.5)
    points(xx, yy, pch = 21, cex = cex, bg = rbpal[z[j]])
  }
}

#
# ltopo: connectivity
#

#' LUNA plotting
#'
#' draw a connectivity topoplot (i.e. of lines between pairs of channels)
#'
#' @param chs1 channel \code{a}
#' @param chs2 channel \code{b}
#' @param z numeric vector - the associated value for the pair \code{(a,b)}
#' @param flt optional boolean vector of \code{length(z)} - only plot values that are T here
#' @param zr range of Z axis
#' @param cex size of points (default 2)
#' @param w width of lines (defauly 8)
#' @param title main title text (default "")
#' @param head boolean value: if T, plot a simple head outline
#' @param signed boolean value: if T, assume Z values are signed and show directions in plots
#'
#' @return plot is generated in the current graphics device; no return value
#' @export
#'
#' @examples
#' \dontrun{
#' ltopo.conn(chs1 = coh$CH1, chs2 = coh$CH2, z = coh$ICOH,
#'     flt = coh$B == "SIGMA" & coh$COH > 0.7, w = 5, signed = T)
#' }
#'
#' @note
#' - lines are drawn in ascending order of the absolute value of \code{z} - i.e. so that
#' larger values are more evident on the plot.  Note that this plot can take a long
#' time and give very indistinct figures if too many pairs are included - use the
#' filter \code{f} argument to restrict visualization to only the most relevant sets of pairs
#' - if signed equals T, then the lines connecting two channels are drawn using a gradient
#' of 'red-to-white-to-blue' going from one channel to the second, depending on the sign
#' of Z, such that if Z(A,B) = +2, then the line at channel 'A' is blue, whereas the end at 'B' will be red
#' (e.g. implying a flow of information "from A to B").  If Z(a,b) = -2, the opposite will be rendered.
#' - if \code{signed} is F, then all lines are of the same, solid color. The intention of the signed option
#' is to be able to visually represent directed connectivity metrics (e.g. PSI), in contrast to
#' undirected metrics such as magnitude squared coherence.
#'
#' @importFrom plotrix draw.ellipse
#' @importFrom graphics lines points
ltopo.conn <- function(chs1, chs2, z, flt = T, zr = range(z[flt], na.rm = T),
                       cex = 2, w = 8, title = "", head = T, signed = F) {
  chs1 <- lremap.chs(chs1)
  chs2 <- lremap.chs(chs2)
  xy.coh <- ldefault.coh.xy(ldefault.xy(unique(c(chs1, chs2))))
  plot(xy.coh$X, xy.coh$Y,
    pch = 21, cex = cex, main = title, lwd = 0.5,
    bg = NA, col = "gray", axes = F, xaxt = "n", yaxt = "n", xlab = "",
    ylab = "", ylim = c(-2, 24), xlim = c(-55, 55)
  )
  if (head) {
    draw.ellipse(0, 9.5, 52, 12, lwd = 0.75, border = "darkgray")
    lines(c(0, -8), c(23, 21), lwd = 0.75, col = "darkgray")
    lines(c(0, 8), c(23, 21), lwd = 0.75, col = "darkgray")
  }
  if (!any(flt)) {
    return(0)
  }
  if (signed) zr <- c(-max(abs(zr)), max(abs(zr)))
  chs1 <- chs1[flt]
  chs2 <- chs2[flt]
  z <- z[flt]
  chs1 <- chs1[order(abs(z))]
  chs2 <- chs2[order(abs(z))]
  z <- z[order(abs(z))]
  if (length(chs1) != length(z)) stop("bad inputs to ltopo.conn()")
  negz <- z
  if (signed) negz <- -1 * negz
  z <- round(100 * (z - zr[1]) / (zr[2] - zr[1]))
  z[z == 0] <- 1
  z[z > 100] <- 100
  negz <- round(100 * (negz - zr[1]) / (zr[2] - zr[1]))
  negz[negz == 0] <- 1
  negz[negz > 100] <- 100
  if (signed) {
    for (j in 1:length(chs1)) {
      t <- c(chs1[j], chs2[j])
      farc.signed(t[1], t[2], rbpal[z[j]], rbpal[negz[j]], w = w)
    }
  } else {
    for (j in 1:length(chs1)) {
      t <- c(chs1[j], chs2[j])
      farc(t[1], t[2], rbpal[z[j]], w = w)
    }
  }
  points(xy.coh$X, xy.coh$Y, pch = 21, cex = cex, lwd = 0.5, bg = NA, col = "gray")
  if (head) {
    draw.ellipse(0, 9.5, 52, 12, lwd = 0.75, border = "darkgray")
    lines(c(0, -8), c(23, 21), lwd = 0.75, col = "darkgray")
    lines(c(0, 8), c(23, 21), lwd = 0.75, col = "darkgray")
  }
  cat("zr", zr, "\n")
}

#' @importFrom stats quantile
intop <- function(x, p) {
  x > quantile(x, 1 - p)
}

#' @importFrom stats quantile
inbottom <- function(x, p) {
  x < quantile(x, p)
}

#' @importFrom grDevices col2rgb rgb
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  rgb.val <- col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
    maxColorValue = 255,
    alpha = (100 - percent) * 255 / 100,
    names = name
  )
  invisible(t.col)
}


t_cols <- function(x, percent = 50) {
  sapply(x, t_col, percent)
}


#
# ltopo.dconn : directed connectivity (i.e. A->B)
# i.e. 'ch' specifies which channel to 'seed' on
# so always assuming a signed value here
#

#' LUNA plotting
#'
#' draw	a connectivity topoplot	(i.e. of lines between pairs of	channels, but seeding on one channel,
#' i.e. as a special case of \code{\link{ltopo.conn}()}
#'
#' @param ch single character value: channel to seed on
#' @param chs1 channel 'a' as per \code{\link{ltopo.conn}()}
#' @param chs2 channel 'b' as per \code{\link{ltopo.conn}()}
#' @param z numeric vector of pairwise Z values
#' @param flt optional boolean vector of length(z), to filter observations in/out of the plot
#' @param zr range of Z axis
#' @param cex size of channel circles
#' @param w width of lines between channels
#' @param title main title text
#' @param head boolean: draw simple head circle if T
#' @param signed assume that Z is directional (see \code{\link{ltopo.conn}()} notes)
#'
#' @return plot is generated in the current graphics device; no return value
#' @export
#'
#' @examples
#' \dontrun{
#' ltopo.dconn(ch = "C3", chs1 = coh$CH1, chs2 = coh$CH2, z = coh$COH, flt = coh$B == "SIGMA")
#' }
#'
#' @note
#' This is a special case of \code{\link{ltopo.conn}()}, where instead of showing all available pairwise measures,
#' only those where channel A matches the single channel specified by \code{ch} are shown.
ltopo.dconn <- function(ch, chs1, chs2, z, flt = T, zr = NULL, cex = 2, w = 8, title = "", head = T, signed = F) {
  if (is.null(zr)) zr <- range(z[flt], na.rm = T)
  zr <- c(-1, 1) * max(abs(zr))
  print(table(flt))
  chs1 <- chs1[flt]
  chs2 <- chs2[flt]
  z <- z[flt]
  # double entry whatever is left
  dchs1 <- c(chs1, chs2)
  dchs2 <- c(chs2, chs1)
  dz <- c(z, -z)
  inc <- dchs1 == ch
  dchs1 <- dchs1[inc]
  dchs2 <- dchs2[inc]
  dz <- dz[inc]
  print(cbind(dchs1, dchs2, dz))
  # filtering done above, so set flt to 'T' here
  ltopo.conn(chs1 = dchs1, chs2 = dchs2, z = dz, flt = T, zr = zr, cex = cex, w = w, title = title, head = head, signed = signed)
}

##
## TOPO HEATMAP: each point is a lheatmap() object (X/Y/Z plot)
##

#' @importFrom graphics rect text lines points
ltopo.heat2 <- function(c, x, y, z, zlim = NULL,
                        f = rep(T, length(x)),
                        sz = 0.08, cex = 1,
                        col = lturbo(100), lwd = 0.5,
                        ylab = "Frequency (Hz)", xlab = "Time", zlab = "log(power)", mt = "") {
  topo <- ldefault.xy()
  c <- lremap.chs(c)
  f[!c %in% toupper(topo$CH)] <- F
  # z is color depth: scaled 1..100 as requires that col is a 100-element pallete
  if (!is.na(z[1])) {
    if (length(z) != length(x)) stop("is z is specified, must match other long data")
    if (length(col) != 100) stop("requires col is a 100-element palette if z is specified")
    z <- z[f]
    if (is.null(zlim)) zlim <- range(z, na.rm = T)
    zcol <- col[1 + round(99 * ((z - zlim[1]) / (zlim[2] - zlim[1])))]
    col <- zcol
  }
  # also pair down x/y/z/CH to filtered set
  c <- c[f]
  x <- x[f]
  y <- y[f]
  z <- z[f]
  rx <- range(x)
  ry <- range(y)
  # plots
  plot(c(0, 1), c(0, 1), type = "n", axes = F, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  rect(0, 0, 1, 1)
  if (mt != "") text(0.025, 0.05, mt, cex = 0.8, pos = 4)
  lgd.x <- 0.88
  lgd.y <- 0.08
  lines(c(lgd.x, lgd.x + sz), c(lgd.y, lgd.y))
  lines(c(lgd.x, lgd.x), c(lgd.y, lgd.y + sz))
  text(lgd.x, lgd.y, signif(ry[1], 2), cex = 0.5, pos = 2)
  text(lgd.x, lgd.y + sz, signif(ry[2], 2), cex = 0.5, pos = 2)
  text(lgd.x, lgd.y, signif(rx[1], 2), cex = 0.5, pos = 1)
  text(lgd.x + sz, lgd.y, signif(rx[2], 2), cex = 0.5, pos = 1)
  text(lgd.x + sz / 2, lgd.y - 0.05, xlab, cex = 0.5)
  text(lgd.x - 0.05, lgd.y + sz / 2, ylab, cex = 0.5)
  # plot each channel
  for (ch in unique(c)) {
    ch.idx <- toupper(topo$CH) == ch
    px <- topo$X[ch.idx] + 0.5
    py <- topo$Y[ch.idx] + 0.5
    if (length(px) == 1) {
      ch.label <- topo$CH[ch.idx]
      x0 <- px - sz / 2
      x1 <- x0 + sz
      y0 <- py - sz / 2
      y1 <- y0 + sz
      lines(c(x0, x1), c(y0, y0), col = "gray")
      lines(c(x0, x0), c(y0, y1), col = "gray")
      xx <- x[c == ch]
      yy <- y[c == ch]
      zz <- z[c == ch]
      xx <- (xx - rx[1]) / (rx[2] - rx[1])
      yy <- (yy - ry[1]) / (ry[2] - ry[1])
      points(x0 + xx * sz, y0 + yy * sz, pch = ".", cex = cex, col = col[c == ch])
      # lines( x0 + xx * sz , y0 + yy * sz ,lwd=lwd , col=col) # just single color for lines
      text(x0 + 0.8 * sz, y0 + 0.9 * sz, ch.label, cex = 0.5, col = ifelse(col == "black", "blue", "black"))
    }
  }
}

#' Winsorize a vector
#'
#' Winsorizes a vector (sets values greater/less than the specified percentile to that value)
#'
#' @param x a numeric vector to be winsorized
#' @param p percentile, must be less than 0.5 (defaults to 0.05, implying 5th and 95th percentiles)
#'
#' @return winsorized version of \code{x}
#' @export
#'
#' @importFrom stats quantile
lwin <- function(x, p = 0.05) {
  t <- quantile(x, c(p, 1 - p), na.rm = T)
  x[x < t[1]] <- t[1]
  x[x > t[2]] <- t[2]
  x
}


#' LUNA plotting
#'
#' draw a 'topoplot of topoplots'
#'
#' @param c character vector of channel labels for the inner plots
#' @param c2 character vector of channel labels for the outer plots
#' @param z numeric vector of values to plot (by Z-axis color scale)
#' @param zlim set Z range (defaults to observed range)
#' @param f boolean vector: filter of \code{length(z)}
#' @param sz relative size of each inner point (default 0.05)
#' @param sz2 relative size of each outer plot (default 0.05)
#' @param ring.lwd width of ring around each point
#' @param same.cols use the same Z color range for each inner topoplot
#' @param col optional color palette (100-elements)
#' @param zlab optional Z-axis label for legend
#' @param mt optional main title text (default "")
#' @param zeroed boolean value: if T, set Z ranges to be symmetric around 0
#'
#' @return plot is generated in the current graphics device; no return value
#' @export
#'
#' @examples
#' \dontrun{
#' ltopo.topo(c = c(coh$CH1, coh$CH2), c2 = c(coh$CH2, coh$CH1),
#'     z = c(coh$ICOH, -1 * coh$ICOH), f = rep(coh$B == "SIGMA", 2),
#'     sz=0.08, sz2=0.6)
#' }
#'
#' @importFrom graphics rect text points
ltopo.topo <- function(c, c2, z, zlim = NULL,
                       f = rep(T, length(z)),
                       sz = 0.05, sz2 = 0.05,
                       ring.lwd = 1,
                       same.cols = T,
                       col = rbpal,
                       zlab = "", mt = "", zeroed = T) {
  # key inputs: c, c2 and z
  topo <- ldefault.xy()
  topo$CH <- toupper(topo$CH)
  c <- lremap.chs(c)
  c2 <- lremap.chs(c2)

  f[!c %in% toupper(topo$CH)] <- F
  f[!c2 %in% toupper(topo$CH)] <- F

  c <- c[f]
  c2 <- c2[f]
  z <- z[f]
  if (is.null(zlim) & same.cols) zlim <- range(z, na.rm = T)
  zlim2 <- zlim

  # plots
  plot(c(0, 1), c(0, 1), type = "n", axes = F, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  rect(0, 0, 1, 1)
  if (mt != "") text(0.025, 0.05, mt, cex = 0.8, pos = 4)

  # loop over key channels (c)
  for (ch in unique(c)) {
    ch.idx <- topo$CH == ch
    px <- topo$X[ch.idx] + 0.5
    py <- topo$Y[ch.idx] + 0.5
    ch.label <- topo$CH[ch.idx]

    # plot loc. for this key channel: x0-x1 and y0-y1
    x0 <- px - sz / 2
    x1 <- x0 + sz
    y0 <- py - sz / 2
    y1 <- y0 + sz
    # lines( c(x0,x1),c(y0,y0),col="gray"); lines( c(x0,x0),c(y0,y1),col="gray" )
    # get inner values
    zz <- z[c == ch]
    cc <- c2[c == ch]
    px2 <- numeric(0)
    py2 <- numeric(0)
    for (ch2 in cc) {
      ch.idx <- topo$CH == ch2
      px2 <- c(px2, x0 + sz * (topo$X[ch.idx] + 0.5))
      py2 <- c(py2, y0 + sz * (topo$Y[ch.idx] + 0.5))
    }
    if (is.null(zlim)) zlim2 <- range(zz, na.rm = T) else zlim2 <- zlim
    if (zeroed) zlim2 <- c(-1, 1) * max(abs(zlim2))
    points(px2, py2,
      pch = 21, cex = sz2, col = ifelse(cc == ch, "black", "lightgray"), lwd = ring.lwd,
      bg = col[1 + round(100 * ((zz - zlim2[1]) / (zlim2[2] - zlim2[1])))]
    )
  }
}



##############################################################
#
# POPS viewers
#
##############################################################

#' Plot a hypnogram
#'
#' @param ss vector of sleep stages (N1,N2,N3,R,W,?,L)
#' @param cycles vector of NREM cycle codes (NA,1,2,3...)
#' @param times vector of epoch times (default, 0, 30, 60, etc)
#' @param start start time of plot (seconds, default 0)
#' @param stop stop time of plot (seconds, default last epoch)
#'
#' @return none
#' @export
lhypno <- function(ss, cycles = NULL, times = seq( 0 , by = 30 , length.out = length(ss) ) , start = 0 , stop = max(times) ) {
  ss[is.na(ss)] <- "?"
  e <- times / 3600
  sn <- lstgn(ss)
  plot(e, sn, type = "n", lwd = 2, col = "gray", axes = F, ylim = c(-3, 3.5), ylab = "", yaxt = "n", xaxs = "i", xlim=c(start,stop)/3600,xlab = "Time (hrs)")
  # change points
  chgs <- which( ss[ 1:(length(ss)-1)] != ss[2:length(ss) ]  )  
  for (chg in chgs) {
   # do not plot connector if change spans a gap; gap define assuming 30-second eppchs
   if ( ! ( times[chg+1] - times[chg] > 40 ) ) 
     lines( rep( (( times[chg]+times[chg+1])/2 ) / 3600, 2 ) , c( sn[chg] , sn[chg+1] ) , lwd = 2, col = "gray" ) 
  }
  points(e, sn, col = lstgcols(ss), type = "p", cex = 1, pch = 20)
  axis(1) # axis(1, at=seq(0,round(max(e)),2))
  axis(2, 2, "?", col.axis = "black", las = 2)
  axis(2, 1, "W", col.axis = lstgcols("W"), las = 2)
  axis(2, 0, "R", col.axis = lstgcols("R"), las = 2)
  axis(2, -1, "N1", col.axis = lstgcols("N1"), las = 2)
  axis(2, -2, "N2", col.axis = lstgcols("N2"), las = 2)
  axis(2, -3, "N3", col.axis = lstgcols("N3"), las = 2)
  if (!is.null(cycles)) {
    if (length(cycles) != length(ss)) stop("ss and cycles must be same length")
    cc <- unique(cycles)
    cc <- cc[!is.na(cc)]
    odd <- T
    for (i in cc) {
      xc <- range(e[cycles == i & !is.na(cycles)])
      if (odd) {
        rect(xc[1], 3, xc[2], 3.3, col = "orange")
      } else {
        rect(xc[1], 2.7, xc[2], 3, col = "purple")
      }
      odd <- !odd
    }
  }
}

#' Numeric encoding of sleep stage labels
#'
#' @param ss vector of sleep stages (N1,N2,N3,R,W,?,L)
#'
#' @return vector of numerically encoded sleep stages
#' @export
lstgn <- function(ss) {
  ss[ss == "N1" | ss == "NREM1" ] <- -1
  ss[ss == "N2" | ss == "NREM2" ] <- -2
  ss[ss == "N3" | ss == "NREM3" ] <- -3
  ss[ss == "R"  | ss== "REM" ] <- 0
  ss[ss == "W" | ss == "wake" ]  <- 1
  ss[ss == "?" | ss == "L" ] <- 2
  ss[is.na(ss)] <- 2
  as.numeric(ss)
}



lstgn2 <- function(x) {
  x[x == "N1"] <- 3
  x[x == "N2"] <- 4
  x[x == "N3"] <- 5
  x[x == "R" | x == "REM" ] <- 6
  x[x == "W"] <- 2
  x[x == "?" | x == "L" ] <- 1
  x[is.na(x)] <- 1
  as.numeric(x)
}

#' @importFrom graphics image
lstgmat <- function(m) {
  m <- apply(m, 2, lstgn2)
  pal <- lstgcols(c("?", "W", "N1", "N2", "N3", "R"))
  image(m, col = pal)
}


# expect output from SUDS
# mode: 3:  W, NR, R
#       5:  N1, N2, N3, W, R
#       "N1" , etc.. only plot that one thing


# helper for lpp
lf100 <- function(x) {
  t <- numeric()
  if (any(is.na(x))) {
   return(rep(6, 100))
  }
  for (s in rev(order(x))) t <- c(t, rep(s, x[s]))
  t[1:100]
}

#' Plot POPS posterior probabilities
#'
#' @param m data table from POPS (epoch-stratified)
#'
#' @return none
#' @export
#' @importFrom graphics image
lpp <- function(m) {
  e <- m$E
  ne <- max(e)
  # ensure all 5 cols
  if ( !any( names( m ) == "PP_N1" ) ) m$PP_N1 <- 0
  if ( !any( names( m ) == "PP_N2" ) ) m$PP_N2 <- 0
  if ( !any( names( m ) == "PP_N3" ) ) m$PP_N3 <- 0
  if ( !any( names( m ) == "PP_R"  ) ) m$PP_R  <- 0
  if ( !any( names( m ) == "PP_W"  ) ) m$PP_W  <- 0
  m$PP_NA <- as.integer( m$FLAG == -1 )
  h <- m[, c("PP_N1", "PP_N2", "PP_N3", "PP_R", "PP_W", "PP_NA")]
  h[ is.na(h) ] <- 0
  xr <- c(1, ne)
  hh <- matrix(NA, nrow = max(e), ncol = 100)
  yy <- numeric(ne)
  h <- round(as.matrix(h), 2) * 100
  h[h < 0] <- 0
  h[h > 100] <- 100
  hh <- t(apply(h, 1, lf100))
  stgpal <- c(lstgcols("N1"), lstgcols("N2"), lstgcols("N3"), lstgcols("R"), lstgcols("W"), lstgcols("?") )
  image(hh, col = stgpal, xaxt = "n", yaxt = "n", axes = F , breaks=0.5 + ( 0:6) )
}

#' Plot POPS posterior probabilities & hypnogram
#'
#' @param m data table from POPS (epoch-stratified)
#'
#' @return none
#' @export
#' @importFrom graphics par image points
lpp2 <- function(m) {
  par(mfcol = c(2, 1), mar = c(0.2, 0.2, 0.2, 0.2))
  e <- m$E
  ne <- max(e)
  if ( !any( names( m ) == "PP_N1" ) ) m$PP_N1 <- 0
  if ( !any( names( m ) == "PP_N2" ) ) m$PP_N2 <- 0
  if ( !any( names( m ) == "PP_N3" ) ) m$PP_N3 <- 0
  if ( !any( names( m ) == "PP_R"  ) ) m$PP_R  <- 0
  if ( !any( names( m ) == "PP_W"  ) ) m$PP_W  <- 0
  m$PP_NA <- 0
  if ( "FLAG" %in% names(m) ) m$PP_NA <- as.integer( m$FLAG == -1 ) 
  h <- m[, c("PP_N1", "PP_N2", "PP_N3", "PP_R", "PP_W", "PP_NA")]
  h[ is.na(h) ] <- 0 
  xr <- c(1, ne)
  hh <- matrix(NA, nrow = max(e), ncol = 100)
  yy <- numeric(ne)
  h <- round(as.matrix(h), 2) * 100
  h[h < 0] <- 0
  h[h > 100] <- 100
  hh <- t(apply(h, 1, lf100))
  stgpal <- c(lstgcols("N1"), lstgcols("N2"), lstgcols("N3"), lstgcols("R"), lstgcols("W") , lstgcols("?") )
  image(hh, col = stgpal, xaxt = "n", yaxt = "n", axes = F , breaks=0.5 + ( 0:6) ) 
  # next plot
  plot(e, rep(1.5, length(e)), col = lstgcols(m$PRED), pch = "|", ylim = c(0, 2), xlab = "", ylab = "", axes = F, xaxs = "i")
  points(e, rep(0.5, length(e)), col = lstgcols(m$PRIOR), pch = "|")
}


####################################################
##                                                ##
## lsummary.page()                                ##
##                                                ##
####################################################

lsummviz <- function( chs , hypno = T , pdf.file = NULL , cols = 1 )
{
np <- cols + length( chs )
rows <- ceiling( np / cols ) 

if ( ! is.null( pdf.file ) )
pdf( file = pdf.file , width = 12 , height = rows  ) 
par( mfrow = c( rows , cols ) , mar=c(1.5,3,2,2) )
if ( hypno ) {
 k <- leval( "EPOCH align & HYPNO epoch" )
 for (c in 1:cols) lhypno( k$HYPNO$E$STAGE ) 
}
for (ch in chs) {
 k <- leval( paste( "PSD sig=" , ch , " epoch-spectrum dB max=25 min=0.5" , sep="" ) ) 
 d <- k$PSD$CH_E_F
 lheatmap( d$E , d$F , d$PSD , win = 0.05 , mt = ch ) 
}
if ( ! is.null( pdf.file ) ) dev.off()
}



##############################################################
#
# Color palettes
#
##############################################################


#' Generating a turbo palette
#'
#' @param n number of colors to return
#'
#' @return vector of color values, length \code{n}
#' @export
lturbo <- function( n = 100 )
{
  turbo.hex <- c(
    "#30123B", "#321543", "#33184A", "#341B51", "#351E58", "#36215F", "#372466",
    "#38276D", "#392A73", "#3A2D79", "#3B2F80", "#3C3286", "#3D358B", "#3E3891",
    "#3F3B97", "#3F3E9C", "#4040A2", "#4143A7", "#4146AC", "#4249B1", "#424BB5",
    "#434EBA", "#4451BF", "#4454C3", "#4456C7", "#4559CB", "#455CCF", "#455ED3",
    "#4661D6", "#4664DA", "#4666DD", "#4669E0", "#466BE3", "#476EE6", "#4771E9",
    "#4773EB", "#4776EE", "#4778F0", "#477BF2", "#467DF4", "#4680F6", "#4682F8",
    "#4685FA", "#4687FB", "#458AFC", "#458CFD", "#448FFE", "#4391FE", "#4294FF",
    "#4196FF", "#4099FF", "#3E9BFE", "#3D9EFE", "#3BA0FD", "#3AA3FC", "#38A5FB",
    "#37A8FA", "#35ABF8", "#33ADF7", "#31AFF5", "#2FB2F4", "#2EB4F2", "#2CB7F0",
    "#2AB9EE", "#28BCEB", "#27BEE9", "#25C0E7", "#23C3E4", "#22C5E2", "#20C7DF",
    "#1FC9DD", "#1ECBDA", "#1CCDD8", "#1BD0D5", "#1AD2D2", "#1AD4D0", "#19D5CD",
    "#18D7CA", "#18D9C8", "#18DBC5", "#18DDC2", "#18DEC0", "#18E0BD", "#19E2BB",
    "#19E3B9", "#1AE4B6", "#1CE6B4", "#1DE7B2", "#1FE9AF", "#20EAAC", "#22EBAA",
    "#25ECA7", "#27EEA4", "#2AEFA1", "#2CF09E", "#2FF19B", "#32F298", "#35F394",
    "#38F491", "#3CF58E", "#3FF68A", "#43F787", "#46F884", "#4AF880", "#4EF97D",
    "#52FA7A", "#55FA76", "#59FB73", "#5DFC6F", "#61FC6C", "#65FD69", "#69FD66",
    "#6DFE62", "#71FE5F", "#75FE5C", "#79FE59", "#7DFF56", "#80FF53", "#84FF51",
    "#88FF4E", "#8BFF4B", "#8FFF49", "#92FF47", "#96FE44", "#99FE42", "#9CFE40",
    "#9FFD3F", "#A1FD3D", "#A4FC3C", "#A7FC3A", "#A9FB39", "#ACFB38", "#AFFA37",
    "#B1F936", "#B4F836", "#B7F735", "#B9F635", "#BCF534", "#BEF434", "#C1F334",
    "#C3F134", "#C6F034", "#C8EF34", "#CBED34", "#CDEC34", "#D0EA34", "#D2E935",
    "#D4E735", "#D7E535", "#D9E436", "#DBE236", "#DDE037", "#DFDF37", "#E1DD37",
    "#E3DB38", "#E5D938", "#E7D739", "#E9D539", "#EBD339", "#ECD13A", "#EECF3A",
    "#EFCD3A", "#F1CB3A", "#F2C93A", "#F4C73A", "#F5C53A", "#F6C33A", "#F7C13A",
    "#F8BE39", "#F9BC39", "#FABA39", "#FBB838", "#FBB637", "#FCB336", "#FCB136",
    "#FDAE35", "#FDAC34", "#FEA933", "#FEA732", "#FEA431", "#FEA130", "#FE9E2F",
    "#FE9B2D", "#FE992C", "#FE962B", "#FE932A", "#FE9029", "#FD8D27", "#FD8A26",
    "#FC8725", "#FC8423", "#FB8122", "#FB7E21", "#FA7B1F", "#F9781E", "#F9751D",
    "#F8721C", "#F76F1A", "#F66C19", "#F56918", "#F46617", "#F36315", "#F26014",
    "#F15D13", "#F05B12", "#EF5811", "#ED5510", "#EC530F", "#EB500E", "#EA4E0D",
    "#E84B0C", "#E7490C", "#E5470B", "#E4450A", "#E2430A", "#E14109", "#DF3F08",
    "#DD3D08", "#DC3B07", "#DA3907", "#D83706", "#D63506", "#D43305", "#D23105",
    "#D02F05", "#CE2D04", "#CC2B04", "#CA2A04", "#C82803", "#C52603", "#C32503",
    "#C12302", "#BE2102", "#BC2002", "#B91E02", "#B71D02", "#B41B01", "#B21A01",
    "#AF1801", "#AC1701", "#A91601", "#A71401", "#A41301", "#A11201", "#9E1001",
    "#9B0F01", "#980E01", "#950D01", "#920B01", "#8E0A01", "#8B0902", "#880802",
    "#850702", "#810602", "#7E0502", "#7A0403"
  )

  turbo.colors <- grDevices::colorRampPalette(colors = turbo.hex, space = "rgb", interpolate = "spline")

  turbo.colors( n )

}



#' Generating a plasma palette (based on \code{viridis} package)
#'
#' @param n number of colors to return
#'
#' @return vector of color values, length \code{n}
#' @export
lplasma <- function( n = 100 )
{
  plasma.hex <- c(
    "#0D0887FF", "#100788FF", "#130789FF", "#16078AFF", "#19068CFF", "#1B068DFF",
    "#1D068EFF", "#20068FFF", "#220690FF", "#240691FF", "#260591FF", "#280592FF",
    "#2A0593FF", "#2C0594FF", "#2E0595FF", "#2F0596FF", "#310597FF", "#330597FF",
    "#350498FF", "#370499FF", "#38049AFF", "#3A049AFF", "#3C049BFF", "#3E049CFF",
    "#3F049CFF", "#41049DFF", "#43039EFF", "#44039EFF", "#46039FFF", "#48039FFF",
    "#4903A0FF", "#4B03A1FF", "#4C02A1FF", "#4E02A2FF", "#5002A2FF", "#5102A3FF",
    "#5302A3FF", "#5502A4FF", "#5601A4FF", "#5801A4FF", "#5901A5FF", "#5B01A5FF",
    "#5C01A6FF", "#5E01A6FF", "#6001A6FF", "#6100A7FF", "#6300A7FF", "#6400A7FF",
    "#6600A7FF", "#6700A8FF", "#6900A8FF", "#6A00A8FF", "#6C00A8FF", "#6E00A8FF",
    "#6F00A8FF", "#7100A8FF", "#7201A8FF", "#7401A8FF", "#7501A8FF", "#7701A8FF",
    "#7801A8FF", "#7A02A8FF", "#7B02A8FF", "#7D03A8FF", "#7E03A8FF", "#8004A8FF",
    "#8104A7FF", "#8305A7FF", "#8405A7FF", "#8606A6FF", "#8707A6FF", "#8808A6FF",
    "#8A09A5FF", "#8B0AA5FF", "#8D0BA5FF", "#8E0CA4FF", "#8F0DA4FF", "#910EA3FF",
    "#920FA3FF", "#9410A2FF", "#9511A1FF", "#9613A1FF", "#9814A0FF", "#99159FFF",
    "#9A169FFF", "#9C179EFF", "#9D189DFF", "#9E199DFF", "#A01A9CFF", "#A11B9BFF",
    "#A21D9AFF", "#A31E9AFF", "#A51F99FF", "#A62098FF", "#A72197FF", "#A82296FF",
    "#AA2395FF", "#AB2494FF", "#AC2694FF", "#AD2793FF", "#AE2892FF", "#B02991FF",
    "#B12A90FF", "#B22B8FFF", "#B32C8EFF", "#B42E8DFF", "#B52F8CFF", "#B6308BFF",
    "#B7318AFF", "#B83289FF", "#BA3388FF", "#BB3488FF", "#BC3587FF", "#BD3786FF",
    "#BE3885FF", "#BF3984FF", "#C03A83FF", "#C13B82FF", "#C23C81FF", "#C33D80FF",
    "#C43E7FFF", "#C5407EFF", "#C6417DFF", "#C7427CFF", "#C8437BFF", "#C9447AFF",
    "#CA457AFF", "#CB4679FF", "#CC4778FF", "#CC4977FF", "#CD4A76FF", "#CE4B75FF",
    "#CF4C74FF", "#D04D73FF", "#D14E72FF", "#D24F71FF", "#D35171FF", "#D45270FF",
    "#D5536FFF", "#D5546EFF", "#D6556DFF", "#D7566CFF", "#D8576BFF", "#D9586AFF",
    "#DA5A6AFF", "#DA5B69FF", "#DB5C68FF", "#DC5D67FF", "#DD5E66FF", "#DE5F65FF",
    "#DE6164FF", "#DF6263FF", "#E06363FF", "#E16462FF", "#E26561FF", "#E26660FF",
    "#E3685FFF", "#E4695EFF", "#E56A5DFF", "#E56B5DFF", "#E66C5CFF", "#E76E5BFF",
    "#E76F5AFF", "#E87059FF", "#E97158FF", "#E97257FF", "#EA7457FF", "#EB7556FF",
    "#EB7655FF", "#EC7754FF", "#ED7953FF", "#ED7A52FF", "#EE7B51FF", "#EF7C51FF",
    "#EF7E50FF", "#F07F4FFF", "#F0804EFF", "#F1814DFF", "#F1834CFF", "#F2844BFF",
    "#F3854BFF", "#F3874AFF", "#F48849FF", "#F48948FF", "#F58B47FF", "#F58C46FF",
    "#F68D45FF", "#F68F44FF", "#F79044FF", "#F79143FF", "#F79342FF", "#F89441FF",
    "#F89540FF", "#F9973FFF", "#F9983EFF", "#F99A3EFF", "#FA9B3DFF", "#FA9C3CFF",
    "#FA9E3BFF", "#FB9F3AFF", "#FBA139FF", "#FBA238FF", "#FCA338FF", "#FCA537FF",
    "#FCA636FF", "#FCA835FF", "#FCA934FF", "#FDAB33FF", "#FDAC33FF", "#FDAE32FF",
    "#FDAF31FF", "#FDB130FF", "#FDB22FFF", "#FDB42FFF", "#FDB52EFF", "#FEB72DFF",
    "#FEB82CFF", "#FEBA2CFF", "#FEBB2BFF", "#FEBD2AFF", "#FEBE2AFF", "#FEC029FF",
    "#FDC229FF", "#FDC328FF", "#FDC527FF", "#FDC627FF", "#FDC827FF", "#FDCA26FF",
    "#FDCB26FF", "#FCCD25FF", "#FCCE25FF", "#FCD025FF", "#FCD225FF", "#FBD324FF",
    "#FBD524FF", "#FBD724FF", "#FAD824FF", "#FADA24FF", "#F9DC24FF", "#F9DD25FF",
    "#F8DF25FF", "#F8E125FF", "#F7E225FF", "#F7E425FF", "#F6E626FF", "#F6E826FF",
    "#F5E926FF", "#F5EB27FF", "#F4ED27FF", "#F3EE27FF", "#F3F027FF", "#F2F227FF",
    "#F1F426FF", "#F1F525FF", "#F0F724FF", "#F0F921FF"
  )

  plasma.colors <- grDevices::colorRampPalette(colors = plasma.hex, space = "rgb", interpolate = "spline")

  plasma.colors( n )

}



