
####################################################
##                                                ##
## lunaR                                          ##
##                                                ##
####################################################

lunaR.version <- "v0.2"

lunaR.date    <- "1-Mar-2019"


####################################################
##                                                ##
## Initialize                                     ##
##                                                ##
####################################################

.onLoad <- function(libname, pkgname) 
{
  packageStartupMessage( paste( "** lunaR" , lunaR.version , lunaR.date ) )
  library.dynam("luna", package="luna", lib.loc = NULL)
  luna.id <<- luna.edf <<- luna.annots <<- ""
  luna.logmode <<- 0 
}


.onUnload <- function (libpath) {
  library.dynam.unload("luna", libpath)
}



####################################################
##                                                ##
## Work with sample lists                         ##
##                                                ##
####################################################


lsl <- function( file , path = "" ) { 
d <- read.table(file,header=F,fill=T,sep="\t",stringsAsFactors=F) 
if ( dim(d)[2] < 2 ) stop("invalid sample list")
cat( dim(d)[1],"observations in",file,"\n")
if ( dim(d)[1] > length(unique(d[,1])) ) stop("duplicate IDs found")
l <- list()
for (i in 1:dim(d)[1]) {  
 l[[ d[i,1] ]]$EDF <- ifelse( path=="" , d[i,2] , paste(path,d[i,2],sep="/") )  
 a <- d[i,-c(1:2)] ; a <- a[ a != "" ]
 if ( path != "" ) a <- sapply( a , function(x) paste( path,x,sep="/" ) )
 l[[ d[i,1] ]]$ANNOT <- as.character(a)  
 }
l
}

lattach <- function(sl,idx="")  {
  id <- idx
  if ( is.numeric(idx) ) {
  if ( idx < 1 | idx > length(sl) ) stop( paste("idx out of range, expecting 1 .." , length(sl) ) )
  id = names(sl)[idx] 
 } else { 
  idx <- which( names(sl) == id )	   
  if ( length( idx ) != 1 ) stop( paste("could not find index",idx))
 }
ledf( sl[[idx]]$EDF , id , sl[[idx]]$ANNOT )
}


lparam <- function( v = "" ) { 
if ( v=="" ) { .Call( "Rclear_vars" , PACKAGE="luna" )
} else if ( is.list(v) ) { 
 v <- unlist(v)
 for (i in 1:length(v)) .Call( "Rset_var" , as.character(names(v)[i]) , as.character(v[i]) , PACKAGE="luna")
} else { # assume this is a file
 d <- read.table( v , sep="\t" , header=F , stringsAsFactors = F)
 if ( dim(d)[2] != 2 ) stop( paste( "expecting two tab-delimited columns in" , v ) )
 n <- dim(d)[1]
 for (i in 1:n) .Call( "Rset_var" , as.character(d[i,1]) , as.character(d[i,2]) , PACKAGE="luna")
}
} 


####################################################
##                                                ##
## Attach an EDF                                  ##
##                                                ##
####################################################


ledf <- function( edf , id = "." , annots = character(0) )
{
 ldrop()
 # EDF, ID, annotations
 .Call("Rattach_edf", as.character(edf) , as.character(id) , as.character(annots) , PACKAGE = "luna" );
 lflush()
 lstat()
 luna.edf <<- as.character(edf); luna.id <<- as.character(id); luna.annots <<- as.character(annots)
 invisible(1)
} 

## report on the in-memory EDF what is in memory
lstat <- function() { 
 invisible( .Call("Rstat" , PACKAGE = "luna" ) );
}

## report on the in-memory EDF what is in memory
ldesc <- function() { 
 .Call("Rdesc" , PACKAGE = "luna" ) ;
}

llog <- function(x) { 
 if ( length(x) != 1 ) stop( "expecting a single 0/1" )
 if ( ! is.numeric(x) ) stop( "expecting a single 0/1" )
 luna.logmode <<- as.logical(x)
 .Call("Rlogmode" , as.integer(x) , PACKAGE = "luna" );
 invisible(1)
}

lflush <- function()
{
 if ( luna.logmode ) .Call("Rflush_log" , PACKAGE="luna" )
 invisible( luna.logmode )
}

lepoch <- function( dur = 30 , inc = -1 ) 
{
 if ( inc <= 0 ) inc = dur; 
 k <- leval( paste( "EPOCH" , paste("dur",dur,sep="=") , paste("inc",inc,sep="=") ) )  
 invisible( k$EPOCH$BL$NE )
}

letable <- function( annots = character(0) ) {
 .Call("Rmask" , as.character(annots) , PACKAGE = "luna" );
}

lannots <- function() {
 .Call("Rannots" , PACKAGE = "luna" );
}

lchs <- function() { 
 .Call("Rchannels" , PACKAGE = "luna" );
}

ldrop <- function() {
 .Call( "Rclear" , PACKAGE = "luna" );
 invisible(1)
} 


lrefresh <- function()
{
if ( luna.edf == "" ) stop( "no EDF yet attached" )
ledf( luna.edf , luna.id , luna.annots )
}

####################################################
##                                                ##
## Evaluate a (set of) command(s)                 ##
##                                                ##
####################################################

leval <- function( x )
{	
 xx <- paste0( x, collapse = " & " )  
 retval <- .Call("Reval_cmd", as.character(xx) , PACKAGE = "luna" )
 lflush()
 lstat()
 invisible(retval)
} 


####################################################
##                                                ##
## Attach a destrat database                      ##
##                                                ##
####################################################

ldb <- function( dbfile , indiv_id = "" )
{	
 .Call("Rdb2retval", as.character(dbfile) , as.character(indiv_id) , PACKAGE = "luna" );
} 


####################################################
##                                                ##
## Iterate epoch-wise, applying a function        ##
##                                                ##
####################################################

literate <- function( func , chs = lchs() , annots = lannots() )
{
 tmp <- .Call( "Riterate" , as.function(func) , as.character(chs) , as.character(annots) , new.env() , PACKAGE = "luna" )
 invisible(tmp)
}


####################################################
##                                                ##
## Pull raw signals/annotations                   ##
##                                                ##
####################################################


ldata <- function( e , chs , annots = character(0) ) 
{
 .Call( "Rmatrix", as.integer(e) , as.character(chs) , as.character(annots) , PACKAGE = 'luna' )
}




####################################################
##                                                ##
## Misc, helper functions                         ##
##                                                ##
####################################################


lstrat <- function( lst , cmd = "" )
{
   if ( cmd == "" )
   {
        n <- names(lst)
        for (i in n) cat( i , ":" , lstrat( lst	, i ) ,	"\n" , sep=" " )
   }
   else
   {
        t <- names(lst[[cmd]])
   }
}


lx <- function( lst , cmd = "" , f = "" , ... )
{
   if ( cmd == "" ) return(lstrat(lst))
   f <- paste(sort( unlist( c( f , list(...) )  )  ), sep="" , collapse="_" )
   if (	f != ""	    )
      lst[[cmd]][[f]]
   else 
      lst[[cmd]]
}
