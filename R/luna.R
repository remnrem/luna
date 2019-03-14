
####################################################
##                                                ##
## lunaR                                          ##
##                                                ##
####################################################

luna.globals <- new.env()

luna.globals$version  <- "v0.9"
luna.globals$date     <- "13-Mar-2019"
luna.globals$id       <- ""
luna.globals$edf      <- ""
luna.globals$annots   <- ""
luna.globals$logmode  <- 0

####################################################
##                                                ##
## Initialize                                     ##
##                                                ##
####################################################

.onLoad <- function(libname, pkgname) 
{
  packageStartupMessage( paste( "** lunaR" , luna.globals$version , luna.globals$date ) )
  library.dynam("luna", package="luna", lib.loc = NULL)  
  luna.globals$logmode <- 0 
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
 luna.globals$edf <- as.character(edf) 
 luna.globals$id <- as.character(id)
 luna.globals$annots <- as.character(annots)
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
 luna.globals$logmode <- as.logical(x)
 .Call("Rlogmode" , as.integer(x) , PACKAGE = "luna" );
 invisible(1)
}

lflush <- function()
{
 if ( luna.globals$logmode ) .Call("Rflush_log" , PACKAGE="luna" )
 invisible( luna.globals$logmode )
}

lepoch <- function( dur = 30 , inc = -1 ) 
{
 if ( inc <= 0 ) inc = dur; 
 k <- leval( paste( "EPOCH" , paste("dur",dur,sep="=") , paste("inc",inc,sep="=") ) )  
 invisible( k$EPOCH$BL$NE )
}

ladd.annot <- function( a )
{
.Call("Radd_annot" , as.character(a) , PACKAGE = "luna" ); 	
invisible(1)
}

letable <- function( annots = character(0) ) {
 .Call("Rmask" , as.character(annots) , PACKAGE = "luna" );
}

lannots <- function( a = "" ) {
if ( a == "" ) return( .Call("Rannots" , PACKAGE = "luna" ) );
if ( length(a) != 1 ) stop( "lannots( annot ) only takes one annotation class" )
.Call("Rannot" , as.character( a ) , PACKAGE = "luna" );
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
if ( luna.globals$edf == "" ) stop( "no EDF yet attached" )
lreset() # clears any 'problem' flag set
ledf( luna.globals$edf , luna.globals$id , luna.globals$annots )
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

lprob <- function() 
{ 
 .Call("Rproblem" , PACKAGE = "luna" )
}

lreset <- function( state = 0 ) 
{ 
 .Call("Rsetproblem" , as.integer(state) , PACKAGE = "luna" )
}


####################################################
##                                                ##
## Attach a destrat database                      ##
##                                                ##
####################################################

ldb <- function( dbfile , ids = character(0) )
{	
 .Call("Rdb2retval", as.character(dbfile) , as.character(ids) , PACKAGE = "luna" );
} 


####################################################
##                                                ##
## Iterate epoch-wise or annot-wise, applying     ## 
## user-defined function                          ##
##                                                ##
####################################################

literate <- function( func , chs = character(0) , annots = character(0) , by.annot = character(0) , w = 0 , env = new.env() )
{
 tmp <- .Call( "Riterate" , as.function(func) , as.character(chs) , 
     	       as.character(annots) , as.character(by.annot) , as.numeric(w) , 
	       env , PACKAGE = "luna" )
 invisible(tmp)
}


####################################################
##                                                ##
## Pull raw signals/annotations                   ##
##                                                ##
####################################################


ldata <- function( e , chs , annots = character(0) ) 
{
 .Call( "Rmatrix_epochs", as.integer(e) , as.character(chs) , as.character(annots) , PACKAGE = 'luna' )
}

ldata.intervals <- function( i , chs , annots = character(0) , w = 0 ) 
{
   if ( ! is.list(i) ) stop( "expecting a list() for i" )
   if ( w > 0 ) i <- lapply( i , function(x) { c( max(0,x[1]-w),x[2]+w) } )   
   if ( length(i) != 0 ) .Call( "Rmatrix_intervals", as.numeric(unlist(i)) , as.character(chs) , as.character(annots) , PACKAGE = 'luna' )
   else stop("no intervals found")
}



####################################################
##                                                ##
## Misc, helper functions                         ##
##                                                ##
####################################################

lsanitize <- function(s) { gsub( "[^[:alnum:]]" , "_" , s ) }

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

lstgcols <- function(s) {
 as.vector( sapply( s , function(x) {
  ifelse( x == "NREM1" , rgb(0,190,250,255,max=255) ,
   ifelse( x == "NREM2" , rgb(0,80,200,255,max=255) ,
    ifelse( x == "NREM3" , rgb(0,0,80,255,max=255) ,
     ifelse( x == "NREM4" , rgb(0,0,50,255,max=255) ,
      ifelse( x == "REM"   , rgb(250,20,50,255,max=255) ,
       ifelse( x == "wake"  , rgb(100,100,100,255,max=255) ,
         rgb( 20,160,20,100,max=255) ) ) ) ) ) ) } ) )
}


#lshape <- function( d , r , c )  
#{      	  
#   rs <- paste0( r , collapse="+" )
#   cs <- paste0( c , collapse="+" )
#   f <- as.formula( paste( rs , "~" , cs ) )
#   vals <- names(d)[ ! names(d) %in% c(r,c) ]
#   k <- as.data.frame( dcast( setDT(d) , f , value.var = vals ) )
#   k
#}


loutliers <- function(x,m =mean(x,na.rm=T) , sdev = sd(x,na.rm=T) ,t=3)
{
 lwr <- m - t * sdev
 upr <- m + t * sdev
 x[ x < lwr ] <- NA
 x[ x > upr ] <- NA
 x
}

####################################################
##                                                ##
## Visualization                                  ##
##                                                ##
####################################################


lheatmap <- function(x,y,z) {
 #z <- loutliers( z , 3 )
# assumes square
 nx <- length(unique(x))
 ny <- length(unique(y))
 nz <- length(z)
 if ( nz != nx * ny ) stop( "requires square data" )
 d <- data.frame( x,y,z )
 d <- d[ order(d$y,d$x) , ]
 m <- matrix( d$z , byrow = T , nrow = ny , ncol = nx )
 hmcols<-colorRampPalette(rev(c("red","orange","yellow","cyan","blue")))(100)
 image(t(m[1:ny,]),col=hmcols)
}


## Signal viewer (for use w/ ldata() or literate())
lsigview <- function(x) { 
# INT [E] SEC { SIGS }
if ( names(x)[2] == "E" ) { sigs <- names(x)[-(1:3)] } else { sigs <- names(x)[-(1:2)] } 
nsigs <- length(sigs)
# labels
int.label <- x$INT[1]
e.label <- ifelse( names(x)[2] == "E" , x$E[1] , "." ) 
t.label <- x$SEC
# simple 0..N Y-scale, with each signal has unit 1 space

readline()
} 

