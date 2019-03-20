
####################################################
##                                                ##
## lunaR                                          ##
##                                                ##
####################################################

luna.globals <- new.env()

luna.globals$version  <- "v0.2"
luna.globals$date     <- "17-Mar-2019"
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

lset <- function( var , val = NULL ) { 
if ( ! is.null(val) ) { 
 .Call( "Rset_var" , as.character(var) , as.character(val) , PACKAGE="luna" )
} else if ( is.list(var) ) {
 v <- unlist(var)
 for (i in 1:length(v)) .Call( "Rset_var" , as.character(names(v)[i]) , as.character(v[i]) , PACKAGE="luna")
} else { # assume this is a file
 d <- read.table( var , sep="\t" , header=F , stringsAsFactors = F)
 if ( dim(d)[2] != 2 ) stop( paste( "expecting two tab-delimited columns in" , v ) )
 n <- dim(d)[1]
 for (i in 1:n) .Call( "Rset_var" , as.character(d[i,1]) , as.character(d[i,2]) , PACKAGE="luna")
}
 invisible(1)
} 

lvar <- function(v) {
 .Call( "Rshow_var" , as.character(v) , PACKAGE="luna" )
}

lclear <- function(v) {
 .Call( "Rset_var" , as.character(v) , NULL , PACKAGE="luna" )
 invisible(1)
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

letable <- function( annots = character(0) ) {
 .Call("Rmask" , as.character(annots) , PACKAGE = "luna" );
}

ladd.annot.file <- function( a )
{
.Call("Radd_annot" , as.character(a) , PACKAGE = "luna" ); 	
invisible(1)
}

ladd.annot <- function( annot , intervals )
{
 if ( ! is.list( intervals ) ) stop( "expecting a list of intervals" ) 
 # check that each have two of each, convert to vector
 t <- unlist( intervals ) 
 if ( length(t) != 2 * length(intervals) ) stop( "bad interval list format: expecting two items per list element" ) 
 if ( length(t) != 0 ) .Call("Radd_annot_fromR" , as.character(annot) , as.numeric(t) , PACKAGE = "luna" ); 	
 lstat()
 invisible(1)
}

le2i <- function( e , dur = 30 , inc = 30 )
{
  mapply( c , (e-1)*inc , (e-1)*inc+dur , SIMPLIFY = F )
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

lreturnless_eval <- function( x )
{
 # we apply this to whomever is attached
 xx <- paste0( x, collapse = " & " )  
 .Call("Reval_cmd_noreturns", as.character(xx) , PACKAGE = "luna" )
 invisible(1)
}

leval.project <- function( sl , x )
{
 if ( missing(sl) ) stop( "no sample list 'sl' specified" )
 if ( missing(x) ) stop( "no Luna commands 'x' specified" )  
 ids <- names(sl)
 if ( length(ids) == 0 ) stop( "no individuals in sample-list" )
 .Call("Reval_init_returns" , PACKAGE = "luna" )
 for (id in ids) { 
     lattach(sl,id)
     lreturnless_eval( x )
 }
 ldrop()
 .Call("Reval_get_returns",PACKAGE="luna")
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
   if (	f != ""	) 
      lst[[cmd]][[f]]
   else if ( length(lst[[cmd]])==1 )
      lst[[cmd]][[1]]
   else		
      lst[[cmd]]	
}

lx2 <- function( k , ... ) { do.call( rbind , lapply( k , lx , ... ) ) }

lid <- function(d,id) { d[ d$ID %in% id , ] } 

lstages <- function() { 
 leval( "STAGE" )$STAGE$E$STAGE
}

lcmd <- function(filename) {
 lines <- readLines( filename , warn = F )
 lines <- lines[ which( lines != "" ) ]
 # delete '%' comments
 for (i in 1:length(lines)) lines[i] <- gsub( "%.*","",lines[i] )
 # append lines		    starting with a   space to the previous one
 prv_line = ""
 newlines <- character(0)
 for (i in 1:length(lines))
 {
  if ( substr( lines[i] ,1,1 ) == " " ) {
     if ( prv_line == "" ) stop( "badly formed command line continuation" )
     prv_line =	   paste( prv_line	, lines[i] )
  } else {
    if ( prv_line != ""	) newlines <- c( newlines , prv_line )
    prv_line = lines[i]
  }
  }
  if ( prv_line	!= "" ) newlines <- c( newlines	, prv_line )
 newlines
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


####################################################
##                                                ##
## Statistical helper functions                   ##
##                                                ##
####################################################


loutliers <- function(x,m =mean(x,na.rm=T) , sdev = sd(x,na.rm=T) ,t=3)
{
 lwr <- m - t * sdev
 upr <- m + t * sdev
 x[ x < lwr ] <- NA
 x[ x > upr ] <- NA
 x
}


ldenoise <- function(x,lambda) { 
 .Call( "R1d_denoise" , as.numeric(x) , as.numeric(lambda) , PACKAGE = "luna" )
} 


####################################################
##                                                ##
## Visualization                                  ##
##                                                ##
####################################################


lheatmap <- function(x,y,z) {
 # assumes a square matrix 
 nx <- length(unique(x))
 ny <- length(unique(y))
 nz <- length(z)
 if ( nz != nx * ny ) stop( "requires square data" )
 d <- data.frame( x,y,z )
 d <- d[ order(d$y,d$x) , ]
 m <- matrix( d$z , byrow = T , nrow = ny , ncol = nx )
 hmcols<-colorRampPalette(rev(c("red","orange","yellow","cyan","blue")))(100)
 image(t(m[1:ny,]),col=hmcols,xaxt='n',yaxt='n')
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


