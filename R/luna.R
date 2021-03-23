
####################################################
##                                                ##
## lunaR                                          ##
##                                                ##
####################################################

luna.globals <- new.env()

luna.globals$version  <- "v0.25.2"
luna.globals$date     <- "23-Feb-2021"
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
  luna.globals$xy       <- ldefault.xy()
  luna.globals$xy.coh   <- ldefault.coh.xy( luna.globals$xy )
  require( plotrix )
  require( geosphere )
  require( matlab )
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
 for (i in 1:n)   .Call( "Rset_var" , as.character(d[i,1]) , as.character(d[i,2]) , PACKAGE="luna")
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

lreset <- function() { 
 ldrop()
 .Call( "Rclear_vars" , PACKAGE = "luna" );
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
if ( ! file.exists( a ) ) stop( paste( "cannot find" , a ) ) 
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
 .Call( "Rdrop" , PACKAGE = "luna" );
 invisible(1)
} 

lrefresh <- function()
{
if ( luna.globals$edf == "" ) stop( "no EDF yet attached" )
lprob_clear() # clears any 'problem' flag set
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

lprob_clear <- function( state = 0 ) 
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
## Load a text-table                              ##
##                                                ##
####################################################

ltxttab <- function( root , f = "" , ids = dir( root ) , silent = F )  
{

  if ( f == "" ) return( ltxttab.dir( root , ids ) )

  # root : folder root
  files <- paste( root , "/" , ids , "/" , f , sep="" )  

  cnt = 1
  for (file in files) {
    if ( ! silent ) cat("reading" , file , "\n" )
    if ( cnt == 1 ) d <- read.table( file , header = T , stringsAsFactors = F ) 
    else d <- rbind( d , read.table( file , header = T , stringsAsFactors = F ) )
    cnt <- cnt + 1
  }
d
} 

ltxttab.dir <- function( root , ids = dir( root ) ) 
{
  folders <- paste( root , "/" , ids , "/" , sep="" )	
  r <- character(0)
  for (folder in folders) r <- c( r , dir( folder ) )  
  table( r ) 
}


####################################################
##                                                ##
## Iterate epoch-wise or annot-wise, applying     ## 
## user-defined function                          ##
##                                                ##
####################################################

literate <- function( func , chs = character(0) , annots = character(0) , 
	              by.annot = character(0) , w = 0 , env = new.env() )
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

# nb. NREM4->N3 in color scheme
lstgcols <- function(s) {
 as.vector( sapply( s , function(x) {
  ifelse( x == "NREM1" | x == "N1" , rgb(0,190,250,255,maxColorValue=255) ,
   ifelse( x == "NREM2" | x == "N2" , rgb(0,80,200,255,maxColorValue=255) ,
    ifelse( x == "NREM3" | x == "N3" , rgb(0,0,80,255,maxColorValue=255) ,
     ifelse( x == "NREM4" | x == "N3" , rgb(0,0,50,255,maxColorValue=255) ,
      ifelse( x == "REM" | x == "R"  , rgb(250,20,50,255,maxColorValue=255) ,
       ifelse( x == "wake" | x == "W" , rgb(100,100,100,255,maxColorValue=255) ,
         rgb( 20,160,20,100,maxColorValue=255) ) ) ) ) ) ) } ) )
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



lbands <- function( l ) { 
invisible(capture.output({ leval( paste( "COPY sig=",l, " tag=delta" , sep="" ) )
leval( paste( "COPY sig=",l, " tag=theta" , sep="" ) )
leval( paste( "COPY sig=",l, " tag=alpha" , sep="" ) )
leval( paste( "COPY sig=",l, " tag=sigma" , sep="" ) )
leval( paste( "COPY sig=",l, " tag=beta" , sep="" ) )
leval( paste( "FILTER sig=",l,"_delta bandpass=0.5,4 tw=1 ripple=0.02" , sep="" ) )
leval( paste( "FILTER sig=",l,"_theta bandpass=4,8   tw=1 ripple=0.02" , sep="" ) )
leval( paste( "FILTER sig=",l,"_alpha bandpass=8,12  tw=1 ripple=0.02" , sep="" ) )
leval( paste( "FILTER sig=",l,"_sigma bandpass=12,15 tw=1 ripple=0.02" , sep="" ) )
leval( paste( "FILTER sig=",l,"_beta  bandpass=15,30 tw=1 ripple=0.02" , sep="" ) )
})) }



####################################################
##                                                ##
## Visualization                                  ##
##                                                ##
####################################################


lheatmap <- function(x,y,z,
            col = colorRampPalette(rev(c("red","orange","yellow","cyan","blue")))(100) ,
	    mt = "" ,
	    zlim = range(z) ) {
 # assumes a square matrix 
 nx <- length(unique(x))
 ny <- length(unique(y))
 nz <- length(z)
 if ( nz != nx * ny ) stop( "requires square data" )
 d <- data.frame( x,y,z )
 d <- d[ order(d$y,d$x) , ]
 m <- matrix( d$z , byrow = T , nrow = ny , ncol = nx )
 image(t(m[1:ny,]),col=col,xaxt='n',yaxt='n',main=mt,zlim=zlim)
}






## --------------------------------------------------------------------------------
##
## Standard 64-channel 2D coords
##
## --------------------------------------------------------------------------------


lremap.chs <- function( chs ) {
chs <- toupper( chs )
chs[ chs == "T3" ] <- "T7"
chs[ chs == "T4" ] <- "T8"
chs[ chs == "T5" ] <- "P7"
chs[ chs == "T6" ] <- "P8"
chs
}

ldefault.xy <- function( chs = character(0) ) { 

chlab <- c( "Fp1", "AF7", "AF3", "F1",  "F3",  "F5",  "F7",  "FT7", "FC5", "FC3", "FC1", "C1",
 "C3",  "C5",  "T7",  "TP7", "CP5", "CP3", "CP1", "P1",  "P3",  "P5",  "P7",  "P9", 
 "PO7", "PO3", "O1",  "Iz",  "Oz",  "POz", "Pz",  "CPz", "Fpz", "Fp2", "AF8", "AF4",
 "AFz", "Fz",  "F2",  "F4",  "F6",  "F8",  "FT8", "FC6", "FC4", "FC2", "FCz", "Cz", 
 "C2",  "C4",  "C6",  "T8",  "TP8", "CP6", "CP4", "CP2", "P2",  "P4",  "P6",  "P8", 
 "P10", "PO8", "PO4", "O2" )
 
chx <- c( -0.139058, -0.264503, -0.152969, -0.091616, -0.184692, -0.276864, -0.364058,
 -0.427975, -0.328783, -0.215938, -0.110678, -0.112500, -0.225000, -0.337500,
 -0.450000, -0.427975, -0.328783, -0.215938, -0.110678, -0.091616, -0.184692,
 -0.276864, -0.364058, -0.430900, -0.264503, -0.152969, -0.139058,  0.000000,
  0.000000,  0.000000,  0.000000,  0.000000,  0.000000,  0.139058,  0.264503,
  0.152969,  0.000000,  0.000000,  0.091616,  0.184692,  0.276864,  0.364058,
  0.427975,  0.328783,  0.215938,  0.110678,  0.000000,  0.000000,  0.112500,
  0.225000,  0.337500,  0.450000,  0.427975,  0.328783,  0.215938,  0.110678,
  0.091616,  0.184692,  0.276864,  0.364058,  0.430900,  0.264503,  0.152969,
  0.139058 )

chy <- c( 0.430423,  0.373607,  0.341595,  0.251562,  0.252734,  0.263932,  0.285114,
  0.173607,  0.162185,  0.152059,  0.148380,  0.050000,  0.050000,  0.050000,
  0.050000, -0.073607, -0.062185, -0.052059, -0.048380, -0.151562, -0.152734,
 -0.163932, -0.185114, -0.271394, -0.273607, -0.241595, -0.330422, -0.450000,
 -0.350000, -0.250000, -0.150000, -0.050000,  0.450000,  0.430423,  0.373607,
  0.341595,  0.350000,  0.250000,  0.251562,  0.252734,  0.263932,  0.285114,
  0.173607,  0.162185,  0.152059,  0.148380,  0.150000,  0.050000,  0.050000,
  0.050000,  0.050000,  0.050000, -0.073607, -0.062185, -0.052059, -0.048380,
 -0.151562, -0.152734, -0.163932, -0.185114, -0.271394, -0.273607, -0.241595,
 -0.330422 )

chxy <- data.frame( CH = toupper( chlab ) , X = chx , Y = chy )

if ( length( chs ) == 0 )  return(chxy)
chxy <- chxy[ chxy$CH %in% toupper( chs ) , ] 
return(chxy)
}


## --------------------------------------------------------------------------------
## 
## Plot generic X/Y line plots (e.g. power spectra) with  
##
## --------------------------------------------------------------------------------

ltopo.xy <- function( c , x , y , z = NA , zlim = NA , 
	 	      f = rep(T, length(x) ) , y.symm = F , 
	       	       	 sz = 0.08 ,
			 col = "black" , lwd = 0.5 ,
			 xline = numeric() ,
			 yline = numeric() ,
			 pch = NA , cex = 1 , 
 	       		 xlab="Frequency (Hz)" , ylab = "log(power)" , mt="" ) { 
topo <- ldefault.xy()
c <- lremap.chs( c )
f[ ! c %in% toupper( topo$CH ) ] <- F 
# z is color depth: scaled 1..100 as requires that col is a 100-element pallete
if ( ! is.na( z[1] ) ) {
if ( length(z) != length(x) ) stop( "is z is specified, must match other long data" )
if ( is.na(pch) ) stop( "cannot specify z with line points" )
if ( length(col) != 100 ) stop( "requires col is a 100-element palette if z is specified" )
z <- z[f]
if (is.na(zlim[1])) zlim <- range( z , na.rm=T) 
zcol = col[ 1 + round( 99 * ( ( z - zlim[1] ) / ( zlim[2] - zlim[1] ) ) ) ] 
col = zcol
} else if ( ! is.na( pch ) ) {
# single color for point plots
col = rep( col , length(x) )
}
# also pair down x/y/CH to filtered set
c <- c[f]; x <- x[f]; y <- y[f] 
# ranges (and symmetric Y?)
rx <- range( x ); ry <- range( y ) 
if ( y.symm ) ry <- c( - max(abs(ry)) , max(abs(ry) ) ) 
# pltos
plot( c(0,1),c(0,1) , type="n" , axes = F , xlab="", ylab="" , xaxt='n' , yaxt='n' )
rect(0,0,1,1)
#draw.circle( 0.5,0.5,0.5 ) 
if (mt!="") text(0.025,0.05,mt,cex=0.8,pos=4)
lgd.x <- 0.88 ; lgd.y <- 0.08
lines( c(lgd.x,lgd.x+sz),c(lgd.y,lgd.y) ) ; lines( c(lgd.x,lgd.x) , c(lgd.y,lgd.y+sz) ) 
text( lgd.x , lgd.y , signif(ry[1],2) , cex=0.5 , pos=2) ; text( lgd.x , lgd.y+sz , signif(ry[2],2) , cex=0.5 , pos=2) 
text( lgd.x , lgd.y , signif(rx[1],2) , cex=0.5 , pos=1) ; text( lgd.x+sz , lgd.y , signif(rx[2],2) , cex=0.5 , pos=1) 
text( lgd.x+sz/2,lgd.y-0.05 , xlab , cex=0.5 )  ; text( lgd.x-0.05,lgd.y+sz/2 , ylab , cex=0.5 ) 
# plot each channel
for ( ch in unique( c ) ) { 
 ch.idx <- toupper( topo$CH ) == ch 
 px <- topo$X[ ch.idx ] + 0.5
 py <- topo$Y[ ch.idx ] + 0.5
 if ( length(px) == 1 ) { 
 ch.label <- topo$CH[ ch.idx ]
 x0 <- px - sz/2 ; x1 <- x0 + sz 
 y0 <- py - sz/2 ; y1 <- y0 + sz 
 lines( c(x0,x1),c(y0,y0),col="gray"); lines( c(x0,x0),c(y0,y1),col="gray" )
 xx <- x[ c == ch ] 
 yy <- y[ c == ch ] 
 xx <- ( xx - rx[1] ) / ( rx[2] - rx[1] )
 yy <- ( yy - ry[1] ) / ( ry[2] - ry[1] )
 for ( xl in xline ) lines( rep( x0+sz*(xl-rx[1])/(rx[2]-rx[1]) , 2 ) , c(y0,y1) , col="gray",lwd=0.5)
 for ( yl in yline ) lines( c(x0,x1) , rep( y0+sz*(yl-ry[1])/(ry[2]-ry[1]) , 2 ) , col="gray",lwd=0.5)
 if ( ! is.na( pch ) ) points( x0 + xx * sz , y0 + yy * sz ,pch = pch , cex = cex , col=col[ c == ch ] )
 else lines( x0 + xx * sz , y0 + yy * sz ,lwd=lwd , col=col) # just single color for lines
 text( x0+0.8*sz,y0+0.9*sz, ch.label , cex=0.5,col=ifelse( col=="black" , "blue", "black" ) ) 
}
}}


## --------------------------------------------------------------------------------
##
## Topo heat map
##
## --------------------------------------------------------------------------------

ltopo.heat <- function( c , z ,
                        sz = 1 ,
                        flt = rep(T,length(z)),
                        zlab="<Z-value>" ,
			mt="" ,
                        zlim = NULL , 
                        th = NA ,
			th.z = z ,
			show.leg = T ,
			zeroed = F )

{
 ltopo.rb( c,z,flt,sz,zlab,mt,zlim,th,th.z,show.leg,zeroed,col=colorRampPalette(rev(c("red","orange","yellow","cyan","blue")))(101) )
}

ltopo.rb <- function( c , z ,
        	      flt = rep(T,length(z)),
                      sz = 1 ,
		      zlab="",
		      mt="" ,
                      zlim = NULL , 
                      th = NULL ,
		      th.z = z ,
		      show.leg = T ,
		      zeroed = T , 
                      col = colorRampPalette( c("blue","white","red" ))(101) )
{
 topo <- ldefault.xy()
 if ( length(col) != 101 ) stop( "col needs to be 101 length" )
 c <- c[flt] ; z <- z[flt] ; th.z <- th.z[flt] 
 c <- lremap.chs( c ) ; f <- c %in% toupper( topo$CH ) ; c <- c[f] ; z <- z[f] 
 if ( is.null(zlim) ) zlim <- range( z , na.rm=T)
 if ( zeroed ) zlim <- c(-1,1) * max(abs(zlim)) 
 plot( c(0,1),c(0,1) , type="n" , axes = F , xlab="", ylab="" , xaxt='n' , yaxt='n' )
 if (mt!="") text(0.025,0.95,mt,cex=0.8,pos=4)
 text( 0.75,0.05 , zlab , cex=1 )  
 for ( ch in unique( c ) ) { 
 px <- topo$X[ topo$CH == ch ] + 0.5
 py <- topo$Y[ topo$CH == ch ] + 0.5
 if ( length(px) == 1 ) {
  if ( sum( c == ch ) > 1 ) stop("multiple values for a single channel" )  
  ch.label <- topo$CH[ toupper( topo$CH ) == ch ]
  this.z <-  z[ c == ch ] ; this.th.z <- th.z[ c == ch ] 
  ring <- rep( "gray" , length(px) ); ring.lwd <- 1
  if ( ! is.null(th) ) ring[ this.th.z >= th ] <- "black"
  if ( ! is.null(th) ) ring.lwd[ this.th.z >= th ] <- 3  
  points( px , py , pch = 21 , cex = sz , col = ring , lwd= ring.lwd, 
  bg = col[ 1 + round( 100 * ( ( this.z - zlim[1] ) / ( zlim[2] - zlim[1] ) ) ) ] ) 
  x0 <- px - sz/2 ; y0 <- py - sz/2 
#  cat( ch , ch.label, this.z , "\n" ) 
}}
for ( ch in unique( c ) ) {
 px <- topo$X[ toupper( topo$CH ) == ch ] + 0.5
 py <- topo$Y[ toupper( topo$CH ) == ch ] + 0.5
 if ( length(px) == 1 ) {
 ch.label <- topo$CH[ toupper( topo$CH ) == ch ]
 x0 <- px - sz/2 ; y0 <- py - sz/2
 #text( px-0.005*sz,py+0.005*sz, ch.label , cex=0.6,col="blue")
}}
if ( show.leg ) { 
 points(seq( 0.05 , 0.5 , length.out = 101 ) , rep( 0.05 , 101 )  , col = col , pch=20 )
 text( 0.05 , 0.01 , signif( zlim[1] , 3 )  , cex=1 ) ; text( 0.5 , 0.01 , signif( zlim[2] , 3 ) , cex=1 )
}
}


## --------------------------------------------------------------------------------
##
## Topo coherence plots (links between electrodes)
##
## --------------------------------------------------------------------------------

# need diff. coords, so make xy.coh
ldefault.coh.xy <- function( xy )
{
 xy$X <- 100 * xy$X
 xy$Y <- ( 30 * xy$Y ) + 10
 return(xy)
}

# helper function: draw ARC
farc <- function(c1,c2,kol,w=4) { 
 gc <- gcIntermediate( as.vector( luna.globals$xy.coh[ luna.globals$xy.coh$CH == c1 , c("X","Y") ] ) , 
                       as.vector( luna.globals$xy.coh[ luna.globals$xy.coh$CH == c2 , c("X","Y") ] ) , 
                       breakAt=TRUE, n=100 )
 lines(gc,lwd=w+1,col="black")
 lines(gc,lwd=w,col=kol)
 #invisible(lapply(gc, lines, col=k, lwd=2))
}


farc.signed <- function (c1, c2, k1, k2, w = 4) 
{
 gc <- gcIntermediate( as.vector( luna.globals$xy.coh[ luna.globals$xy.coh$CH == c1, c("X", "Y")]),
                       as.vector( luna.globals$xy.coh[ luna.globals$xy.coh$CH == c2, c("X", "Y")]), 
                       breakAt = TRUE,
		       n = 100)		       
 segments( gc[,1] , gc[,2] , gc[,1],gc[,2], lwd=w , 
           t_cols( colorRampPalette( c(k2,"white",k1) )(101) , 80 ) )
}



# palette 
rbpal <- rev( rainbow(150)[1:100] ) 
fcol <- colorRampPalette( c( "blue" , "white" , "red" ) ) 
rbpal <- fcol(100)


# fhead1() topo

fhead1 <- function( chs , z , flt = T , zr = range(z,na.rm=T) , cex = 4 , title = "" ) 
{
plot( luna.globals$xy.coh$X , luna.globals$xy.coh$Y , pch=21 , cex=cex*0.5 , bg="white" , 
  axes=F,xaxt='n' , yaxt='n' , xlab="" , ylab = "" , ylim=c(-2,24) , xlim=c(-55,55) , main = title ) 
draw.ellipse( 0, 9.5 , 52 , 12 ); lines( c(0,-8),c(23,21), lwd=1)  ; lines( c(0,8),c(23,21), lwd=1)  
if ( ! any(flt) ) { return(0) } 
chs <- chs[flt] ; z <- z[ flt ] 
if ( length(chs) != length( z ) ) stop( "bad" )
z <- ( z - zr[1] ) / ( zr[2] - zr[1] )
z <- round( z * 100 )
z[ z==0 ] <- 1
z[ z> 100 ] <- 100
for (j in 1:length(chs)) { 
xx <- luna.globals$xy.coh$X[ xy$CH == chs[j] ] ; yy <- luna.globals$xy.coh$Y[ xy$CH == chs[j] ]
points( xx , yy , pch=21 , cex=cex*1.1 , bg="white" , lwd=1.5 ) 
points( xx , yy , pch=21 , cex=cex , bg= rbpal[z[j]] ) 
}
}

#
# ltopo: connectivity
#

ltopo.conn <- function (chs1, chs2, z, flt = T, zr = range(z[flt], na.rm = T), 
                        cex = 2, w = 8, title = "", head = T , signed = F ) 
{
    chs1 <- lremap.chs(chs1)
    chs2 <- lremap.chs(chs2)
    xy.coh <- ldefault.coh.xy(ldefault.xy(unique(c(chs1, chs2))))
    plot(xy.coh$X, xy.coh$Y, pch = 21, cex = cex, main = title, lwd=0.5,
        bg = NA, col="gray", axes = F, xaxt = "n", yaxt = "n", xlab = "", 
        ylab = "", ylim = c(-2, 24), xlim = c(-55, 55))
    if (head) {
        draw.ellipse(0, 9.5, 52, 12,lwd=0.75,border="darkgray")
        lines(c(0, -8), c(23, 21), lwd = 0.75,col="darkgray")
        lines(c(0, 8), c(23, 21), lwd = 0.75,col="darkgray")
    }
    if ( ! any(flt) ) return(0) 
    if ( signed ) zr <- c( -max(abs(zr)) , max(abs(zr)) ) 
    chs1 <- chs1[flt] ; chs2 <- chs2[flt]
    z <- z[flt] 
    chs1 <- chs1[order(abs(z))]
    chs2 <- chs2[order(abs(z))]
    z <- z[order(abs(z))]
    if (length(chs1) != length(z)) stop("bad inputs to ltopo.conn()")
    negz <- z
    if ( signed ) negz <- -1 * negz
    z <- round( 100 * (z - zr[1])/(zr[2] - zr[1]) )
    z[z == 0] <- 1 ; z[z > 100] <- 100
    negz <- round( 100 * (negz - zr[1])/(zr[2] - zr[1] ) )
    negz[negz == 0] <- 1 ; negz[negz > 100] <- 100
    if ( signed ) {
     for (j in 1:length(chs1)) {
        t <- c(chs1[j], chs2[j])
        farc.signed( t[1], t[2], rbpal[ z[j] ], rbpal[ negz[j] ] , w=w)
      }
     } else {
     for (j in 1:length(chs1)) {
        t <- c(chs1[j], chs2[j])
        farc( t[1], t[2], rbpal[ z[j] ], w = w)
     }
    }
    points(xy.coh$X, xy.coh$Y, pch = 21, cex = cex, lwd=0.5, bg = NA , col= "gray" )
    if (head) {
        draw.ellipse(0, 9.5, 52, 12,lwd=0.75,border="darkgray")
        lines(c(0, -8), c(23, 21), lwd = 0.75,col="darkgray")
        lines(c(0, 8), c(23, 21), lwd = 0.75,col="darkgray")
    }
    cat("zr", zr, "\n")
}

intop    <- function(x , p ) { x > quantile(x,1-p) }

inbottom <- function(x , p ) { x < quantile(x,p) }


t_col <- function(color, percent = 50 , name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  rgb.val <- col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
 invisible(t.col)
}


t_cols <- function(x,percent=50) { sapply( x, t_col , percent ) }


#
# ltopo.dconn : directed connectivity (i.e. A->B)
# i.e. 'ch' specifies which channel to 'seed' on
# so always assuming a signed value here
#

ltopo.dconn <- function( ch, chs1 , chs2 , z , flt = T , zr = NULL, cex = 2 , w = 8 , title = "" , head=T , signed=F)
{
if ( is.null(zr) ) zr = range(z[flt],na.rm=T)
zr <- c(-1,1) * max(abs(zr))
print(table(flt))
chs1 <- chs1[flt] ; chs2 <- chs2[flt] ; z <- z[flt] 
# double entry whatever is left
dchs1 <- c( chs1 , chs2 )
dchs2 <- c( chs2 , chs1 )
dz <- c( z , -z )
inc <- dchs1 == ch
dchs1 <- dchs1[ inc ]
dchs2 <- dchs2[ inc ]
dz <- dz[ inc ]
print( cbind( dchs1 , dchs2 , dz ) ) 
# filtering done above, so set flt to 'T' here
ltopo.conn( chs1 = dchs1 , chs2=dchs2 , z=dz , flt = T , zr = zr , cex = cex , w = w , title = title , head = head , signed=signed )
}

##
## TOPO HEATMAP: each point is a lheatmap() object (X/Y/Z plot) 
##

ltopo.heat2 <- function( c , x , y , z , zlim = NULL , 
                         f = rep(T, length(x) ) , 
                         sz = 0.08 ,cex = 1 ,
                         col = jet.colors(100) , lwd = 0.5 ,                         
                         ylab="Frequency (Hz)" , xlab="Time",  zlab = "log(power)" , mt="" )

{ 
topo <- ldefault.xy()
c <- lremap.chs( c )
f[ ! c %in% toupper( topo$CH ) ] <- F 
# z is color depth: scaled 1..100 as requires that col is a 100-element pallete
if ( ! is.na( z[1] ) ) {
 if ( length(z) != length(x) ) stop( "is z is specified, must match other long data" )
 if ( length(col) != 100 ) stop( "requires col is a 100-element palette if z is specified" )
 z <- z[f]
 if (is.null(zlim)) zlim <- range( z , na.rm=T ) 
 zcol = col[ 1 + round( 99 * ( ( z - zlim[1] ) / ( zlim[2] - zlim[1] ) ) ) ] 
 col = zcol
} 
# also pair down x/y/z/CH to filtered set
c <- c[f]; x <- x[f]; y <- y[f]; z <- z[f]
rx <- range( x ); ry <- range( y ) 
# plots
plot( c(0,1),c(0,1) , type="n" , axes = F , xlab="", ylab="" , xaxt='n' , yaxt='n' )
rect(0,0,1,1)
if (mt!="") text(0.025,0.05,mt,cex=0.8,pos=4)
lgd.x <- 0.88 ; lgd.y <- 0.08
lines( c(lgd.x,lgd.x+sz),c(lgd.y,lgd.y) ) ; lines( c(lgd.x,lgd.x) , c(lgd.y,lgd.y+sz) ) 
text( lgd.x , lgd.y , signif(ry[1],2) , cex=0.5 , pos=2) ; text( lgd.x , lgd.y+sz , signif(ry[2],2) , cex=0.5 , pos=2) 
text( lgd.x , lgd.y , signif(rx[1],2) , cex=0.5 , pos=1) ; text( lgd.x+sz , lgd.y , signif(rx[2],2) , cex=0.5 , pos=1) 
text( lgd.x+sz/2,lgd.y-0.05 , xlab , cex=0.5 )  ; text( lgd.x-0.05,lgd.y+sz/2 , ylab , cex=0.5 ) 
# plot each channel
for ( ch in unique( c ) ) { 
 ch.idx <- toupper( topo$CH ) == ch 
 px <- topo$X[ ch.idx ] + 0.5
 py <- topo$Y[ ch.idx ] + 0.5
 if ( length(px) == 1 ) { 
  ch.label <- topo$CH[ ch.idx ]
  x0 <- px - sz/2 ; x1 <- x0 + sz 
  y0 <- py - sz/2 ; y1 <- y0 + sz 
  lines( c(x0,x1),c(y0,y0),col="gray"); lines( c(x0,x0),c(y0,y1),col="gray" )
  xx <- x[ c == ch ] 
  yy <- y[ c == ch ] 
  zz <- z[ c == ch ]
  xx <- ( xx - rx[1] ) / ( rx[2] - rx[1] )
  yy <- ( yy - ry[1] ) / ( ry[2] - ry[1] )
  points( x0 + xx * sz , y0 + yy * sz ,pch = "." , cex = cex , col=col[ c == ch ] )
#lines( x0 + xx * sz , y0 + yy * sz ,lwd=lwd , col=col) # just single color for lines
  text( x0+0.8*sz,y0+0.9*sz, ch.label , cex=0.5,col=ifelse( col=="black" , "blue", "black" ) ) 
} }
}

# Helpr:: winsorize format
lwin <- function(x,p=0.05) {
 t <- quantile( x , c(p,1-p) , na.rm=T) 
 x[x<t[1]] <- t[1]
 x[x>t[2]] <- t[2]
 x
}

#
## TOPO-TOPO : each point is a ltopo.rb() plot
#    c    high-level channel
#    c2   within-plot channel
#    z    plot value
#

ltopo.topo <- function( c , c2 , z , zlim = NULL , 
                         f = rep(T, length(z) ) , 
                         sz = 0.05 , sz2 = 0.05 , 
			 ring.lwd = 1 , 
                         same.cols = T , 
                         col = rbpal , 
                         zlab = "" , mt="" , zeroed = T )

{ 
# key inputs: c, c2 and z
topo <- ldefault.xy()
topo$CH <- toupper( topo$CH )
c  <- lremap.chs( c )
c2 <- lremap.chs( c2 )

f[ ! c  %in% toupper( topo$CH ) ] <- F
f[ ! c2 %in% toupper( topo$CH ) ] <- F 

c <- c[f];
c2 <- c2[f];
z <- z[f]
if ( is.null( zlim ) & same.cols ) zlim <- range( z , na.rm=T ) 
zlim2 <- zlim 

# plots
plot( c(0,1),c(0,1) , type="n" , axes = F , xlab="", ylab="" , xaxt='n' , yaxt='n' )
rect(0,0,1,1)
if (mt!="") text(0.025,0.05,mt,cex=0.8,pos=4)

# loop over key channels (c)
for ( ch in unique( c ) ) { 
 ch.idx <- topo$CH == ch 
 px <- topo$X[ ch.idx ] + 0.5
 py <- topo$Y[ ch.idx ] + 0.5
 ch.label <- topo$CH[ ch.idx ]

 # plot loc. for this key channel: x0-x1 and y0-y1
 x0 <- px - sz/2 ; x1 <- x0 + sz 
 y0 <- py - sz/2 ; y1 <- y0 + sz 
# lines( c(x0,x1),c(y0,y0),col="gray"); lines( c(x0,x0),c(y0,y1),col="gray" )
 # get inner values
 zz <- z[ c == ch ]
 cc <- c2[ c == ch ]
 px2 <- numeric(0) ; py2 <- numeric(0)
 for (ch2 in cc ) {
  ch.idx <- topo$CH == ch2
  px2 <- c( px2 , x0 + sz * ( topo$X[ ch.idx ] + 0.5 ) )
  py2 <- c( py2 , y0 + sz * ( topo$Y[ ch.idx ] + 0.5 ) )
 }
 if ( is.null( zlim ) ) zlim2 <- range( zz , na.rm=T ) else zlim2 <- zlim
 if ( zeroed ) zlim2 <- c(-1,1) *  max(abs( zlim2 ) )
 points( px2 , py2 , pch = 21 , cex = sz2 , col = ifelse( cc == ch , "black", "lightgray" )  , lwd= ring.lwd ,
      bg = col[ 1 + round( 100 * ( ( zz - zlim2[1] ) / ( zlim2[2] - zlim2[1] ) ) ) ] )
}
}



##############################################################
#
# SUDS viewers
#
##############################################################


