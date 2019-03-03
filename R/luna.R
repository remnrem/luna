
####################################################
##                                                ##
## Rluna                                          ##
##                                                ##
####################################################

rluna.version <- "v0.2"

rluna.date    <- "1-Mar-2019"


####################################################
##                                                ##
## Initialize                                     ##
##                                                ##
####################################################

.onLoad <- function(libname, pkgname) 
{
  packageStartupMessage( paste( "** rluna" , rluna.version , rluna.date ) )
  library.dynam("rluna", package="rluna", lib.loc = NULL)
}


.onUnload <- function (libpath) {
  library.dynam.unload("rluna", libpath)
}



####################################################
##                                                ##
## Work with sample lists                         ##
##                                                ##
####################################################


lsl <- function( f , path = "" ) { 
d <- read.table(f,header=F,fill=T,sep="\t",stringsAsFactors=F) 
if ( dim(d)[2] < 2 ) stop("invalid sample list")
cat( dim(d)[1],"observations in",f,"\n")
if ( dim(d)[1] > length(unique(d[,1])) ) stop("duplicate IDs found")
l <- list()
for (i in 1:dim(d)[1]) {  
l[[ d[i,1] ]]$EDF <- ifelse( path=="" , d[i,2] , paste(path,d[i,2],sep="/") )  
a <- d[i,-c(1:2)] ; l[[ d[i,1] ]]$ANNOT <- a[ a != "" ]  }
l
}


# IDs : names(sl)


####################################################
##                                                ##
## Attach an EDF                                  ##
##                                                ##
####################################################


ledf <- function( x , y = "." , z = character(0) )
{
 # EDF, ID, annotations
 .Call("Rattach_edf", as.character(x) , as.character(y) , as.character(z) , PACKAGE = "rluna" );
 lstat()
 invisible(1)
} 

## report on the in-memory EDF what is in memory
lstat <- function() { 
 invisible( .Call("Rstat" , PACKAGE = "rluna" ) );
}

## report on the in-memory EDF what is in memory
ldesc <- function() { 
 invisible( .Call("Rdesc" , PACKAGE = "rluna" ) );
}

llog <- function(x) { 
 if ( length(x) == 1 ) .Call("Rlogmode" , as.integer(x) , PACKAGE = "rluna" );
 invisible(1)
}

lepoch <- function( d , i = -1 ) 
{
 if ( i <= 0  ) .Call( "Repoch_data" , as.double(d), as.double(d) , PACKAGE = "rluna" );
 if ( i > 0   ) .Call( "Repoch_data" , as.double(d), as.double(i) , PACKAGE = "rluna" );
 invisible(1)
}


ldrop <- function()
{
 .Call( "Rclear" , PACKAGE = "rluna" );
 invisible(1)
} 


lrefresh <- function()
{
# load back in existing EDF
stop("not implemented yet")
}

####################################################
##                                                ##
## Evaluate a (set of) command(s)                 ##
##                                                ##
####################################################

leval <- function( x )
{	
 retval <- .Call("Reval_cmd", as.character(x) , PACKAGE = "rluna" )
 lstat()
 invisible(retval)
} 


####################################################
##                                                ##
## Attach a destrat database                      ##
##                                                ##
####################################################

ldb <- function( x , y = "" )
{	
 .Call("Rdb2retval", as.character(x) , as.character(y) , PACKAGE = "rluna" );
} 


####################################################
##                                                ##
## Iterate epoch-wise, applying a function        ##
##                                                ##
####################################################

literate <- function( func , epoch_size )
{
# hmm.. not sure about the new.env() and tmp
tmp <- .Call( "Riterate" ,
       	       as.function(func) ,
	       as.integer(epoch_size) ,
	       new.env() ,
	       PACKAGE = "rluna" )
}



####################################################
##                                                ##
## Pull raw signals/annotations                   ##
##                                                ##
####################################################

# returns a matrix
lsig <- function( e , chs )
{
 .Call( Rextract_my_signals_by_epoch, PACKAGE = 'rluna' , as.integer(e) , as.character(chs) );
} 


####################################################
##                                                ##
## Misc, helper functions                         ##
##                                                ##
####################################################


lstrat <- function( l , cmd = "" )
{
   if ( cmd == "" )
   {
        n <- names(l)
        for (i in n) cat( i , ":" , lstrat( l	, i ) ,	"\n" , sep=" " )
   }
   else
   {
        t <- names(l[[cmd]])
   }
}


lx <- function( l , cmd , f = "" , ... )
{
   f <- paste(sort( unlist( c( f , list(...) )  )  ), sep="" , collapse="_" )
   if (	f != ""	    )
      l[[cmd]][[f]]
   else 
      l[[cmd]]
}






lunar.test <- function(x)
{
 .Call("Rtest", as.integer(x) , PACKAGE = "rluna" );
}


# e.g. list -> data.frame
# as.data.frame( list( X = unlist( lapply( tmp , "[[" , "NAME" ) ) ,
#  	               Y = unlist( lapply( tmp , "[[" , "Y" ) ) ,
# 		       Z = as.numeric( lapply( tmp , "[[" , "Z" ) ) ) )


# extraction tools for lists
# l is the list, m is the matching label
#x1 <- function(l , m) {
#as.vector( unlist( lapply( lapply( l , "[[" , m ) , function(x) ifelse(is.null#(x),NA,x) ) ) ) }
# matrix:
#x2 <- function(l,m="GT") {
# matrix( unlist( lapply( lapply( l , "[[" , "CON" ) , "[[" , c("GENO",m) ) ) ,# nrow = length(l) , byrow = T ) }
