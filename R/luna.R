
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
## Attach an EDF                                  ##
##                                                ##
####################################################

ledf <- function( x , y = "." )
{
 .Call("Rattach_edf", as.character(x) , as.character(y) , PACKAGE = "rluna" );
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


ldesc <- function()
{
 .Call( "Rdesc", PACKAGE = "rluna" );
 invisible(1)
}


####################################################
##                                                ##
## Evaluate a (set of) command(s)                 ##
##                                                ##
####################################################

leval <- function( x )
{	
 .Call("Reval_cmd", as.character(x) , PACKAGE = "rluna" );
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

iterate <- function( func , epoch_size )
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

luna.sigs <- function( e , chs )
{
 .Call( Rextract_my_signals_by_epoch, PACKAGE = 'rluna' , as.integer(e) , as.character(chs) );
} 



####################################################
##                                                ##
## Basic queries                                  ##
##                                                ##
####################################################

# stats: channels, frequencies,
# intervals: length, # of epochs

luna.head <- function() {

} 


####################################################
##                                                ##
## Visualization                                  ##
##                                                ##
####################################################

#require(dygraphs)
#d <- luna.sigs(22)
#h <- luna.head()





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
        names(l[[cmd]])
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
