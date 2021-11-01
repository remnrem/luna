
 #
 # Helper functions 
 #

fhypnogram <- function( e , sn , sstg , disc3 = NULL , disc5 = NULL ) {
  sstg[ is.na( sstg ) ] <- "?" 
  # hypnogram image
  yh = ifelse( is.null( disc3 ) , 2 , 4 )
  e <- e/120
  plot( e  , sn , type = "l" , lwd = 2, col = "gray" , axes = F , ylim = c(-3, yh) , ylab = "" , yaxt='n' , xaxs="i" )
  points( e  , sn , col = lstgcols(sstg) , type = "p" , cex = 1 , pch = 20 )
  axis(1)
#  axis(2 , 2 , "?" , col.axis = "black" , las = 2)
#  axis(2 , 1 , "W" , col.axis = lstgcols("wake") , las = 2)
#  axis(2 , 0 , "R" , col.axis = lstgcols("REM") , las = 2)
#  axis(2 ,-1 , "N1" , col.axis = lstgcols("NREM1") , las = 2)
#  axis(2 ,-2 , "N2" , col.axis = lstgcols("NREM2") , las = 2)
#  axis(2 ,-3 , "N3" , col.axis = lstgcols("NREM3") , las = 2)
  if ( ! is.null( disc3 ) ) points( e[disc3==1] , rep(3,length(e[disc3==1])),pch="|",col="red" )
  if ( ! is.null( disc5 ) ) points( e[disc5==1] , rep(3.8,length(e[disc5==1])),pch="|",col="orange",cex=0.8 ) 
}

fstgn <- function(x) {
 x[ x == "N1" ] <- -1
 x[ x == "N2" ] <- -2
 x[ x == "N3" ] <- -3
 x[ x == "R" ] <- 0
 x[ x == "W" ] <- 1
 x[ x == "?" ] <- 2
 x[ is.na(x) ] <- 2 
 as.numeric( x )
}

f100 <- function(x) {
t <- numeric()
if ( any( is.na( x ) ) ) return( rep(6,100) ) 
for (s in rev(order(x))) t <- c( t , rep(s,x[s]) ) 
t[1:100]
}

fphyp <- function(m) {
e <- m[,1]
ne <- max(e)
h <- m[,-1]
xr <- c(1,ne)
hh <- matrix( NA , nrow = max(e) , ncol = 100 ) 
yy <- numeric(ne)
h <- round(as.matrix(h),2) * 100
h[h<0] <- 0
h[h>100] <- 100
hh <- t( apply( h, 1 , f100 ) )
stgpal <- c( lstgcols("N1") , lstgcols("N2") , lstgcols("N3") , lstgcols("R") , lstgcols("W") , "lightgray" )
# build pallete, taking only observed values
stgpal <- stgpal[ as.integer( names(table(hh)) ) ]
image(hh,col=stgpal,xaxt='n',yaxt='n',axes=F)
}


fstgdur <- function( d ) {
d2 <- t(as.matrix( d[ dim(d)[1]:1  ,c("DUR_OBS","DUR_PRD")] ))
barplot( matrix(as.numeric(d2),ncol = ncol(d2)) ,
         beside=T, horiz=T , col=lstgcols( rev( rep(d$SS,each=2) ) ) ,
	 names = rev( d$SS ) , las=2 , density=c(30,NA) ,
	 xlab="Minutes" , ylab="Sleep Stage" ) 
}


#
# SOAP
#

# original hypnogram (nb. using ALIGNED staging
# which will match SOAP if non-zero offset used
# i.e. EPOCH align=N1,N2,N3,W,R)

output$soap.view.orig <- renderPlot({
  req(attached.edf(), values$data$luna_suds_SOAP)
  par(mar = c(2, 1, 1, 1))
  ss <- values$ss.aligned 
  # hypnogram image
  fhypnogram( ss$E , ss$STAGE_N , ss$STAGE ) 
})

# SOAP hypnogram (w/ discordance)

output$soap.view.hypno <- renderPlot({
  req(attached.edf(),values$data$luna_suds_SOAP)
  par(mar = c(2, 1, 1, 1))
  sstg   <- values$data$luna_suds_SOAP$"luna_suds_SOAP_E"$data$PRED 
  epochs <- values$data$luna_suds_SOAP$"luna_suds_SOAP_E"$data$E 
  disc3  <- values$data$luna_suds_SOAP$"luna_suds_SOAP_E"$data$DISC3
  disc5  <- values$data$luna_suds_SOAP$"luna_suds_SOAP_E"$data$DISC
  fhypnogram( epochs , fstgn( sstg) , sstg , disc3, disc5 )
})

# SOAP posteriors

output$soap.view.prob <- renderPlot({
  req(attached.edf(), values$data$luna_suds_SOAP)
  par(mar = c(1, 1, 1, 1))
  epp <- values$data$luna_suds_SOAP$"luna_suds_SOAP_E"$data[,c("E","PP_N1","PP_N2","PP_N3","PP_R","PP_W") ] 
  fphyp( epp )
})

# stage durations

output$soap.view.stgdur <- renderPlot({
  req(attached.edf(),values$data$luna_suds_SOAP)
  par(mar = c(3, 3, 0, 1))
  dat <- values$data$luna_suds_SOAP$"luna_suds_SOAP_SS"$data[,c("SS","DUR_OBS","DUR_PRD")]
  fstgdur( dat )
})


#
# SUDS
#


# original hypnogram (also uses aligned observed epochs, i.e. if missing values prior to first Wake/Sleep epoch)

output$suds.view.orig <- renderPlot({
  req(attached.edf(),values$data$luna_suds_SUDS,values$has_manual_staging)
  par(mar = c(2, 1, 1, 1))
  ss <- values$ss.aligned
  # hypnogram image
  fhypnogram( ss$E , ss$STAGE_N , ss$STAGE ) 
})


# SUDS hypnogram (w/ discordance)

output$suds.view.hypno <- renderPlot({
  req(attached.edf(), values$data$luna_suds_SUDS)
  par(mar = c(2, 1, 1, 1))
  sstg <- values$data$luna_suds_SUDS$luna_suds_SUDS_E$data$PRED 
  epochs <- values$data$luna_suds_SUDS$luna_suds_SUDS_E$data$E 
  disc3 <- values$data$luna_suds_SUDS$luna_suds_SUDS_E$data$DISC3
  disc5 <- values$data$luna_suds_SUDS$luna_suds_SUDS_E$data$DISC
  fhypnogram( epochs , fstgn( sstg) , sstg , disc3, disc5 )
})

# SUDS posteriors

output$suds.view.prob <- renderPlot({
  req(attached.edf(),values$data$luna_suds_SUDS)
  par(mar = c(1, 1, 1, 1))
  epp <- values$data$luna_suds_SUDS$luna_suds_SUDS_E$data[,c("E","PP_N1","PP_N2","PP_N3","PP_R","PP_W") ] 
  fphyp( epp )
})


output$suds.view.stgdur <- renderPlot({
  req(attached.edf(),values$data$luna_suds_SUDS)
  par(mar = c(3, 3, 0, 1))
  dat <- values$data$luna_suds_SUDS$luna_suds_SUDS_SS$data[,c("SS","DUR_OBS","DUR_PRD")]
  dat$DUR_OBS[ is.na( dat$DUR_OBS ) ] <- 0
  dat$DUR_PRD[ is.na( dat$DUR_PRD ) ] <- 0  
  fstgdur( dat )
})





