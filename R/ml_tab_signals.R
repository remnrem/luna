  #
  # Signals
  #

  output$signal.master <- renderPlot( { 
    req(attached.edf())
    session$resetBrush("master_brush")
    leval( "MASK clear" )

    # hypnogram image used to select from the above
    par(mar=c(0,0,0,0))

    if ( staging_panel_present ) {
       # use manual staging, if available
       
      if ( values$has_manual_staging ) {  
          plot( values$ss$E , rep( 0.5 , length(values$ss$E) ) , 
                 col = lstgcols( values$ss$STAGE ) , axes = F , ylim = c(0, 1) , pch="|" , ylab = "" , xaxs='i' , yaxs='i' ) 
      } else if ( values$has_suds_staging ) {
          suds_ss <- values$data$luna_suds_SUDS$luna_suds_SUDS_E$data$PRED
          plot( suds_ss , rep( 0.5 , length( suds_ss ) ) , 
                 col = lstgcols( suds_ss ) , axes = F , ylim = c(0, 1) , pch="|" , ylab = "" , xaxs='i' , yaxs='i' ) 
     } 
    } else {
      # just fill in blank
      plot(seq(1,values$ne), rep( 0.5 , values$ne ) , axes = F , ylim = c(0, 1) , pch="|" , ylab = "" , xaxs='i' , yaxs='i' )
    }
  } )
  
  output$signal.master2 <- renderPlot( { 
    req(attached.edf())
    par(mar=c(0,0,0,0))
    plot( values$epochs , c( 0.5 , 0.5 ), col = "black" , lwd=5 , type="l" , axes=F, ylab="" , xlab="" , ylim=c(0,1), xlim=c(1,values$ne) , xaxs='i' , yaxs='i' ) 
  } )
  

  #
  # Summary PSD (SPSD) plot
  # 


  output$signal.spsd <- renderPlot( {
    req(attached.edf(),values$spsd)
    par(mar=c(0,0,0,0))

    fspsd( )  
  } )


  #
  # Primary signals plot  
  #
  
  output$signal.view <- renderPlot({
    req( attached.edf() , c(input$sel.ch , input$sel.ann) )

    # reset MASK
    lrefresh()

    epochs <- values$epochs
    zoom   <- values$zoom
    
    isolate( { 
      
      #    cat( "\nin renderPlot()\n" )
      
      # epochs are the (30-second) spanning epochs which are fetched (that always)
      # if zoom is defined, then back calculate 
      
      # should not happen, but if for some reason nothing is defined, 
      # display the first epoch:
      if ( is.null( epochs ) & is.null( zoom ) )
      {
        epochs <- c(1,1)
        zoom <- c(0,30)
        values$raw.signals <- T 
      }
      else
      {
        
        if ( is.null( epochs ) ) 
          epochs <- c( floor( ( zoom[1]/30) + 1 ) , floor( ( zoom[2]/30)+1 ) )
        
        if ( is.null( zoom ) )
          zoom <- c( ( epochs[1] - 1 ) * 30 , epochs[2] * 30 ) 
        
        epochs <- c( floor( epochs[1] ) , ceiling( epochs[2] ) )
      }
      
      # compile final values: epochs and seconds (always round to nearest whole second) 
      secs <- c( floor( zoom[1] ) , ceiling( zoom[2] ) ) 
      
      # we should now have a) the spanning epochs (for ldata() ) in values$epochs
      # and the range to display in values$zoom (in seconds)
      
      #    cat( "epochs : " , epochs , "\n" )
      #    cat( "seconds: " , secs , "\n" )
      
      # update raw signals status as needed: if more than 5 mins, use summary stats
      #values$raw.signals <- ( epochs[2] - epochs[1] ) < 10 
      values$raw.signals <- ( zoom[2]/30 - zoom[1]/30 ) < 10
      
      annots <- input$sel.ann
      chs    <- input$sel.ch
      na     <- length( annots )
      nc     <- length( chs )
      
      
      #
      # Plot parameters
      #
      
      # room for text on left (but w/in plot),
      # is 20% of main span
      x0 <- secs[1] - ( secs[2] - secs[1] ) * 0.2
      xr <- range( x0 , secs[2] )
      
      # y-axis
      cfac <- 3    # channel : annotation y-expansion factor
      sfac <- 1.5  #           spanning factor (only for raw signals, not summ stats)
      
      # i.e. give chs x3 vertical space; +1 is spacer
      yinc  <- 1.0 / ( cfac * nc + na + 1 )   
      
      # width of y-range (might be > yinc, i.e. for partial overlap)
      yspan <- yinc * sfac  
      
      # initiate y-poinyter (half an increment up)
#      yp <- yinc * 0.5
      yp <- 0
      yidx <- 1
      
      # initiate plot
      par( mar=c(2.2,0,0,0) )
      
      plot( c(0, 1) , type = "n" , 
            ylim = c(0, 1) , 
            xlim = xr, xaxt='n',yaxt='n',axes=F,
            xlab="" , ylab="" )
      
      axis(1 , c(secs[1],secs[2]) ) 
      
      #
      # Zoomed-in hypnogram at top
      #
      
      stgs <- values$ss$STAGE
      enum <- values$ss$E 
      
      for (e in epochs[1]:epochs[2]) {
        s <- secs[1] + ( e - epochs[1] ) * 30 
        if(s<secs[2]){
          s_end=s+30
          if(s_end > secs[2]){
            s_end=secs[2]
          }
          rect( s , 0.99 , s_end , 1.00 , 
                col =  lstgcols( stgs[ enum == e ]  ) , 
                border = NA )          
        }

      }
      
      #
      # Signals 
      #
      
      if ( nc ) {
        

        #
        # For short intervals, plot original data 
        #
        
        if ( values$raw.signals )
        {
          
          #
          # Pull raw signal data 
          #
          
          yidx <- 0
          for (ch in rev(chs)){
            req(epochs[1] >= 1, epochs[2] <= values$ne)
            dat <- ldata( epochs[1]:epochs[2] , chs = ch )
            dat <- dat[ dat$SEC >= secs[1] & dat$SEC <= secs[2] , ]
            ts <- dat$SEC
            dy <- dat[, 4]
            
            yr <- range( dy )
            # zero-centered signal?
            zc <- yr[1] < 0 & yr[2] > 0 
            # max absolute value
            yrmx <- max(abs(range( dy ) ) )
            # if +/- signal, scale to -1, +1 ( 0 .. 1 ) based on max( |x| ) 
            dy <- ( dy - mean(dy) ) / (2*yrmx)
            # convert to plot co-oords
            dy <- ( yp + (yinc*cfac)/2 ) + dy * yspan * cfac
            # plot
            cidx <- yidx %% 10 + 1  
            lines(ts , dy , lwd = 0.75 , col = pal10[cidx])
            # labels
            text( x0 , yp+ (yinc*cfac)/2 , 
                  paste( ch , "\n(" , signif(yr[1],3), ",", signif(yr[2],3) , values$units[ ch ] , ")" ) 
                  , pos = 4 , col = pal10[cidx] , cex = 0.9 )  
            # drop down to next channel
            yp <- yp + yinc * cfac 
            yidx <- yidx + 1 
            
          }
          
        }
        
        #
        # else, plot reduced form if longer interval (if sigstats available)
        #
        
        else if ( ! is.null( values$sigstats ) )
        {
          
          # sigstats data
          sigstats <- NULL

          #          
          # sigstats contains two stats: S1 and S2
          #  if both defined, S1 = Hjorth 1,  S2 = Hjorth 2 
          #  if only S1 defiend , S1 = mean  ( S2 == NA ) 
          
          sigstats <- values$sigstats[ values$sigstats$E >= epochs[1] 
                                       & values$sigstats$E <= epochs[2] 
                                       & values$sigstats$CH %in% chs , ]
          

          # palette for H2
          pal100 <- rev( plasma(100) ) 
          
          # reset        
          yidx <- 0
          
          for (ch in rev(chs) ) {
            
            cidx <- yidx %% 10 + 1  
            
            no_summaries <- sum( sigstats$CH== ch ) == 0  

            
            #
            # No summary data available for this channel
            #
            
            if ( no_summaries) {
              text( x0 + 0.1 * (xr[2]-xr[1]) , yp+ (yinc*cfac)/2 , 
                    "... no summary values available ... \n... select a smaller region to view this signal ... "  
                    , pos = 4 , col = pal10[cidx] , cex = 1 )  
              text( x0 , yp+ (yinc*cfac)/2 , 
                    ch ,  
                    pos = 4 , col = pal10[cidx] , cex = 0.9 )  
              
            }
            else
            {

              #
              # Either show means or Hjorth paramters, depending if we have just S1 or S1+S2 in sigstats
              #

              use_mean <- any( is.na( sigstats$S2[ sigstats$CH== ch] ) )
              
              min.S1 <- min( sigstats$S1[ sigstats$CH== ch] , na.rm=T)
              max.S1 <- max( sigstats$S1[ sigstats$CH== ch] , na.rm=T)
              min.S2 <- ifelse( use_mean , NA , min( sigstats$S2[ sigstats$CH== ch] , na.rm=T) )
              max.S2 <- ifelse( use_mean , NA , max( sigstats$S2[ sigstats$CH== ch] , na.rm=T) )
              
              if ( use_mean ) {
                
                #
                # Show epoch-wise mean  
                #
                zoomed.epochs <- c( floor( ( secs[1]/30) + 1 ) , floor( ( secs[2]/30)+1 ) )
                secx <- seq( secs[1] , secs[2] , length.out = zoomed.epochs[2] - zoomed.epochs[1]+1 )  # 30 second epochs 
                ydat <- sigstats$S1[ sigstats$E >= zoomed.epochs[1] & sigstats$E <= zoomed.epochs[2] & sigstats$CH== ch ]  
                # scale to 0..1
                ydat <- ( ydat - min.S1 ) / ifelse( max.S1 - min.S1 > 0 ,  max.S1 - min.S1 , 1 ) # normalize within range(?)
                # scale to pixel co-ords (nb. dropped yspan)
                dy <- yp + ydat * yinc * cfac

                cidx <- yidx %% 10 + 1  
                
                secx <- secx[1:length(dy)] # normalize both axes
                
                lines( secx, dy , lwd = 2 , col = pal10[cidx])

                # labels
                text( x0 , yp+ (yinc*cfac)/2 , 
                      paste( ch , "\n(" , signif(min.S1,3), ",", signif(max.S1,3) , values$units[ ch ] , ")" ) 
                      , pos = 4 , col = pal10[cidx] , cex = 0.9 )  
              } else {
                
                #
                # Show H1/H2 
                #
                
                for ( e in epochs[1]:epochs[2] ) {
                  secx <- secs[1] + 30 * ( e - epochs[1] )
                  
                  if (secx < secs[2]){
                    secx_end=secx+30
                    if(secx+30 > secs[2]){
                      secx_end=secs[2]
                    }
                    ydat <- sigstats$S1[ sigstats$E == e & sigstats$CH== ch ]  
                    ydat <- ( ydat - min.S1 ) / ifelse( max.S1 - min.S1 > 0 ,  max.S1 - min.S1 , 1 )
                    h2 <-  sigstats$S2[ sigstats$E == e &sigstats$CH== ch ] 
                    if ( max.S2 - min.S2 > 0 ) h2 <- ( h2 - min.S2 ) / ( max.S2 - min.S2 )
                    else h2 <- rep( 0 , length(h2) )
                    ycol <- floor( 100 * h2 ); ycol[ ycol < 1 ] <- 1 
                    rect( secx ,  yp + (yinc*cfac)/2 - yspan  * ydat , 
                          secx_end , yp + (yinc*cfac)/2 + yspan  * ydat  , 
                        col = pal100[ ycol ] , border=pal100[ ycol ]) 
                }
                }
                text( x0 , yp+ (yinc*cfac)/2 , ch , pos = 4 , col = "black" ) 
                
              } # end of Hjorth summ
            } # end of summary choice (none / mean / Hjorth )
            
            # more major increment
            yp <- yp + yinc * cfac 
            yidx <- yidx + 1 
                      
          } # next channel
          
        } # end of sigstats views

        else
        {

          #
          # otherwise, print message about data not present (i.e. no summary data ) 
          # 
          
          yidx <- 0
          cidx <- 0
          
          for (ch in rev(chs)){
            cidx <- yidx %% 10 + 1  
            
            text( x0 + 0.1 * (xr[2]-xr[1]) , yp+ (yinc*cfac)/2 , 
                  "... no summary values available ... \n... select a smaller region to view this signal ... "  
                  , pos = 4 , col = pal10[cidx] , cex = 1.0 )  
            
            # labels
            text( x0 , yp+ (yinc*cfac)/2 , 
                    ch ,  
                   pos = 4 , col = pal10[cidx] , cex = 0.9 )  

            # drop down to next channel
            yp <- yp + yinc * cfac 
            yidx <- yidx + 1 
            
          }
          
        }
        
      } # end of 'if-channels'
      
      #
      # Annotations (these are pre-loaded) [ will be plotted at top ]
      # 
      
      if ( na ) 
      {
        
        df <- values$annot.inst$ANNOT_INST_T1_T2[ , c("ANNOT", "START", "STOP")]
        df <- df[df$ANNOT %in% annots , ]
        df <- df[ ( df$START <= secs[2] & df$STOP >= secs[1]) , ]
        # left/right censor
        df$START[ df$START < secs[1] ] <- secs[1]
        df$STOP[ df$STOP > secs[2] ] <- secs[2]
        
        for (annot in rev(annots)) {
          # color 
          cidx <- 1 + ( yidx %% 10 ) 
          flt <- which( df$ANNOT == annot )
          
          for (a in flt)
            rect( df$START[ flt ]  , yp + yinc/2  + yinc/4 , 
                  df$STOP[ flt ] , yp + yinc/2 - yinc/4 ,
                  col = pal10[cidx]  , border = NA )
          # labels
          legend( x0 , yp+ yinc/2 ,  annot  , yjust = 0.5 , fill = pal10[cidx] , cex = 0.9 , border = NA   )  
          
          # drop down to next annotation
          yp <- yp + yinc
          yidx <- yidx + 1 
        }
        
      } # end if annots
      
      session$resetBrush("zoom_brush")
      
    }) # isolate
    
    
  }, height = "auto")
  
  
  # 
  # Handle signal plot interactions
  #
  
  #Clear Annotation Instance selection on interaction with master plot
  
  clear_sel_inst <- function(){
    if(!is.null(input$sel.inst)){
      updateSelectInput(session, "sel.inst", selected = '')
    }
  }
  
  # single-click jumps to a single epoch
  observeEvent( input$master_click , {
    if ( is.null( input$master_brush  ) ) { 
      clear_sel_inst()
      values$epochs = c( floor( input$master_click$x ) , floor( input$master_click$x ) )
      values$zoom = NULL
    }
  })  
  
  # double-click clears all
  observeEvent( input$master_dblclick , {
    clear_sel_inst()
    values$epochs <- c(1,1)
    values$zoom = NULL
  })  
  
  # brush will zoom in to range of epochs
  observeEvent( input$master_brush , {
    clear_sel_inst()
    brush <- input$master_brush
    if (  ! is.null( brush ) ) {
      if(brush$xmin < 1 || brush$xmax > values$ne ){
        session$resetBrush("master_brush")
      } else{
        values$epochs = c( brush$xmin , brush$xmax ) 
        values$zoom = NULL 
      }
    } else {
      values$epochs = values$zoom = NULL 
    }
  })  
  
  # Full Length selection
  observeEvent(input$entire.record,{
    req(attached.edf())
    session$resetBrush("master_brush")
    clear_sel_inst()
    values$epochs=c(1,values$ne)
    values$zoom = NULL
  })
  
  # drive by annotation instance box
  observeEvent( input$sel.inst , {
    print("C2\n")
    xx <- range( as.numeric( unlist( strsplit( input$sel.inst ," ") )  )  ) 
    xx <- c( floor( xx[1]/30 )+1 , ceiling( xx[2] /30 ) )
    values$epochs = xx
    values$zoom = NULL
    session$resetBrush("master_brush")
    session$resetBrush( "zoom_brush" )
    #    session$setBrush(
    #      brushId = "master_brush", 
    #      coords = list(xmin=xx[1], xmax=xx[2]) )
    #      panel = 1
    
  })  
  
  
  #  observeEvent( input$zoom_click , {
  #    if ( is.null( input$zoom_click$zoom_brush) ) {
  #      session$resetBrush( "master_brush" )
  #      values$zoom = NULL
  #    }
  #  })  
  
  observeEvent( input$zoom_dblclick , {
    session$resetBrush( "zoom_brush" )
    #    session$resetBrush( "master_brush" )
    #    values$epochs = NULL
    values$zoom = NULL
  })  
  
  observeEvent( input$zoom_brush , {
    brush <- input$zoom_brush
    epochs <- values$epochs
    if  ( ! is.null( brush ) && !(brush$xmin < (epochs[1]-1)*30  || brush$xmax > epochs[2]*30) ) {
      values$zoom = c( brush$xmin , brush$xmax ) 
    } else {
      values$zoom = NULL
    }
  })  
  
  observeEvent(input$button_epoch_prv, {
    req( attached.edf() )
    clear_sel_inst()
    curr_epochs <- values$epochs
    values$zoom = NULL 
    session$resetBrush( "master_brush" )
    if(curr_epochs[1] >1 &&  curr_epochs[1] <= values$ne && curr_epochs[2]>1 && curr_epochs[2] <= values$ne ){
        values$epochs = c( curr_epochs[1]-1,curr_epochs[2]-1)
    }
  })
  
  observeEvent(input$button_epoch_nxt, {
    req( attached.edf() )
    clear_sel_inst()
    curr_epochs <- values$epochs
    values$zoom = NULL 
    session$resetBrush( "master_brush" )
    if(curr_epochs[1]>0 && curr_epochs[1] < values$ne && curr_epochs[2] > 0 && curr_epochs[2] < values$ne){
        values$epochs = c( curr_epochs[1]+1,curr_epochs[2]+1 ) 
      }
    
  })
  
  output$info2 <- 
    renderText({
    req( attached.edf() )

    # zoom info to display?

    zoom_info <- NULL

    if(!is.null(values$zoom)){
      brush <- input$zoom_brush
      zoom_info <- paste0(". Zoomed in epoch range is: ",floor(values$zoom[1]/30), " to ",ceiling(values$zoom[2]/30))
    } 

    epochs <- values$epochs
    if ( is.null( epochs ) ) epochs <- c(1,1)
    hrs <- ( ( epochs[1] - 1 ) * 30 ) / 3600 
    
    all_good <- TRUE
    max_epoch <- values$ne
    if( (epochs[1] < 1 || epochs[2] > max_epoch) && is.null(input$master_brush)) {
        all_good <- FALSE
    }
    if((epochs[1] < 1 || epochs[1] > max_epoch) && (epochs[2] < 1  || epochs[2] > max_epoch) && !is.null(input$master_brush)){
       all_good <- FALSE
    }

    if ( all_good ) {
      paste0( "Epoch ",  floor( epochs[1]) , " to ",  ceiling( epochs[2] ) , 
            " ( " , ( ceiling( epochs[2] ) - floor( epochs[1] ) + 1 ) * 0.5 , " minutes )" , 
            " beginning " , signif( hrs , 2 ) , " hours from EDF start", zoom_info)
    } else{
      paste0("Selected value is out of range")
    }
  })



# ----------------------------------------------------------------------------------------
#
# SPSD plot
#
# ----------------------------------------------------------------------------------------


fspsd <- function( ) { 

# resolution
#    seconds:  30 - 600 = d1
#              600 - 3600 = d5
#              3600 - 7200 = d10
#              7200 +      = d30 

#   d1    up to 10 minutes  --> 1 sec resol ( up to 600 datapoints;  one epoch = 30 data points)
#   d5    up to 1 hour      --> 5 sec resol ( up to 720 datapoints)
#   d10   up to 2 hours     --> 10 sec resol ( up to 720 datapoints)
#   d30   over 2 hours      --> 30 sec resol ( e.g. 1200 data points for 10 hour study)

 epochs <- values$epochs
 if ( is.null( epochs ) ) epochs <- c(1,1)
 t1 <- ( epochs[1] - 1 ) * 30 
 t2 <- ( epochs[2]     ) * 30 
 td <- t2 - t1
 dd <- data.frame()

cat("t1,t2" , t1 , t2 , td , "\n" ) 

if ( td <= 600 ) {
  dd <- values$spsd$d1[ t1:t2 , ] 
 } else if ( td <= 3600 ) {
  dd <- values$spsd$d5[ ((t1-1)*6+1):((t2-1)*6+1) , ]
 } else if ( td <= 7200 ) {
  dd <- values$spsd$d10[ ((t1-1)*3+1):((t2-1)*3+1) , ]
 } else {
  dd <- values$spsd$d30[ (epochs[1]:epochs[2]) , ] 
 }

cat("epoch",epochs,"\n")
print(dim(dd))
print(head(dd))

# d <- d[ ceil( t1/sc ) : floor( t2/sc ) , ]

 #dd <- spsd$d30
# neg: slow, delta, sigma
# pos: alpha, beta, gamma

pcol <- c(6,7)
ncol <- c(1,2,3,4,5)

n <- dim(dd)[1]
tt <- 1:n
dd[,ncol] <- dd[,ncol] * -1
yr <- range( c( apply( dd[,pcol] , 1 , sum ) , apply( dd[,ncol] , 1 , sum ) ) )
#yr <- c(-1,1) * max( abs( yr ) )

# x-axis scaling (20% blank at left, to match signals)
xr <- range(tt)
x0 <- xr[1] - ( xr[2] - xr[1] ) * 0.2
xr <- range( x0 , xr[2] )
par(mar=c(0,0,0,0))
plot( tt , rep(0,n) , ylim= yr , xaxt='n' , yaxt='n' , xaxs='i' , yaxs='i', type="n" , axes=F,xlab="",ylab="",main="", xlim=xr)
abline(h=0)

bpal <- colorRampPalette( c("blue","white","red") )(7)
# pos
ycurr <- rep(0,n)
for (b in pcol) {
 polygon( c( tt , rev(tt) ) , c( dd[,b] + ycurr , rev(ycurr) ) , col = bpal[b] )
 ycurr <- ycurr + dd[,b]
}

# neg
ycurr <- rep(0,n)
for (b in rev(ncol)) {
 polygon( c( tt , rev(tt) ) , c( dd[,b] + ycurr , rev(ycurr) ) , col = bpal[b] )
 ycurr <- ycurr + dd[,b]
}

}