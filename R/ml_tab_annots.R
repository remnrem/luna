#
# Annotations
#
  
  output$annot.view <- renderPlot({
    req(attached.edf())
    # get annotationss
    df <- values$annot.inst$ANNOT_INST_T1_T2[, c("ANNOT", "START", "STOP")]
    df <- df[df$ANNOT %in% input$sel.ann ,]
    df$START <- df$START / 3600
    df$STOP <- df$STOP / 3600
    na <- length(unique( df$ANNOT ) )
    
    # length of recording
    k <- leval("HEADERS")
    recdur.hrs <- k$HEADERS$BL$TOT_DUR_SEC / 3600
    # main plot (-3600 puts 2 hr of time in the left axis for labels)
    par(mar = c(2.2, 0, 0, 0))
    plot( c(-2,recdur.hrs) , c(0,1) , type = "n" , axes = F , ylim = c(0, 1) , ylab = "" , xlab="" )
    axis(1 , 0:round(recdur.hrs))  
    # plot each annot (0.5 is spacer for top/bottom)
    py <- yinc <- 1 / ( length(input$sel.ann) + 0.5 )
    yidx <- 1
    for (ann in input$sel.ann) { 
      cidx <- 1 + ( yidx %% 10 ) 
      flt <- df$ANNOT == ann 
# OLD: points( df$START[ flt ] , rep( 1 - py , sum( flt ) ) , pch="|" , cex=1 , col = pal10[cidx]  )
      for (aa in which(flt))
        rect( df$START[ aa ] , 1-py-0.5*yinc , df$STOP[aa] , 1-py+0.5*yinc , col = pal10[cidx]  , border=NA)
      text( -2 , 1-py , ann , col =  pal10[cidx] , pos = 4 )
      py <- py + yinc  
      yidx <- yidx + 1
    }
  })
  
  
  output$annot.summary <- renderTable({
    req(attached.edf())
    df <- values$annot.inst$ANNOT
    df$ID <- NULL
    df$AVG <- df[,3] / df[,2] 
    df[,2] <- as.integer(df[,2])
    names(df) <- c("Annotation", "Count", "Total duration (secs)" , "Average duration (secs)"  )
    df
  } , width = "100%" , striped = T , rownames = F , colnames = T)
  
  output$annot.table <- renderDataTable({
    req(attached.edf())
    df <-
      values$annot.inst$ANNOT_INST_T1_T2[, c("ANNOT", "INST", "START", "STOP")]
    df <- df[df$ANNOT %in% input$sel.ann ,]
    df$DUR <- round( df$STOP - df$START , 3 ) 
    names(df) <-
      c("Annotation ID", "Instance ID", "Start (secs)", "Stop (secs)", "Dur (secs)")
    df
  })
