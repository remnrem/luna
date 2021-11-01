  #
  # Staging tabs: hypnogram & summary, repeated for Manual staging & SUDS
  #
  
  output$stage.view <- renderPlot({
    req(attached.edf())
    par(mar = c(2.2, 4, 1, 0))
    # get stages
    ss <- values$ss 
    # hypnogram image
    plot( ss$E / 120 , ss$STAGE_N , type = "l" , lwd = 2, col = "gray" , axes = F , ylim = c(-3, 2) , ylab = "" )
    points( ss$E / 120 , ss$STAGE_N , col = lstgcols(ss$STAGE) , type = "p" , cex = 1 , pch = 20 )
    axis(1)
    axis(2 , 2 , "?" , col.axis = "black" , las = 2)
    axis(2 , 1 , "W" , col.axis = lstgcols("wake") , las = 2)
    axis(2 , 0 , "R" , col.axis = lstgcols("REM") , las = 2)
    axis(2 ,-1 , "N1" , col.axis = lstgcols("NREM1") , las = 2)
    axis(2 ,-2 , "N2" , col.axis = lstgcols("NREM2") , las = 2)
    axis(2 ,-3 , "N3" , col.axis = lstgcols("NREM3") , las = 2)
  })
  
  output$stage.summary <- renderTable({
    req(attached.edf())
    # reset MASK
    # get hypnogram information
    ss <- values$eval$HYPNO$BL
    #hypnogram summary
    ss$ID <- NULL
    t(ss)
  } , width = "100%" , striped = T , rownames = T , colnames = F)
  
