

  #
  # SOAP
  #

observeEvent( input$soap_run, {
  req(attached.edf() , input$sel.ch)
  values$soap <- leval( "SOAP sig=C3 save nc=10" )
})
    
observeEvent( input$resoap_run, {
  req(attached.edf() , input$sel.ch)
  if ( is.null( values$soap ) )
    values$soap <- leval( "SOAP sig=C3 save nc=10" )
  values$soap <- leval( paste( "RESOAP epoch=" , values$soap.epoch , " stage=N2" , sep="" ) )
})

observeEvent( input$resoap_scrub, {
  req( attached.edf() )
  leval( "RESOAP scrub" )

})

output$soap.results <- renderPrint({
      values$soap
})
  
output$soap.epoch.n <- renderPrint({
 # 1,2,3 = 49, 50, 51
 # 5 = 53
 # 0 = 48
 # 9 = 57 
  
 # q 81 
 # p 80


kp <- input$soap.keypress
if ( kp == 81 ) values$soap.epoch <- max( 1 , values$soap.epoch-1 ) 
if ( kp == 80 ) values$soap.epoch <- min( values$ne , values$soap.epoch+1 ) 
values$soap.epoch 
})



# Plot posteriors

output$soap.view.pp <- renderPlot({
  req(attached.edf())
  par(mar = c(2.2, 4, 1, 0))
  # get posteriors
  pp <- values$soap$RESOAP$E 
  # hypnogram image
  plot( pp$E / 120 , pp$PP_NR , type = "l" , lwd = 1, col = lstgcols("NREM2") , axes = F , ylim = c(0, 1) , ylab = "" )
  lines( pp$E / 120 , pp$PP_W , type = "l" , lwd = 1, col = lstgcols("wake")  , ylim = c(0, 1) , ylab = "" )
  lines( pp$E / 120 , pp$PP_REM , type = "l" , lwd = 1, col = lstgcols("REM") , ylim = c(0, 1) , ylab = "" )
})

output$soap.view.orig <- renderPlot({
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


