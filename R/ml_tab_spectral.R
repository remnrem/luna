  #
  # Power tab
  #
  
  # helper function to set height of PSD plots
  psdplot_height <- function() {
    values$psd.plot.height <-
      250 * max(1, ceiling(length(input$sel.ch) / 2))
    return(values$psd.plot.height)
  }
  
  # wrap plotOutput in renderUI
  output$ui_psdplot <- renderUI({
    req(attached.edf(),input$sel.ch)
    plotOutput("power.spectra", height = psdplot_height(), width = "100%")
  })
  
  
  output$power.spectra <- renderPlot({
    req(attached.edf() , input$sel.ch)
    
    # reset MASK
    lrefresh()
    # specified channels/annotations
    annots <- input$sel.ann
    # if 1+ annotation selected, restrict analysis to those
    # otherwise, consider all epochs
    alabel <- "All epochs"
    if (length(annots) > 0) {
      leval("MASK all")
      for (annot in annots) {
        cat(annot , "is ann\n")
      }
      unmask_annots<- paste(annots,collapse = ",")
      k_mask <- leval(paste("MASK unmask-if",unmask_annots,sep = "="))
      if(tail(sort(k_mask$MASK$EMASK$N_RETAINED),n=1)){
        leval("RESTRUCTURE")
      }
      alabel <- paste0(annots , collapse = ", ")
    }
    # get PSD
    k <- leval(paste(
      "PSD spectrum dB sig=" ,
      paste0(input$sel.ch , collapse = ",") ,
      " max=" ,
      input$sel.freq[2] ,
      sep = ""
    ))
    
    # plot PSD
    ns <- length(input$sel.ch)
    par(mfrow = c(ceiling(ns / 2), 2), mar = c(4, 4, 2, 1))
    for (ch in input$sel.ch) {
      frq <- k$PSD$CH_F$F[k$PSD$CH_F$CH == ch]
      pwr <- k$PSD$CH_F$PSD[k$PSD$CH_F$CH == ch]
      pwr <-
        pwr[frq >= input$sel.freq[1] & frq <= input$sel.freq[2]]
      frq <-
        frq[frq >= input$sel.freq[1] & frq <= input$sel.freq[2]]
      if (length(pwr) > 0)
        plot(
          frq ,
          pwr ,
          col = "darkgreen" ,
          type = "l" ,
          lwd = 2 ,
          xlim = input$sel.freq ,
          main = ch ,
          xlab = "Frequency (Hz)",
          ylab = "Power (dB)"
        )
    }
  })
  
  
  
