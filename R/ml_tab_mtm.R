  
  #
  # MTM spectrogram tab
  #

  # helper function to set height of MTM plots
#  mtmplot_height <- function() {
#    return(150 * length(values$mtm.files) )    
#  }

#  # wrap plotOutput in renderUI
#  output$ui_mtmplot <- renderUI({
#    print("MT HGT"); print( mtmplot_height() )
#    plotOutput( "mtm.plots", height = mtmplot_height(), width = "100%")
#  })

  output$ui_mtmplot <- renderUI({
    req(attached.edf(),values$mtm.files)
    mtspec_list <- 
      lapply( 1:length( values$mtm.files ) , 
              function(i)
              {
                imagename = paste0("mtspec", i)
                imageOutput(imagename)
              })

    do.call(tagList, mtspec_list)
  })

  observe({
    if(identical(values$mtm.files, character(0))) return( NULL )
    for (i in 1:length(values$mtm.files ) )
    {
      local({
        my_i <- i
        imagename = paste0("mtspec", my_i)
        output[[imagename]] <- 
          renderImage({
            list(src = values$mtm.files[my_i],
                 alt = "Image failed to render")
          }, deleteFile = FALSE)
      })
    }
  })
