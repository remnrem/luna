

#
# NAP console
#

show_detailed_logs_butn <- reactiveVal(FALSE)

observeEvent(input$errLogsButton,{
  output$errLogs <- renderUI({verbatimTextOutput("errLogsText", placeholder = TRUE)})
  output$errLogsText <-renderText({
    filename <- file.path(nap.dir ,  values$ID , 'nap.err') 
    req( file.exists( filename ) )
    filename <- normalizePath( filename , mustWork = F)
    cat(filename, "\n")
    readChar(filename, file.info(filename)$size)
  })
})

observeEvent( input$refresh_nap_log, {

 req( attached.edf())
 if (use_aws) {
    nap_files <- paste( aws.user, aws.runid, "nap", values$ID, sep = "/", collapse = NULL)
    withProgress(message="Pulling latest NAP files",{
      file_index <- 1
      for (f in s3_bucket) {
        if ( grepl( nap_files, f[["Key"]]) ) {
          save_object( s3_bucket[[file_index]], file=f[["Key"]], show_progress = TRUE)
        }
        incProgress(file_index*(1/length(s3_bucket)))
        file_index <- file_index+1
      }
    })
 }
})


read_nap_log <- reactive({

  filename <- file.path(nap.dir ,  values$ID , 'nap.log')

  if ( ! file.exists(filename) )
    return ( "NAP not initiated: refresh to update" )

  filename <- normalizePath( filename , mustWork = F)
  show_detailed_logs_butn(TRUE)
  readChar(filename, file.info(filename)$size)
})

output$errLogsButton <- renderUI({
  if( show_detailed_logs_butn() )
    actionButton( "errLogsButton", "View detailed log")
})

output$logger <- renderText({
  req(attached.edf())
  read_nap_log()
})
  


  #
  # Tables tab
  #
  
  # update table-table depending on table-group
  
  observe({
    req(input$sel.table.group)
    # extract names/desc for the tables in this group (skipping the group-level desc)
    # i.e. everyhting other than 'desc' keyword in the list is assumed to be a list(desc,data) object
    tables <-
      lapply(values$data[[input$sel.table.group]][names(values$data[[input$sel.table.group]]) != "desc"] ,
             "[[" , "desc")
    d.tables <- as.list(names(tables))
    names(d.tables) <- unlist(tables)
    
    updateSelectInput(
      session,
      "sel.table.table",
      choices = d.tables  ,
      label = paste(length(d.tables) , " tables")
    )
  })
  
  
  output$table.table <- DT::renderDataTable(DT::datatable({
    req(attached.edf() ,
        input$sel.table.group  ,
        input$sel.table.table)
    
    data <-
      values$data[[input$sel.table.group]][[input$sel.table.table]]$data
    data
  },
  rownames = F ,
  options = list(
    pageLength = 25 ,
    lengthMenu = list(c(25, 50,-1), c("20", "50", "All")) ,
    columnDefs = list(list(
      className = "dt-center", targets = "_all"
    ))
  )))
  
  
  
  #
  # Figures tab
  #
  
  observe({
    req(input$sel.figure.group)
    fig.labels <-
      unlist(lapply(values$figures[[input$sel.figure.group]][names(values$figures[[input$sel.figure.group]]) != "desc"] ,
                    "[[" , "desc"))
    fig.files <-
      lapply(values$figures[[input$sel.figure.group]][names(values$figures[[input$sel.figure.group]]) != "desc"] ,
             "[[" , "figure")
    names(fig.files) <- fig.labels
    updateSelectInput(
      session,
      "sel.figure.figure",
      choices = fig.files ,
      label = paste(length(fig.files) , " figures")
    )
  })
  
  # show figure (PNG)
  
  output$figure.view <- renderImage({
    req(attached.edf() ,
        input$sel.figure.group  ,
        input$sel.figure.figure)
    filename <- file.path(nap.dir ,  values$ID , input$sel.figure.figure )
    req( file.exists( filename ) )
    list(src = filename)
  } , deleteFile = FALSE)
