#
# Derived Metrics tab
#

# Variables for storing derived data and pre-selected row name
pre_select_rowname <- NULL
derived_data <- NULL

# assuming first column will be the ID col
ID_col_index <- 1

# update derived tables depending on group

observe({
  req(input$sel.derived.group)

  # extract names/desc for the tables in this group (skipping the group-level desc)
  # i.e. everyhting other than 'desc' keyword in the list is assumed to be a list(desc,data) object

 derived_tables <-
    lapply(values$derived_data[[input$sel.derived.group]][names(values$derived_data[[input$sel.derived.group]]) != "desc"] ,
           "[[" , "desc")
  d.derived_tables <- as.list(names(derived_tables))
  names(d.derived_tables) <- unlist(derived_tables)
  
  updateSelectInput(
    session,
    "sel.derived.table",
    choices = d.derived_tables  ,
    label = paste(length(d.derived_tables) , " table(s)")
  )
})

observe({
  req(input$sel.derived.table)
  col_names  <-
    colnames(values$derived_data[[input$sel.derived.group]][[input$sel.derived.table]]$data)
  if(!is.null(col_names) && length(col_names)>1){
    if("DISP_ID" %in% col_names){
      disp_id_col_index <- match("DISP_ID",col_names)
      col_names <- col_names[-c(1,disp_id_col_index)]
    } else{
      col_names <- col_names[-1]
    }
  }
  updateSelectInput(
    session,
    "sel.derived.variables",
    choices = col_names  ,
    label = paste(length(col_names) , " variable(s)")
  )
})


output$derived.view <- DT::renderDataTable(DT::datatable({
  req(attached.edf() ,
      input$sel.derived.group  ,
      input$sel.derived.table)
  derived_data <<-
    values$derived_data[[input$sel.derived.group]][[input$sel.derived.table]]$data
  col_names_dt <- colnames(derived_data)
  ID_col=colnames(derived_data)[ID_col_index]
  pre_select_row_index <<-which(derived_data[ID_col] == values$ID)
  if("DISP_ID" %in% col_names_dt) derived_data <-subset(derived_data, select=-ID)
  derived_data
},
rownames = F ,
selection = list(mode = 'single', selected = list(rows=c(pre_select_row_index),cols=c()), target = 'row+column'),
options = list(
  pageLength = 5 ,
  lengthMenu = list(c(5, 10,25, -1), c("5", "10" , "25" , "All")) ,
  columnDefs = list(list(
    className = "dt-center", targets = "_all"
     ))  
)))


output$var_plot <- renderPlot({
  req(input$sel.derived.table,input$sel.derived.variables!="")
  ID_col <- colnames(derived_data)[ID_col_index]
  selected_var <- input$sel.derived.variables
  plot_val <- as.numeric( derived_data[ , selected_var] )
  row_val  <- as.numeric( derived_data[ derived_data[,ID_col] == values$ID , selected_var ] )
  nu <- length( unique( plot_val[ ! is.na(plot_val) ] ) ) 
  if ( nu < 10 ) {
    tt <- table( plot_val )
    cols <- rep( rgb(0,0,100,100,max=255) , length(tt) )
    cols[ names(tt) == row_val ] <- "red" 
    barplot( tt , main = "" , xlab = selected_var , col = cols )
  }
  if ( nu >= 10 ) {
    hist( plot_val, main = "" , xlab = selected_var , breaks=20 , col = rgb(0,0,100,100,max=255))
    abline( v = row_val , lwd=5 , col="red" )
  }  
})


# Load EDF if selected row's ID field value is present in cohort
observeEvent(input$derived.view_rows_selected,{
  ID_col <- colnames(derived_data)[ID_col_index]
  ID_col_data <-derived_data[ID_col]
  selected_edf <-ID_col_data[input$derived.view_rows_selected, ]
  req((selected_edf != values$ID),sm_allowChangeSelection)
  sm_allowChangeSelection <<- FALSE
  withProgress(message="if available, selection will be changed...",{
  if(selected_edf %in% names(values$sl)){
    updateSelectInput(session , "edfs" , selected =selected_edf )
  } else{
    sm_allowChangeSelection <<- TRUE
  } 
    req(sm_allowChangeSelection)
  })
})

output$selected.info <- renderPrint({
  if( ! is.null(input$derived.view_columns_selected ) ){
    cat("Plotting chart for column: ")
    col_names <- colnames(derived_data)
    cat(col_names[input$derived.view_columns_selected+1])
    cat("\n")
  }

  if( ! is.null( input$derived.view_rows_selected ) ){
   new_id <- derived_data[input$derived.view_rows_selected , ID_col_index]
   if ( any( new_id %in% names(values$sl) ) ) 
       cat( "Attaching new EDF: " , new_id )
   else
       cat( "Could not find" , new_id , "in the attached sample list")
}

})


