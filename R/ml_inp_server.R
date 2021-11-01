
# helper input functions for AWS/ERIS deployment

set_query_values <- reactive({
  query<- parseQueryString(session$clientData$url_search)
  s_lst_path <- values$query[["slst"]]
  nap_path <-  values$query[["nap"]]
  if( ! is.null(s_lst_path) ) {
    fixed.sl <<- s_lst_path 
  }
  if ( ! is.null(nap_path) ) {
    nap.dir <<- nap_path
  }
  
  sl_pre_type <- values$query[["sl_prefix_type"]]

  sl_prefix <- values$query[["sl_prefix"]]

  if ( fixed.sl != "" && ! is.null(sl_pre_type) && ! is.null(sl_prefix) ) {
    sl_df <- read.delim( fixed.sl, header=FALSE )
    if ( sl_pre_type == "swap" ) {
      sl_to_swap <- values$query[["sl_prefix_swap"]]
      new_sl_df <- cbind(sl_df[1], lapply(sl_df[,2:ncol(sl_df)],function(x) gsub(sl_to_swap,sl_prefix,x)))
      write.table(new_sl_df,fixed.sl,sep="\t",col.names = FALSE, row.names = FALSE,quote=FALSE)
    }
    if(sl_pre_type =="add"){
      new_sl_df <- cbind(sl_df[1], lapply(sl_df[,2:ncol(sl_df)],function(x) paste(sl_prefix,x,sep="")))
      write.table(new_sl_df,fixed.sl,sep="\t",col.names = FALSE, row.names = FALSE,quote=FALSE)
    }
  }
})


#
# upload (or use fixed) sample-list
#

verify_token <- reactive({
  is_valid <- FALSE
  is_valid <- tryCatch({
    aes <- AES(enc_key, mode="CBC", enc_iv)
    decrypted <- strsplit(aes$decrypt(hex2raw(values$query[["token"]])),'\003')[[1]][1]
    if( !is.na(suppressWarnings(as.numeric(decrypted)))){
      token_time<-as.numeric(decrypted)
      curr_epoch= time_length(interval('1970-01-01 00:00:00 EDT', Sys.time()),"second")*1000
      if ((curr_epoch - token_time) >0 && (curr_epoch - token_time) < 86400000){
        return(TRUE)
      }
    }
  },error= function(e){
    return(FALSE)
  })
  return(is_valid)
})


if ( opt_eris ) {
  output$cohort <-renderUI( { selectInput("cohort", label= "Cohort", choices=list()) } )
  output$samplesLocal <- renderUI( { selectInput("samplesLocal",label="Sample List",choices = list()) } )
  dl <-read.delim2( file=metadata_lst, sep = '\t', header = FALSE, quote="" )
  updateSelectInput(session, "cohort", choices = dl[[1]] )
}

