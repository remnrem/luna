#
# keep state information here...
#

values <- reactiveValues()



# Upon session end,
# Reset global variables defined in app.R (for browser reload issues not to occur)
# Deattach EDF
# And delete files 

session$onSessionEnded(function(){
  opt_aws <<- Sys.getenv( "USE_S3" ) == "TRUE" || Sys.getenv( "USE_AWS" ) == "TRUE"
  if( SESSION_PATH !="" ){
    fixed.sl <<- paste( SESSION_PATH, SESSION_SLST , sep="/", collapse = NULL)
  } else {
    fixed.sl <<- ""
  }

  ldrop()
  annots_panel_present <<- TRUE
  staging_panel_present <<- TRUE
  pheno_panel_present <<- TRUE
  
  if ( opt_nap ) {
    sm_panel_present <<- TRUE
    if(opt_aws){
      req(aws.user,aws.runid)
      to_delete_dir = paste(getwd(),aws.user,aws.runid, sep= "/", collapse = NULL)
      unlink(to_delete_dir,recursive = TRUE)
    }
  }
})


#
# Show version and last build/time 
#

output$version_build_time <- renderText({
  print(paste("Build ",system("git log -1 | grep Date | cut -d':' -f2- | cut -d' ' -f5-8", intern = TRUE)))
})


observeEvent(input$cohort,{
  list_lst=dl[dl$V1 ==input$cohort ,]
  char_lst <- as.vector(t(list_lst))
  sampleLists=vector()
  for(lst in char_lst){
    if(lst !="" && lst !=input$cohort){
      sampleLists <- c(sampleLists,lst)
    }
  }
  updateSelectInput(session, "samplesLocal", choices=sampleLists)
})


observeEvent(input$samplesLocal,{
  req(input$samplesLocal)
  sl_path <-paste0(home_lst,input$samplesLocal,sep="")
  sl <- lsl(sl_path) 
  updateSelectInput(session , "edfs" , choices = names(sl))
  values$sl <- sl
  attached.edf()
}) 


# --------------------------------------------------------------------------------
#
# Load phenotypes
#
# --------------------------------------------------------------------------------

load_phenotypes <- function(){
  values$phenoData <- NULL
  pheno.files <- list.files(nap.dir, full.names = T , pattern = "^_pheno-.*.RData")
  # Remove phenotypes panel if no phenotype data files are available
  if(identical(pheno.files, character(0))){
    if(pheno_panel_present){
      removeTab("main_panel","Phenotypes",session = getDefaultReactiveDomain())
      pheno_panel_present <<-FALSE
    }
  } else{
    if(!pheno_panel_present){
      insertTab("main_panel",
                tabPanel( "Phenotypes" ,br(), dataTableOutput("pheno.table")),"Headers",
                position = "after", select = FALSE, session = getDefaultReactiveDomain())
      pheno_panel_present <<- TRUE
    }
  }
  pheno_tmpenv = new.env()
  invisible(lapply(pheno.files, load, envir = pheno_tmpenv))
  values$phenoData <- as.list(pheno_tmpenv)
  rm(pheno_tmpenv)
}


# --------------------------------------------------------------------------------
#
# Attach a sample list
#
# --------------------------------------------------------------------------------

attached.sl <- reactive({

  values$query <- parseQueryString( session$clientData$url_search )

# TODO : when is this needed?
  set_query_values()
  
  if ( fixed.sl != "" )
  {
    sl <- lsl( fixed.sl )
  }
  else if( length(values$query) == 0 )
  {
    output$samplesLocal <- renderUI({
      fileInput("samples", "Sample List" ,  accept = c( "lst" ) )
    })
    req( input$samples )
    sl <- lsl(input$samples$datapath) 
    opt_aws <<- FALSE
  }
  
  # AWS run-mode
  else
  {
    
    req(values$query[["user"]],values$query[["token"]])
    req(verify_token())
    opt_aws <<-TRUE
    isolate( { aws.user <<- values$query[["user"]] } )
    aws.runid <<- ""
    if ( ! is.null(values$query[["runid"]]) ){
      aws.runid <<- values$query[["runid"]]
    }
    
    keyV = paste( aws.user, aws.runid, "s.lst", sep = "/", collapse = NULL )

    final_keyV = gsub( "//" , "/" , keyV )
    
    index=1
    pre_val = paste( aws.user, aws.runid, sep = "/", collapse = NULL )

    s3_bucket <<- get_bucket( s3BucketName, prefix=pre_val )

    is_sl_file_present <- FALSE

    for (i in s3_bucket) {
      if ( i["Key"] == final_keyV ) {
        is_sl_file_present <- TRUE
        break
      }
      index= index+1
    }

    if( ! is_sl_file_present ) {
      showNotification("Sample list is missing, you may close the app",
                        duration = NULL, type = "error", session = getDefaultReactiveDomain())
    }

    req(is_sl_file_present)

    aws_sl_file <- save_object(s3_bucket[[index]], file=s3_bucket[[index]][["Key"]] )
    awl_sl_file_size <- file.info(aws_sl_file)$size

    if( awl_sl_file_size==0 ) {
      showNotification( "No EDFs are available for the project, you may close the app",
        duration = NULL, type = "error", session = getDefaultReactiveDomain())
    }

    req( awl_sl_file_size != 0 )
    sl <- lsl( aws_sl_file )
  }
  
  # update sample-list selector
  updateSelectInput(session , "edfs" , choices = names(sl), selected = FALSE)
  values$sl <- sl

  return(1)

})




# --------------------------------------------------------------------------------
#
# Attach an EDF : core load function
#
# --------------------------------------------------------------------------------

attached.edf <- reactive({

  if ( ! opt_eris ) {
    req( attached.sl() )
  }
  else {
    req( ! is.null(values$sl[input$edfs][[1]]) )
    sl_folder <- tolower(tools::file_path_sans_ext(input$samplesLocal))
    nap.dir <<- paste(base_output_dir,tolower(input$cohort),sl_folder, "nap", sep = "/", collapse = NULL)
  }


  load_phenotypes()
  
  req(input$edfs)
  
  if ( fixed.sl == "" && opt_aws) {
    nap.dir <<- paste( getwd(), aws.user, aws.runid, "nap", sep = "/", collapse = NULL)
    get_nap=TRUE
    nap_files <-paste(aws.user,aws.runid,"nap",input$edfs,sep = "/", collapse = NULL)
    for (file_name in values$sl[input$edfs][[1]]){
      file_index<-1
      for (f in s3_bucket){
        file_path=paste(aws.user,aws.runid,file_name,sep = "/", collapse = NULL)
        final_path=gsub("//","/",file_path)
        if (f[["Key"]] == final_path){
          save_object(s3_bucket[[file_index]],file=file_name, show_progress = TRUE)
        }
        if (grepl(nap_files,f[["Key"]]) && get_nap){
          save_object(s3_bucket[[file_index]],file=f[["Key"]], show_progress = TRUE)
        }
        file_index<-file_index+1
      }
      get_nap<-FALSE
    }
  }
  
  #
  # EDF ID
  #
  
  values$ID <- input$edfs

    
  #
  # lunaR to attach EDF from sample-list
  # either: swap in harm.lst (made on-th-fly to ensure path) OR use standard SL
  #
  
  if ( opt_nap && input$harmedf  )
  {
    harm.sl <- list()
    harm.sl[[ input$edfs ]]$EDF <- list.files( paste(nap.dir, values$ID , "data/" , sep = "/") ,
                                               full.names = T , pattern = "*harm.edf" )

    # this gets populated below w/ harm.lst anyway
    harm.sl[[ input$edfs ]]$ANNOT <-  character(0)

    # attach only if there was an EDF to be attached
    if ( length( harm.sl[[ input$edfs ]]$EDF ) != 0 )
      lattach( harm.sl , input$edfs )
  }
  else
  {
    if ( file.exists(  values$sl[[ input$edfs ]]$EDF ) )
      lattach( values$sl , input$edfs )
  }
  

  #
  # Set channels
  #

  x <- lchs()  
  names(x) <- x  
  values$channels <- x


  #
  # additional NAP annotations to attach?
  #
  
  if ( opt_nap )
  {
    nap.annots <- list.files( paste(nap.dir, values$ID , "annots/" , sep = "/") , 
                              full.names = T , pattern = "*.annot" )
    lapply(nap.annots, ladd.annot.file )
  }

  
  
  #
  # annotations
  #
  
  values$annots <- lannots()

  
  #
  # Remove Annotations panel if annotations are not available
  #
  
  if( identical(values$annots, character(0)) ){ 
    if( annots_panel_present ) {
      removeTab("main_panel","Annotations",session = getDefaultReactiveDomain())
      annots_panel_present <<- FALSE
    }
  } 
  else{
    if( ! annots_panel_present ){
      insertTab("main_panel",tabPanel( "Annotations" ,
                                       plotOutput("annot.view", width = '100%', height = "200px") ,
                                       br() , 
                                       tabsetPanel( tabPanel( "Summary" , tableOutput("annot.summary") ) ,
                                                    tabPanel( "Instances" , dataTableOutput("annot.table") ) ) ),
                "Signals",position = "before", select = FALSE, session = getDefaultReactiveDomain())
      annots_panel_present <<- TRUE
      
    }
  }
  
  values$annot.inst <- leval("ANNOTS")$ANNOTS
  
  
  #
  # epoch (fixed at 30 seconds)
  #
  
  values$ne <- lepoch()
  

  #
  # Staging information present (including SOAP/SUDS)?
  # 

  attach.staging()

  
  #
  # NAP derived metrics?
  #

  if ( opt_nap ) attach.nap.data()
  
  #
  # update control widgets
  #
    
  updateSelectInput(
    session,
    "sel.ch",
    choices = values$channels ,
    label = paste(length(values$channels) , "channels"),
    selected = 0
  )

  updateSelectInput(
    session,
    "sel.ann",
    choices = values$annots  ,
    label = paste(length(values$annots) , "annotations"),
    selected = 0
  )

  updateSelectInput(
    session,
    "disp.ann",
    choices = values$annots  ,
    label = paste(length(values$annots) , "annotations (list instances)"),
    selected = 0
  )


  #
  # queries EDF headers
  # 

  isolate( { 
     values$eval <- leval( "HEADERS & STAGE & HYPNO" )
    } )


  #
  # get SS (& aligned) 
  #
  
  values$ss         <- values$eval$STAGE$E
  
  values$ss.aligned <- leval("EPOCH align=W,N1,N2,N3,R & STAGE")$STAGE$E
  
  
  #
  # plot views (seconds)
  #
  
  values$epochs <- c(1,1)  
  values$zoom <- NULL 
  values$raw.signals <- T 
  
  
  #
  # SOAP tracker
  #

  values$soap.epoch <- 1
  values$soap <- NULL
  
  #
  # get channel units
  #
     
  isolate( {
    values$units <- values$eval$HEADERS$CH$PDIM
     names( values$units ) <- as.character( values$eval$HEADERS$CH$CH )
     } )
  
  
  return(1)
  
})



# 
# annot-instance list selector
#

observe( {
  req( values$annot.inst )
  flt <- values$annot.inst$ANNOT_INST_T1_T2$ANNOT %in% input$disp.ann
  if ( sum(flt)>0) {
    secs1  <- values$annot.inst$ANNOT_INST_T1_T2$START[ flt ]
    secs2  <- values$annot.inst$ANNOT_INST_T1_T2$STOP[ flt ]
    annot <- values$annot.inst$ANNOT_INST_T1_T2$ANNOT[ flt ]
    #      inst <- values$annot.inst$ANNOT_INST_T1_T2$INST[ flt ]
    vals <- paste( annot , secs1 , sep=": " )
    inst <- as.list( paste( secs1, secs2 ) )
    names( inst ) <- vals
    if ( length(secs1)>0 ) inst <- inst[ order( secs2 ) ] 
    updateSelectInput(
      session,
      "sel.inst",
      choices = inst , 
      label = paste(length(secs1) , " instances," , length(input$disp.ann) , "annotations"),
      selected = 0
    )
  }
})


observeEvent(input$button_prv,{
  req(attached.edf())
  curr_index <- match(values$ID, names(values$sl))
  if(curr_index > 1){
    updateSelectizeInput(session, "edfs",choices = names(values$sl), selected =names(values$sl[curr_index-1]))
  }
  
})


observeEvent(input$button_nxt,{
  req(attached.edf())
  curr_index <- match(values$ID, names(values$sl))
  if(curr_index < length(values$sl)){
    updateSelectizeInput(session, "edfs",choices = names(values$sl), selected =names(values$sl[curr_index+1]))
  }
  
})

