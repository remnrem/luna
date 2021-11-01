
# helper functions for working with NAP derived data


attach.nap.data <- function()
{


#
# NAP issues
#

  #
  # NAP issues (nap/{id}/nap.issues)
  #

  df <- data.frame( Issue = character() , Channel = character() , Notes = character() )
  if ( opt_nap ) {
    issues.filename <- paste( nap.dir, values$ID , "nap.issues" , sep = "/")
    if ( file.exists( issues.filename ) ) {
      df <-  read.table( issues.filename , header=F , stringsAsFactors=F ) 
      names( df ) <- c( "Issue" , "Channel" , "Notes" ) 
    }
  }
  values$issuesData <- df




#
# any NAP tables?
#

  nap.files <-
    list.files(paste(nap.dir, values$ID , sep = "/") ,
               full.names = T ,
               pattern = "*-tab.RData")
  cat("dir" , paste(nap.dir, values$ID , sep = "/") , "\n")
  print(nap.files)
  tmpenv = new.env()
  invisible(lapply(nap.files, load, envir = tmpenv))
  isolate( { values$data <- as.list(tmpenv) } )
  rm(tmpenv)
  groups <- unlist(lapply(values$data , "[[" , "desc"))
  d.groups <- as.list(names(groups))
  names(d.groups) <- unlist(groups)

  updateSelectInput(
    session,
    "sel.table.group",
    choices = d.groups  ,
    label = paste(length(d.groups) , " groups")
  )
  
  
  #
  # any NAP figures?
  #

  nap.files <-
    list.files(paste(nap.dir, values$ID , sep = "/") ,
               full.names = T ,
               pattern = "*-fig.RData")
  tmpenv = new.env()
  invisible(lapply(nap.files, load, envir = tmpenv))
  values$figures <- as.list(tmpenv)
  rm(tmpenv)
  groups <- unlist(lapply(values$figures , "[[" , "desc"))
  d.groups <- as.list(names(groups))
  names(d.groups) <- unlist(groups)

  updateSelectInput(
    session,
    "sel.figure.group",
    choices = d.groups,
    label = paste(length(d.groups) , " groups")
  )
  
  #    
  # Any dervied metrics? (only attach once) 
  #
  
  derived.files <- list.files(nap.dir, full.names = T , pattern = "^_derived-.*.RData")
  
  if ( opt_nap && length( values$derived_data ) == 0 ) {
    
    if ( identical( derived.files, character(0) ) ) {
      if ( sm_panel_present ) {
        removeTab( "main_panel", "Metrics", session = getDefaultReactiveDomain() )
        sm_panel_present <<- FALSE
      }
    } 
    else {
      if ( ! sm_panel_present ) {
        appendTab("main_panel",
                  tabPanel("Metrics",
                           fluidRow(
                             column(width = 4,
                                    verticalLayout(selectInput("sel.derived.group", label="Group", choices=list() ),
                                                   selectInput("sel.derived.table", label="Sample metrics", choices=list()),
                                                   selectInput("sel.derived.variables", label="Variables", choices = list()), fluid = T)),
                             column(width = 8, plotOutput("var_plot",height="200px") )),
                           br(),
                           verbatimTextOutput('selected.info'),
                           dataTableOutput("derived.view")), select = FALSE, session = getDefaultReactiveDomain())
        sm_panel_present <<- TRUE
      }
    }
    

    #
    # attach derived data 
    #

    derived_tmpenv = new.env()
    invisible(lapply(derived.files, load, envir = derived_tmpenv))
    values$derived_data <- as.list(derived_tmpenv)
    rm(derived_tmpenv)
    derived_groups <- unlist(lapply(values$derived_data , "[[" , "desc"))
    d.derived_groups <- as.list(names(derived_groups))
    names(d.derived_groups) <- unlist(derived_groups)
    
    updateSelectInput(
      session,
      "sel.derived.group",
      choices = d.derived_groups  ,
      label = paste(length(d.derived_groups) , " group(s)")
    )
    #sm_allowChangeSelection <<-TRUE
  }
  sm_allowChangeSelection <<-TRUE
  
#  cat( "DERIVED DATA\n")
#  print( values$derived_data )

    #
    # MTM images                                                                                              
    #
    
    values$mtm.files <- list.files( paste( nap.dir, values$ID , sep="/" ) ,
                                    full.names = T , pattern = glob2rx( "mtm-*.png" ) )
    
    
    #
    # attach pre-computed summary PSD, if exists
    #
    
    values$spsd <- NULL
    spsd.filename <- paste(nap.dir, values$ID , "nap.spsd.RData" , sep = "/")
    if ( file.exists( spsd.filename ) ) {
      # loads 'spsd'
      load( spsd.filename )  
      values$spsd <- spsd
      rm( spsd )
    }
    
    #
    # attach pre-computed sigstats, if exists
    # (decoding with these are original or harmonized versions
    #
    
    values$sigstats <- NULL    
    sigstats.filename <- ifelse( input$harmedf , "nap.harm.sigstats.RData" , "nap.sigstats.RData" )     
    sigstats.filename <- paste(nap.dir, values$ID , sigstats.filename , sep = "/")
    if ( file.exists( sigstats.filename ) ) {
      # loads 'sigstats'
      load( sigstats.filename )
      if ( input$harmedf ) {
        values$sigstats <- harm.sigstats
        rm( harm.sigstats )
      } else {
        values$sigstats <- sigstats
        rm( sigstats )
      }
    }
    
    
    # NAP channel and annotation harmonizations
    #  tables: channel.mapping1, channel.mapping2  -> sigs.harm.map1 , sigs.harm.map2
    #        : annot.mapping1, annot.mapping2      -> annot.alias , annot.map
    
    chmap1.filename <- paste( nap.dir, values$ID , "nap.sig.map1.RData" , sep = "/")
    chmap2.filename <- paste( nap.dir, values$ID , "nap.sig.map2.RData" , sep = "/")
    
    bchmap1.filename <- paste( nap.dir, values$ID , "nap.sig.base.map1.RData" , sep = "/")
    bchmap2.filename <- paste( nap.dir, values$ID , "nap.sig.base.map2.RData" , sep = "/")
    
    amap1.filename <- paste( nap.dir, values$ID , "nap.annot.map.RData" , sep = "/")
    amap2.filename <- paste( nap.dir, values$ID , "nap.annot.alias.RData" , sep = "/")
    
    values$chmap1 <- values$chmap2 <- NULL
    values$bchmap1 <- values$bchmap2 <- NULL
    values$amap1 <- values$amap2 <- NULL
    
    # harmonized EDF channels
    
    if ( file.exists( chmap1.filename ) ) {
      load( chmap1.filename ) ; values$chmap1 <- sigs.harm.map1 ; rm( sigs.harm.map1 )
    }
    
    if ( file.exists( chmap2.filename ) ) {
      load( chmap2.filename ) ; values$chmap2 <- sigs.harm.map2 ; rm( sigs.harm.map2 )
    }
    
    # base EDF channels
    
    if ( file.exists( bchmap1.filename ) ) {
      load( bchmap1.filename ) ; values$bchmap1 <- sigs.base.map1 ; rm( sigs.base.map1 )
    }
    
    if ( file.exists( bchmap2.filename ) ) {
      load( bchmap2.filename ) ; values$bchmap2 <- sigs.base.map2 ; rm( sigs.base.map2 )
    }
    
    # annotations
    
    if ( file.exists( amap1.filename ) ) {
      load( amap1.filename ) ; values$amap1 <- annot.map ; rm( annot.map )
    }
    
    if ( file.exists( amap2.filename ) ) {
      load( amap2.filename ) ; values$amap2 <- annot.alias ; rm( annot.alias )
    }
    
    

}
