


attach.staging <- function() {

#
  # has manual staging?
  #
  
  values$has_manual_staging <- ! is.null( lstages() )
    
  cat ( "values$has_manual_staging" , values$has_manual_staging , "\n" ) 
  
  #
  # has automated staging? (luna_suds_SUDS-)
  #
  
  values$has_suds_staging <- ! is.null( values$data$luna_suds_SUDS )
  values$has_soap_staging <- ! is.null( values$data$luna_suds_SOAP )
  
  #
  # if neither manual nor SUDS staging info available, remove Staging Panel completely
  #
  
  if ( ! ( values$has_manual_staging | values$has_soap_staging | values$has_suds_staging ) )
  {
    if ( staging_panel_present )
    {
      removeTab( "main_panel", "Staging", session = getDefaultReactiveDomain() )
      staging_panel_present <<- FALSE
    }
  }
  else
  {	
    if ( ! staging_panel_present )
    {
      # if not running in NAP-mode, we will only have manual staging
      
      if ( ! opt_nap )
      {
        insertTab( "main_panel",
                   tabPanel( "Staging", br() ,
                             textOutput("stage.num.epochs"), hr(),
                             plotOutput("stage.view", width='100%', height="100px"), hr(),
                             tableOutput("stage.summary") ) ,
                   "Headers" , position = "after", select = FALSE, session = getDefaultReactiveDomain() ) 
      } else {
        
        # here, in NAP-mode, we may have
        #  Manual staging, SOAP and SUDS
        #  No manual staging, and only SUDS
        #  (unlikely, but possible?): only Staging, i.e. manual staging, but no EEG available
        
        if ( values$has_manual_staging & values$has_soap_staging & values$has_suds_staging )
        {
          insertTab("main_panel",
                    tabPanel( "Staging",
                              tabsetPanel(
                                tabPanel( "Manual", br(),
                                          textOutput("stage.num.epochs"), hr(),
                                          plotOutput("stage.view", width='100%', height="100px"), hr(),
                                          tableOutput("stage.summary") ),
                                tabPanel( "SOAP", 
                                          plotOutput("soap.view.orig", width='100%', height="100px"),
                                          plotOutput("soap.view.hypno",  width='100%', height="100px"),
                                          plotOutput("soap.view.prob", width='100%', height="100px"),
                                          plotOutput("soap.view.stgdur", width='100%', height="250px"),
                                          hr(), 
                                          verbatimTextOutput("soap.summary")),
                                tabPanel( "SUDS", 
                                          plotOutput("suds.view.orig", width='100%', height="100px"),
                                          plotOutput("suds.view.hypno", width='100%', height="100px"),
                                          plotOutput("suds.view.prob", width='100%', height="100px"),
                                          plotOutput("suds.view.stgdur", width='100%', height="250px"),
                                          hr(), 
                                          verbatimTextOutput("suds.summary")) ) ) ,
                    "Headers",position = "after", select = FALSE, session = getDefaultReactiveDomain() )
        }
        
        if ( values$has_suds_staging & ! values$has_manual_staging )
        {
          
          insertTab("main_panel",
                    tabPanel( "SUDS", 
                              plotOutput("suds.view.orig", width='100%', height="100px"),
                              plotOutput("suds.view.hypno", width='100%', height="100px"),
                              plotOutput("suds.view.prob", width='100%', height="100px"),
                              plotOutput("suds.view.stgdur", width='100%', height="250px"),
                              hr(), 
                              verbatimTextOutput("suds.summary"))
                    ,"Headers",position = "after", select = FALSE, session = getDefaultReactiveDomain())
        }
      }
      
      # denote that we now have staging available
      staging_panel_present <<- TRUE
    }}
  

}