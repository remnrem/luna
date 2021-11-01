

#
# pallele w/ 10 distinctive colours
#

pal10 <- c(
  rgb(255,88,46,max=255) ,
  rgb(1,56,168,max=255),
  rgb(177,212,18,max=255),
  rgb(255,128,237,max=255),
  rgb(1,199,86,max=255),
  rgb(171,0,120,max=255),
  rgb(85,117,0,max=255),
  rgb(251,180,179,max=255),
  rgb(95,32,0,max=255),
  rgb(164,209,176,max=255)
  )


#
# Define UI for LunaR interdace
#

ui <- fluidPage(

  tags$script('
    $(document).on("keydown", function (e) {
       Shiny.onInputChange("soap.keypress", e.which);
    });
  '),
  
  dashboardPage(  
    
    #
    # Application title
    #
    
    dashboardHeader( title = "Luna | Moonlight | NAP" ),
    
    dashboardSidebar(
      verbatimTextOutput("version_build_time"),
      uiOutput("cohort"),
      uiOutput("samplesLocal"),      
      selectizeInput("edfs", "Samples",options= list(maxOptions = 20000),list()),
      fluidRow(column(width=2,actionButton("button_prv", "<")),
               column(width=2,offset=0,actionButton("button_nxt", ">")),
               column(width=6,offset=1,checkboxInput("harmedf", "Harmonized", TRUE) ) 
                ),
      selectInput( "sel.ch", "Channels" , list(), multiple = TRUE, selectize = TRUE ),      
      selectInput( "sel.ann", "Annotations" , list(), multiple = TRUE, selectize = TRUE ),
#      selectInput( "sel.epoch", "Epochs" , list(), multiple = FALSE ,  selectize = TRUE ),
      br(), hr(),       
      selectInput( "disp.ann", "Annotations (list instances)" , list(), multiple = TRUE, selectize = TRUE ),      
      selectInput( "sel.inst", "Instances" , list(), multiple = TRUE ,  selectize = FALSE )
      ),
    
    
    dashboardBody( tabsetPanel(id = "main_panel",
      
      tabPanel( "NAP", actionButton("refresh_nap_log", "Reload NAP log"),
               br(),
	       verbatimTextOutput("logger", placeholder = TRUE),
	       uiOutput("errLogsButton"),br(), uiOutput("errLogs") ),
      	 
      tabPanel( "Headers",
           tabsetPanel(
             tabPanel( "EDF" , br() , tableOutput("header.summary") , br(), dataTableOutput("header.channels") ) ,

             tabPanel( "Harmonized EDF" , fluidRow(
                      column(width = 6, h4("Harmonized channels"), tableOutput("channel.mapping1" )),
                      column(width = 2 ) , 
                      column(width = 3, h4("Unmapped channels"), tableOutput("channel.mapping2" )) ) ) , 

             tabPanel( "Base EDF" , fluidRow(
                      column(width = 6, h4("Canonical channels"), tableOutput("channel.base.mapping1" )),
                      column(width = 2 ) , 
                      column(width = 3, h4("Unselected channels"), tableOutput("channel.base.mapping2" )) ) ) , 

             tabPanel( "Annotations" , fluidRow(
                      column(width = 6, h4("Harmonized annotations"), tableOutput("annot.mapping1" )),
                      column(width = 1 ) , 
                      column(width = 5, h4("Aliases"), tableOutput("annot.mapping2" )) ) ) 
	    )
      ),


      tabPanel( "Phenotypes", br(), dataTableOutput("pheno.table")),

      tabPanel( "Staging",
      		tabsetPanel(
                 tabPanel( "Manual", br(),                            
                           plotOutput("stage.view", width='100%', height="100px"), 
                           hr(), 
                           tableOutput("stage.summary") ),

                 tabPanel( "SOAP", 
                           plotOutput("soap.view.orig", width='100%', height="100px"),
			   plotOutput("soap.view.hypno",  width='100%', height="100px"),
			   plotOutput("soap.view.prob", width='100%', height="100px"),
			   plotOutput("soap.view.stgdur", width='100%', height="250px"),
			   hr(), 
			   verbatimTextOutput("soap.summary")
                 ),

                 tabPanel( "SUDS", 
		 	   plotOutput("suds.view.orig", width='100%', height="100px"),
			   plotOutput("suds.view.hypno", width='100%', height="100px"),
                           plotOutput("suds.view.prob", width='100%', height="100px"),
			   plotOutput("suds.view.stgdur", width='100%', height="250px"),
			   hr(), 
                           verbatimTextOutput("suds.summary")
                 )
      		) ) ,

      tabPanel( "Annotations" ,
                plotOutput("annot.view", width = '100%', height = "200px") ,
                br() , 
                tabsetPanel( tabPanel( "Summary" , tableOutput("annot.summary") ) , tabPanel( "Instances" , dataTableOutput("annot.table") ) ) ),
      
      tabPanel( "Signals",   
                fluidRow(column(width=1,offset=0,actionButton("button_epoch_prv", " < Prev")),		         
			 column(width=1,actionButton("button_epoch_nxt", "Next > ")), 
                         column(width=1,offset=0,actionButton("entire.record", "All")),
                         column(width=9,verbatimTextOutput("info2") ) )  ,
                plotOutput( "signal.master", width='100%', height="30px", click="master_click", dblclick="master_dblclick", 
                            brush = brushOpts( id="master_brush", direction="x", resetOnNew=F ) ), 
                plotOutput( "signal.master2", width='100%', height="10px" ),
                br() , plotOutput( "signal.spsd", width='100%', height="100px" ),
		plotOutput( "signal.view" , width='100%', height="50vh", dblclick="zoom_dblclick", 
                            brush = brushOpts( id="zoom_brush", direction="x", resetOnNew=F ) )
	),
		
      # to resize the plot dynamically, uiOutput() rather than plotOutput()
      tabPanel( "Spectral",
                sliderInput("sel.freq", "Frequency (Hz)", width = '100%', min=0, max=100, step=0.25, value=c(0.25, 35) ) ,
      	        uiOutput('ui_psdplot') ),

      tabPanel( "MTM", uiOutput('ui_mtmplot') ) ,

      tabPanel( "Issues", br(), dataTableOutput("issue.table")),

      tabPanel( "Tables",
                selectInput("sel.table.group", label="Group", choices=list() ),
                selectInput("sel.table.table", label="Table", choices=list()),
                hr(),
                dataTableOutput("table.table") ),
      
      tabPanel( "Figures",
                selectInput("sel.figure.group", label="Group", choices=list() ),
                selectInput("sel.figure.figure", label="Figure", choices=list() ),
                hr(),
                imageOutput("figure.view") ),
		
       tabPanel("Metrics",		           
		fluidRow(
		column(width = 4,
		 verticalLayout(selectInput("sel.derived.group", label="Group", choices=list() ),
                                selectInput("sel.derived.table", label="Sample metrics", choices=list()),
		                selectInput("sel.derived.variables", label="Variables", choices = list()), fluid = T)),
		  column(width = 8, plotOutput("var_plot",height="200px") )),
		br(),
		verbatimTextOutput('selected.info'),
		dataTableOutput("derived.view"))
		           
      
    )  # tabsetpanel
    
    ) #dashboardBody
    ) #dashboardPage
) # fluidPage
