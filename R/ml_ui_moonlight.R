

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
  
  dashboardPage(  
    
    #
    # Application title
    #
    
    dashboardHeader( title = "Luna | Moonlight" ),
    
    dashboardSidebar(
      verbatimTextOutput("version_build_time"),
      uiOutput("samplesLocal"),      
      selectInput( "edfs", label = "Samples", choices = list() ) ,  
      fluidRow(column(width=5,actionButton("button_prv", "previous")),column(width=5,offset=2,actionButton("button_nxt", "next"))),
      selectInput( "sel.ch", "Channels" , list(), multiple = TRUE, selectize = TRUE ),      
      selectInput( "sel.ann", "Annotations" , list(), multiple = TRUE, selectize = TRUE ),
#      selectInput( "sel.epoch", "Epochs" , list(), multiple = FALSE ,  selectize = TRUE ),
      br(), hr(),       
      selectInput( "disp.ann", "Annotations (list instances)" , list(), multiple = TRUE, selectize = TRUE ),      
      selectInput( "sel.inst", "Instances" , list(), multiple = TRUE ,  selectize = FALSE )
    ),
    
    
    dashboardBody( tabsetPanel(id = "main_panel",
      
      tabPanel( "Headers", br() , tableOutput("header.summary") , br(), dataTableOutput("header.channels") ),
      
#      tabPanel( "Phenotypes", br(), dataTableOutput("pheno.table")),
      
      tabPanel( "Staging", br() , textOutput("stage.num.epochs"), hr(), plotOutput("stage.view", width='100%', height="100px"), hr(), tableOutput("stage.summary") ) , 
      
      tabPanel( "Annotations" ,
                plotOutput("annot.view", width = '100%', height = "200px") ,
                br() , 
                tabsetPanel( tabPanel( "Summary" , tableOutput("annot.summary") ) , tabPanel( "Instances" , dataTableOutput("annot.table") ) ) ),
      
      tabPanel( "Signals",
                actionButton("entire.record", "Entire record"),
                verbatimTextOutput("info2"),
                plotOutput( "signal.master", width='100%', height="30px", click="master_click", dblclick="master_dblclick", 
                            brush = brushOpts( id="master_brush", direction="x", resetOnNew=F ) ), 
                plotOutput( "signal.master2", width='100%', height="10px" ),
                br(),
                plotOutput( "signal.view" , width='100%', height="50vh", dblclick="zoom_dblclick", 
                            brush = brushOpts( id="zoom_brush", direction="x", resetOnNew=F ) ),
                br(),
                fluidRow(column(width=1,offset=5,actionButton("button_epoch_prv", "previous")),column(width=1,actionButton("button_epoch_nxt", "next")))), 
      
      # to resize the plot dynamically, uiOutput() rather than plotOutput()
      tabPanel( "Spectral",
                sliderInput("sel.freq", "Frequency (Hz)", width = '100%', min=0, max=100, step=0.25, value=c(0.25, 35) ) ,
                uiOutput('ui_psdplot') )
      
    )  # tabsetpanel
    
    ) #dashboardBody
  ) #dashboardPage
)
