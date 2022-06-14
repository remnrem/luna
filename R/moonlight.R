
# --------------------------------------------------------------------------------
#
# lunaR moonlight viewer
# v0.04, 24-Feb-2022
# http://zzz.bwh.harvard.edu/luna/
#
# --------------------------------------------------------------------------------

requireNamespace("shiny", quietly = T)
requireNamespace("DT", quietly = T)
requireNamespace("shinyFiles", quietly = T)
requireNamespace("xtable", quietly = T)
requireNamespace("shinydashboard", quietly = T)

# --------------------------------------------------------------------------------
#
# globals
#
# --------------------------------------------------------------------------------

ml.globals <- new.env()

ml.globals$pal10 <- c(
  rgb(255, 88, 46, max = 255),
  rgb(1, 56, 168, max = 255),
  rgb(177, 212, 18, max = 255),
  rgb(255, 128, 237, max = 255),
  rgb(1, 199, 86, max = 255),
  rgb(171, 0, 120, max = 255),
  rgb(85, 117, 0, max = 255),
  rgb(251, 180, 179, max = 255),
  rgb(95, 32, 0, max = 255),
  rgb(164, 209, 176, max = 255)
)

# derived data/rows
ml.globals$pre_select_rowname <- NULL
ml.globals$derived_data <- NULL
ml.globals$ID_col_index <- 1
ml.globals$show_detailed_logs_butn <- shiny::reactiveVal(FALSE)

ml.globals$annots_panel_present <- TRUE
ml.globals$staging_panel_present <- TRUE
ml.globals$pheno_panel_present <- TRUE

ml.globals$sm_panel_present <- TRUE
ml.globals$sm_allowChangeSelection <- TRUE


# --------------------------------------------------------------------------------
#
# wrapper function to set up and initiate the moonlight shiny app
#  either in basic or NAP mode
#  (in the latter case, we expect a "nap/" folder in the project directory)
#
# --------------------------------------------------------------------------------


#' Initiate the Moonlight viewer in a browser window
#'
#' @param sample.list sample list (defaults to \code{s.lst})
#' @param proj.path working folder for Moonlight (default current)
#' @param nap.mode boolean value to indicate whether to expect NAP output (default F)
#' @param environ.file if non-NULL, specify a file with environment variables
#' @param local boolean value to indicate whether running locally (versus Docker container) (default T)
#'
#' @export
#'
#' @importFrom shiny renderPlot renderTable renderImage renderText appendTab updateSelectizeInput observe incProgress withProgress getDefaultReactiveDomain showNotification isolate fileInput getDefaultReactiveDomain insertTab removeTab parseQueryString selectInput renderUI showModal updateSelectInput removeModal observeEvent actionButton modalButton tagList tags div textInput modalDialog reactive req reactiveValues textOutput verticalLayout imageOutput sliderInput brushOpts plotOutput h4 dataTableOutput tableOutput verbatimTextOutput tabPanel tabsetPanel fluidPage uiOutput selectizeInput fluidRow column actionButton checkboxInput selectInput br hr
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody
#' @importFrom digest AES
#' @importFrom wkb hex2raw
#' @importFrom lubridate time_length interval
#' @importFrom utils read.delim2 read.delim write.table glob2rx tail
#' @importFrom aws.s3 get_bucket save_object
#' @importFrom DT renderDataTable
#' @importFrom graphics par axis rect text points image barplot lines legend abline polygon
moonlight <- function(sample.list = NULL,
                      proj.path = NULL,
                      nap.mode = FALSE,
                      environ.file = NULL,
                      local = T) {



  # --------------------------------------------------------------------------------
  #
  # Global variables for this shiny instance
  #
  # --------------------------------------------------------------------------------

  options(shiny.sanitize.errors = FALSE)
  # options( shiny.reactlog=TRUE )

  SESSION_SLST <- "s.lst"
  SESSION_PATH <- "."


  #
  # Deployment options to configure a server-based instance
  #
  opt_aws <- F
  opt_eris <- F
  opt_local_storage <- F
  use_url_auth <- F
  use_access_code <- F


  #
  # If using environment variables to configure a server-based instance
  #

  if (!is.null(environ.file)) {
    readRenviron(environ.file)

    if (Sys.getenv("USE_NAP") == "TRUE") opt_nap <- T
    if (Sys.getenv("USE_AWS_S3") == "TRUE") opt_aws <- T

    # i.e. allow moonlight() args to over-ride environment variables:
    if (is.null(sample.list)) SESSION_SLST <- Sys.getenv("SESSION_SLST")
    if (is.null(proj.path)) SESSION_PATH <- Sys.getenv("SESSION_PATH")
  }

  # or, over-ride by moonlight() args
  if (!is.null(sample.list)) SESSION_SLST <- sample.list
  if (!is.null(proj.path)) SESSION_PATH <- proj.path
  if (!is.null(nap.mode)) opt_nap <- nap.mode




  # --------------------------------------------------------------------------------
  #
  # ERIS Deployment : i.e. hard-coded values for local (BWH/HMS) NSRR instance
  #
  # --------------------------------------------------------------------------------

  if (Sys.getenv("ON_ERIS") == "TRUE") opt_eris <- T

  # nb. ERIS

  eris.metadata_lst <- "/home/shiny/nsrr-dataset/luna-link/metadata_lst.txt"
  eris.home_lst <- "/home/shiny/nsrr-dataset/luna-link/nap_sl/"
  eris.base_output_dir <- "/home/shiny/nsrr-dataset/luna-link/output"

  # ERIS implies NAP mode
  if (opt_eris) opt_nap <- T

  # ERIS implies not AWS
  if (opt_eris & opt_aws) stop("Cannot proceed with AWS mode with ERIS deployment")

  # Local storage (non-ERIS)- Multi-cohort, Multi-Samplelists and their NAP output paths
  if (Sys.getenv("LOCAL_STORAGE") == "TRUE") opt_local_storage <- T
  local_metadata_lst <- Sys.getenv("LOCAL_METADATA_LST")
  local_home_lst <- Sys.getenv("LOCAL_HOME_LST")
  local_base_output_dir <- Sys.getenv("LOCAL_BASE_OUTPUT_DIR")


  # FIX

  if (SESSION_PATH != "" && !opt_aws) {
    fixed.sl <- paste(SESSION_PATH, SESSION_SLST, sep = "/", collapse = NULL)
  } else {
    fixed.sl <- ""
  }


  #
  # Point to NAP output directory, where we look for any tables/figures under nap.dir/{id}/
  #

  nap.dir <- paste(SESSION_PATH, "nap/", sep = "/", collapse = NULL)

  cat("nap.dir", nap.dir, "\n")


  ##
  ## Variables for Auth based on Query String from URL
  ##

  if (Sys.getenv("USE_URL_AUTH") == "TRUE") use_url_auth <- T

  if (use_url_auth) {
    requireNamespace("lubridate", quietly = T)
    requireNamespace("wkb", quietly = T)
    requireNamespace("digest", quietly = T)
    enc_key <- charToRaw(Sys.getenv("ENCRYPT_KEY"))
    enc_iv <- charToRaw(Sys.getenv("ENCRYPT_IV"))
    token_exp_time <- Sys.getenv("TOKEN_EXPIRY_MINUTES")
  }


  ##
  ## Access Code variables to load the Samples only
  ## after the correct access code is provided by application user
  ##

  if (Sys.getenv("USE_ACCESS_CODE") == "TRUE") use_access_code <- T

  if (use_access_code) {
    access_code <- Sys.getenv("ACCESS_CODE")
  }





  # --------------------------------------------------------------------------------
  #
  # AWS deployment
  #
  # --------------------------------------------------------------------------------

  if (opt_aws) {
    requireNamespace("aws.s3", quietly = T)
    s3BucketName <- Sys.getenv("AWS_S3_BUCKET_NAME")
    AWS_ACCESS_KEY_ID <- Sys.getenv("AWS_ACCESS_KEY_ID")
    AWS_SECRET_ACCESS_KEY <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
    AWS_DEFAULT_REGION <- Sys.getenv("AWS_DEFAULT_REGION")
    aws.user <- ""
    aws.runid <- ""
    aws.cid <- ""
    s3_bucket <- ""
  }



  # --------------------------------------------------------------------------------
  #
  # UI: either core moonlight, or w/ additional NAP panels
  #
  # --------------------------------------------------------------------------------

  if (opt_nap) {
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

    dashboardHeader(title = "Luna | Moonlight | NAP"),
    dashboardSidebar(
      uiOutput("cohort"),
      uiOutput("samplesLocal"),
      selectizeInput("edfs", "Samples", options = list(maxOptions = 20000), list()),
      fluidRow(
        column(width = 2, actionButton("button_prv", "<")),
        column(width = 2, offset = 0, actionButton("button_nxt", ">")),
        column(width = 6, offset = 1, checkboxInput("harmedf", "Harmonized", TRUE))
      ),
      selectInput("sel.ch", "Channels", list(), multiple = TRUE, selectize = TRUE),
      selectInput("sel.ann", "Annotations", list(), multiple = TRUE, selectize = TRUE),
      #      selectInput( "sel.epoch", "Epochs" , list(), multiple = FALSE ,  selectize = TRUE ),
      br(), hr(),
      selectInput("disp.ann", "Annotations (list instances)", list(), multiple = TRUE, selectize = TRUE),
      selectInput("sel.inst", "Instances", list(), multiple = TRUE, selectize = FALSE)
    ),
    dashboardBody(
      tabsetPanel(
        id = "main_panel",
        tabPanel(
          "NAP", actionButton("refresh_nap_log", "Reload NAP log"),
          br(),
          verbatimTextOutput("logger", placeholder = TRUE),
          uiOutput("errLogsButton"), br(), uiOutput("errLogs")
        ),
        tabPanel(
          "Headers",
          tabsetPanel(
            tabPanel("EDF", br(), tableOutput("header.summary"), br(), dataTableOutput("header.channels")),
            tabPanel("Harmonized EDF", fluidRow(
              column(width = 6, h4("Harmonized channels"), tableOutput("channel.mapping1")),
              column(width = 2),
              column(width = 3, h4("Unmapped channels"), tableOutput("channel.mapping2"))
            )),
            tabPanel("Base EDF", fluidRow(
              column(width = 6, h4("Canonical channels"), tableOutput("channel.base.mapping1")),
              column(width = 2),
              column(width = 3, h4("Unselected channels"), tableOutput("channel.base.mapping2"))
            )),
            tabPanel("Annotations", fluidRow(
              column(width = 6, h4("Harmonized annotations"), tableOutput("annot.mapping1")),
              column(width = 1),
              column(width = 5, h4("Aliases"), tableOutput("annot.mapping2"))
            ))
          )
        ),
        tabPanel("Phenotypes", br(), dataTableOutput("pheno.table")),
        tabPanel(
          "Staging",
          tabsetPanel(
            tabPanel(
              "Manual", br(),
              plotOutput("stage.view", width = "100%", height = "100px"),
              hr(),
              tableOutput("stage.summary")
            ),
            tabPanel(
              "SOAP",
              plotOutput("soap.view.orig", width = "100%", height = "100px"),
              plotOutput("soap.view.hypno", width = "100%", height = "100px"),
              plotOutput("soap.view.prob", width = "100%", height = "100px"),
              plotOutput("soap.view.stgdur", width = "100%", height = "250px"),
              hr(),
              verbatimTextOutput("soap.summary")
            ),
            tabPanel(
              "POPS",
              plotOutput("pops.view.orig", width = "100%", height = "100px"),
              plotOutput("pops.view.hypno", width = "100%", height = "100px"),
              plotOutput("pops.view.prob", width = "100%", height = "100px"),
              plotOutput("pops.view.stgdur", width = "100%", height = "250px"),
              hr(),
              verbatimTextOutput("pops.summary")
            )
          )
        ),
        tabPanel(
          "Annotations",
          plotOutput("annot.view", width = "100%", height = "200px"),
          br(),
          tabsetPanel(tabPanel("Summary", tableOutput("annot.summary")), tabPanel("Instances", dataTableOutput("annot.table")))
        ),
        tabPanel(
          "Signals",
          fluidRow(
            column(width = 1, offset = 0, actionButton("button_epoch_prv", " < Prev", width = "100%")),
            column(width = 1, actionButton("button_epoch_nxt", "Next > ", width = "100%")),
            column(width = 1, offset = 0, actionButton("entire.record", "All", width = "100%")),
            # 		column(width = 2, offset = 0, actionButton("rescale.ylim", "Toggle Y scaling", width = '100%' )),
            column(width = 2, offset = 0, actionButton("bandpass", "Toggle 0.3-35Hz", width = "100%")),
            column(width = 1, offset = 0),
            column(width = 6, verbatimTextOutput("info2"))
          ),
          plotOutput("signal.master",
                     width = "100%", height = "30px", click = "master_click", dblclick = "master_dblclick",
                     brush = brushOpts(id = "master_brush", direction = "x", resetOnNew = F)
          ),
          plotOutput("signal.master2", width = "100%", height = "10px"),
          br(), plotOutput("signal.spsd", width = "100%", height = "100px"),
          plotOutput("signal.view",
                     width = "100%", height = "50vh", dblclick = "zoom_dblclick",
                     brush = brushOpts(id = "zoom_brush", direction = "x", resetOnNew = F)
          )
        ),

        # to resize the plot dynamically, uiOutput() rather than plotOutput()
        tabPanel(
          "Spectral",
          sliderInput("sel.freq", "Frequency (Hz)", width = "100%", min = 0, max = 100, step = 0.25, value = c(0.25, 35)),
          uiOutput("ui_psdplot")
        ),
        tabPanel("MTM", uiOutput("ui_mtmplot")),
        tabPanel("Issues", br(), dataTableOutput("issue.table")),
        tabPanel(
          "Tables",
          selectInput("sel.table.group", label = "Group", choices = list()),
          selectInput("sel.table.table", label = "Table", choices = list()),
          hr(),
          dataTableOutput("table.table")
        ),
        tabPanel(
          "Figures",
          selectInput("sel.figure.group", label = "Group", choices = list()),
          selectInput("sel.figure.figure", label = "Figure", choices = list()),
          hr(),
          imageOutput("figure.view")
        ),
        tabPanel(
          "Metrics",
          fluidRow(
            column(
              width = 4,
              verticalLayout(selectInput("sel.derived.group", label = "Group", choices = list()),
                             selectInput("sel.derived.table", label = "Sample metrics", choices = list()),
                             selectInput("sel.derived.variables", label = "Variables", choices = list()),
                             fluid = T
              )
            ),
            column(width = 8, plotOutput("var_plot", height = "200px"))
          ),
          br(),
          verbatimTextOutput("selected.info"),
          dataTableOutput("derived.view")
        )
      ) # tabsetpanel
    ) # dashboardBody
  ) # dashboardPage
    ) # fluidPage
  }


  #
  # UI if not using NAP...
  #

  if (!opt_nap) {
    ui <- fluidPage(
      dashboardPage(

        #
        # Application title
        #

        dashboardHeader(title = "Luna | Moonlight"),
        dashboardSidebar(
          uiOutput("samplesLocal"),
          selectInput("edfs", label = "Samples", choices = list()),
          fluidRow(column(width = 5, actionButton("button_prv", "previous")), column(width = 5, offset = 2, actionButton("button_nxt", "next"))),
          selectInput("sel.ch", "Channels", list(), multiple = TRUE, selectize = TRUE),
          selectInput("sel.ann", "Annotations", list(), multiple = TRUE, selectize = TRUE),
          #      selectInput( "sel.epoch", "Epochs" , list(), multiple = FALSE ,  selectize = TRUE ),
          br(), hr(),
          selectInput("disp.ann", "Annotations (list instances)", list(), multiple = TRUE, selectize = TRUE),
          selectInput("sel.inst", "Instances", list(), multiple = TRUE, selectize = FALSE)
        ),
        dashboardBody(
          tabsetPanel(
            id = "main_panel",
            tabPanel("Headers", br(), tableOutput("header.summary"), br(), dataTableOutput("header.channels")),

            #      tabPanel( "Phenotypes", br(), dataTableOutput("pheno.table")),

            tabPanel("Staging", br(), textOutput("stage.num.epochs"), hr(), plotOutput("stage.view", width = "100%", height = "100px"), hr(), tableOutput("stage.summary")),
            tabPanel(
              "Annotations",
              plotOutput("annot.view", width = "100%", height = "200px"),
              br(),
              tabsetPanel(tabPanel("Summary", tableOutput("annot.summary")), tabPanel("Instances", dataTableOutput("annot.table")))
            ),
            tabPanel(
              "Signals",
              actionButton("entire.record", "Entire record"),
              # 	      actionButton("rescale.ylim", "Toggle Y scale"),
              actionButton("bandpass", "0.3-35 Hz"),
              verbatimTextOutput("info2"),
              plotOutput("signal.master",
                         width = "100%", height = "30px", click = "master_click", dblclick = "master_dblclick",
                         brush = brushOpts(id = "master_brush", direction = "x", resetOnNew = F)
              ),
              plotOutput("signal.master2", width = "100%", height = "10px"),
              br(),
              plotOutput("signal.view",
                         width = "100%", height = "50vh", dblclick = "zoom_dblclick",
                         brush = brushOpts(id = "zoom_brush", direction = "x", resetOnNew = F)
              ),
              br(),
              fluidRow(column(width = 1, offset = 5, actionButton("button_epoch_prv", "previous")), column(width = 1, actionButton("button_epoch_nxt", "next")))
            ),

            # to resize the plot dynamically, uiOutput() rather than plotOutput()
            tabPanel(
              "Spectral",
              sliderInput("sel.freq", "Frequency (Hz)", width = "100%", min = 0, max = 100, step = 0.25, value = c(0.25, 35)),
              uiOutput("ui_psdplot")
            )
          ) # tabsetpanel
        ) # dashboardBody
      ) # dashboardPage
    )
  }


  # --------------------------------------------------------------------------------
  #
  # ERIS deployment specific options: fixed sample list values
  #
  # --------------------------------------------------------------------------------

  if (opt_eris && !opt_local_storage) {
    metadata_lst <- eris.metadata_lst
    home_lst <- eris.home_lst
    base_output_dir <- eris.base_output_dir
  } else {
    if (local_metadata_lst != "") metadata_lst <- local_metadata_lst
    if (local_home_lst != "") home_lst <- local_home_lst
    if (local_base_output_dir != "") base_output_dir <- local_base_output_dir
  }

  # --------------------------------------------------------------------------------
  #
  # Global variables to track presence/absence of certain panels
  #
  # --------------------------------------------------------------------------------

  if (opt_nap) {
    ml.globals$sm_panel_present <- TRUE
    ml.globals$sm_allowChangeSelection <- TRUE
  }



  # --------------------------------------------------------------------------------
  #
  # Main server logic
  #
  # --------------------------------------------------------------------------------

  server <- function(input, output, session) {

    #
    # handle inputs
    #

    # --------------------------------------------------------------------------------
    #
    # Inputs: primary
    #
    # --------------------------------------------------------------------------------

    values <- reactiveValues()

    values$access_code_verified <- FALSE


    session$onSessionEnded(function() {
      opt_aws <<- Sys.getenv("USE_AWS_S3") == "TRUE"
      if (SESSION_PATH != "") {
        fixed.sl <<- paste(SESSION_PATH, SESSION_SLST, sep = "/", collapse = NULL)
      } else {
        fixed.sl <<- ""
      }

      ldrop()
      ml.globals$annots_panel_present <- TRUE
      ml.globals$staging_panel_present <- TRUE
      ml.globals$pheno_panel_present <- TRUE

      if (opt_nap) {
        ml.globals$sm_panel_present <- TRUE
        if (opt_aws) {
          req(aws.user, aws.runid)
          to_delete_dir <- ""
          if (aws.cid == "") {
            to_delete_dir <- paste(getwd(), aws.user, aws.user, aws.runid, sep = "/", collapse = NULL)
          } else {
            to_delete_dir <- paste(getwd(), aws.user, aws.cid, aws.runid, sep = "/", collapse = NULL)
          }
          unlink(to_delete_dir, recursive = TRUE)
        }
      }
    })


    verify_token <- reactive({
      is_valid <- FALSE
      is_valid <- tryCatch(
        {
          aes <- AES(enc_key, mode = "CBC", enc_iv)
          decrypted <- strsplit(aes$decrypt(hex2raw(values$query[["token"]])), "\003")[[1]][1]
          if (!is.na(suppressWarnings(as.numeric(decrypted)))) {
            token_time <- as.numeric(decrypted)
            curr_epoch <- time_length(interval("1970-01-01 00:00:00 EDT", Sys.time()), "second") * 1000
            expiry_time <- as.integer(token_exp_time) * 60 * 1000
            if ((curr_epoch - token_time) > 0 && (curr_epoch - token_time) < expiry_time) {
              return(TRUE)
            }
          }
        },
        error = function(e) {
          return(FALSE)
        }
      )
      return(is_valid)
    })



    popupModal <- function(failed = FALSE) {
      modalDialog(
        textInput("access_code", "Enter Access Code"),
        if (failed) {
          div(tags$b("Invalid Access Code", style = "color: red;"))
        },
        footer = tagList(
          modalButton("Cancel"),
          actionButton("enter", "Enter")
        )
      )
    }

    observeEvent(input$enter, {
      if (!is.null(input$access_code)) {
        if (input$access_code == access_code) {
          removeModal()
          values$access_code_verified <- TRUE
          updateSelectInput(session, "cohort", choices = dl[[1]])
        } else {
          showModal(popupModal(failed = TRUE))
        }
      } else {
        showModal(popupModal(failed = TRUE))
      }
    })

    if (opt_eris) {
      output$cohort <- renderUI({
        selectInput("cohort", label = "Cohort", choices = list())
      })
      output$samplesLocal <- renderUI({
        selectInput("samplesLocal", label = "Sample List", choices = list())
      })
      dl <- read.delim2(file = metadata_lst, sep = "\t", header = FALSE, quote = "")
      if (use_access_code) {
        showModal(popupModal())
      } else {
        updateSelectInput(session, "cohort", choices = dl[[1]])
      }
    }

    observeEvent(input$cohort, {
      if (use_access_code) {
        req(values$access_code_verified)
      }
      if (use_url_auth) {
        values$query <- parseQueryString(session$clientData$url_search)
        req(verify_token())
      }

      list_lst <- dl[dl$V1 == input$cohort, ]
      char_lst <- as.vector(t(list_lst))
      sampleLists <- vector()
      for (lst in char_lst) {
        if (lst != "" && lst != input$cohort) {
          sampleLists <- c(sampleLists, lst)
        }
      }
      updateSelectInput(session, "samplesLocal", choices = sampleLists)
    })


    observeEvent(input$samplesLocal, {
      cat("in OBS E input$samplesLocal \n")
      req(input$samplesLocal)
      sl_path <- paste0(home_lst, input$samplesLocal, sep = "")
      cat("sl_path = ", sl_path, "\n")
      sl <- lsl(sl_path)
      updateSelectInput(session, "edfs", choices = names(sl))
      values$sl <- sl
      attached.edf()
    })


    # --------------------------------------------------------------------------------
    #
    # Load phenotypes
    #
    # --------------------------------------------------------------------------------

    load_phenotypes <- function() {
      values$phenoData <- NULL
      pheno.files <- list.files(nap.dir, full.names = T, pattern = "^_pheno-.*.RData")
      # Remove phenotypes panel if no phenotype data files are available
      if (identical(pheno.files, character(0))) {
        if (ml.globals$pheno_panel_present) {
          removeTab("main_panel", "Phenotypes", session = getDefaultReactiveDomain())
          ml.globals$pheno_panel_present <- FALSE
        }
      } else {
        if (!ml.globals$pheno_panel_present) {
          insertTab("main_panel",
                    tabPanel("Phenotypes", br(), dataTableOutput("pheno.table")), "Headers",
                    position = "after", select = FALSE, session = getDefaultReactiveDomain()
          )
          ml.globals$pheno_panel_present <- TRUE
        }
      }
      pheno_tmpenv <- new.env()
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

      values$query <- parseQueryString(session$clientData$url_search)

      if (fixed.sl != "") {
        sl <- lsl(fixed.sl)
      } else if (length(values$query) == 0) {
        output$samplesLocal <- renderUI({
          fileInput("samples", "Sample List", accept = c("lst"))
        })
        req(input$samples)
        sl <- lsl(input$samples$datapath)
        opt_aws <<- FALSE
      }

      # AWS run-mode
      else {
        req(values$query[["user"]], values$query[["token"]])
        req(verify_token())
        opt_aws <<- TRUE
        isolate({
          aws.user <<- values$query[["user"]]
        })
        aws.runid <<- ""
        if (!is.null(values$query[["runid"]])) {
          aws.runid <<- values$query[["runid"]]
        }


        if (!is.null(values$query[["cid"]])) {
          aws.cid <<- values$query[["cid"]]
          pre_val <- paste(aws.cid, aws.runid, sep = "/", collapse = NULL)
        } else {
          aws.cid <<- ""
          pre_val <- paste(aws.user, aws.runid, sep = "/", collapse = NULL)
        }

        s3_bucket <- get_bucket(s3BucketName, prefix = pre_val)

        is_sl_file_present <- FALSE
        keyV <- paste(pre_val, "s.lst", sep = "/", collapse = NULL)
        final_keyV <- gsub("//", "/", keyV)
        index <- 1
        sl_key <- ""
        for (i in s3_bucket) {
          if (i["Key"] == final_keyV) {
            is_sl_file_present <- TRUE
            if (aws.cid == "") {
              sl_key <- paste(getwd(), aws.user, final_keyV, sep = "/")
            } else {
              sl_key <- paste(getwd(), aws.user, i["Key"], sep = "/", collapse = NULL)
            }
            break
          }
          index <- index + 1
        }

        if (!is_sl_file_present) {
          showNotification("Sample list is missing, you may close the app",
                           duration = NULL, type = "error", session = getDefaultReactiveDomain()
          )
        }

        req(is_sl_file_present)

        aws_sl_file <- save_object(s3_bucket[[index]], file = sl_key)
        awl_sl_file_size <- file.info(aws_sl_file)$size

        if (awl_sl_file_size == 0) {
          showNotification("No EDFs are available for the project, you may close the app",
                           duration = NULL, type = "error", session = getDefaultReactiveDomain()
          )
        }

        req(awl_sl_file_size != 0)
        sl_df <- read.delim(sl_key, header = FALSE)
        new_sl_df <- cbind(sl_df[1], lapply(sl_df[, 2:ncol(sl_df)], function(x) paste(aws.user, pre_val, x, sep = "/")))
        write.table(new_sl_df, sl_key, sep = "\t", col.names = FALSE, row.names = FALSE, quote = FALSE)
        sl <- lsl(aws_sl_file)
      }

      # update sample-list selector
      updateSelectInput(session, "edfs", choices = names(sl), selected = FALSE)
      values$sl <- sl

      return(1)
    })




    # --------------------------------------------------------------------------------
    #
    # Attach an EDF : core load function
    #
    # --------------------------------------------------------------------------------

    attached.edf <- reactive({
      if (!opt_eris) {
        req(attached.sl())
      } else {
        req(!is.null(values$sl[input$edfs][[1]]))
        sl_folder <- tolower(tools::file_path_sans_ext(input$samplesLocal))
        nap.dir <<- paste(base_output_dir, tolower(input$cohort), sl_folder, "nap", sep = "/", collapse = NULL)
      }


      load_phenotypes()

      req(input$edfs)

      if (fixed.sl == "" && opt_aws && !(is.character(values$ID) && values$ID == input$edfs)) {
        proj_path <- ""
        if (aws.cid == "") {
          proj_path <- paste(aws.user, aws.user, aws.runid, sep = "/", collapse = NULL)
        } else {
          proj_path <- paste(aws.user, aws.cid, aws.runid, sep = "/", collapse = NULL)
        }
        nap.dir <<- paste(getwd(), proj_path, "nap", sep = "/", collapse = NULL)
        get_nap <- TRUE
        nap_files <- paste(proj_path, "nap", input$edfs, sep = "/", collapse = NULL)
        total_approx_len <- length(values$sl[input$edfs][[1]]) * length(s3_bucket)
        total_index <- 0
        for (file_name in values$sl[input$edfs][[1]]) {
          withProgress(message = "Pulling NAP files", {
            file_index <- 1
            for (f in s3_bucket) {
              full_file_path <- paste(aws.user, f[["Key"]], sep = "/", collapse = NULL)
              if (full_file_path == file_name) {
                save_object(s3_bucket[[file_index]], file = full_file_path, show_progress = TRUE)
              }
              if (grepl(nap_files, paste(aws.user, f[["Key"]], sep = "/")) && get_nap) {
                save_object(s3_bucket[[file_index]], file = paste(aws.user, f[["Key"]], sep = "/"), show_progress = TRUE)
              }
              incProgress(1 / total_approx_len)
              file_index <- file_index + 1
              total_index <- total_index + 1
            }
          })
          get_nap <- FALSE
          total_index <- total_index + 1
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

      if (opt_nap && input$harmedf) {
        harm.sl <- list()
        harm.sl.edfz <- list()

        harm.sl[[input$edfs]]$EDF <- list.files(paste(nap.dir, values$ID, "data/", sep = "/"),
                                                full.names = T, pattern = "*harm.edf$"
        )

        harm.sl.edfz[[input$edfs]]$EDF <- list.files(paste(nap.dir, values$ID, "data/", sep = "/"),
                                                     full.names = T, pattern = "*harm.edf.gz$"
        )

        # this gets populated below w/ harm.lst anyway
        harm.sl[[input$edfs]]$ANNOT <- character(0)

        # attach only if there was an EDF (or EDFZ) to be attached
        if (length(harm.sl[[input$edfs]]$EDF) != 0) {
          lattach(harm.sl, input$edfs)
        } else if (length(harm.sl.edfz[[input$edfs]]$EDF) != 0) {
          lattach(harm.sl.edfz, input$edfs)
        }
      } else {
        if (file.exists(values$sl[[input$edfs]]$EDF)) {
          lattach(values$sl, input$edfs)
        }
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

      if (opt_nap) {
        nap.annots <- list.files(paste(nap.dir, values$ID, "annots/", sep = "/"),
                                 full.names = T, pattern = "*.annot"
        )
        lapply(nap.annots, ladd.annot.file)
      }



      #
      # annotations
      #

      values$annots <- lannots()


      #
      # Remove Annotations panel if annotations are not available
      #

      if (identical(values$annots, character(0))) {
        if (ml.globals$annots_panel_present) {
          removeTab("main_panel", "Annotations", session = getDefaultReactiveDomain())
          ml.globals$annots_panel_present <- FALSE
        }
      } else {
        if (!ml.globals$annots_panel_present) {
          insertTab("main_panel", tabPanel(
            "Annotations",
            plotOutput("annot.view", width = "100%", height = "200px"),
            br(),
            tabsetPanel(
              tabPanel("Summary", tableOutput("annot.summary")),
              tabPanel("Instances", dataTableOutput("annot.table"))
            )
          ),
          "Signals",
          position = "before", select = FALSE, session = getDefaultReactiveDomain()
          )
          ml.globals$annots_panel_present <- TRUE
        }
      }

      values$annot.inst <- leval("ANNOTS")$ANNOTS


      #
      # epoch (fixed at 30 seconds)
      #

      values$ne <- lepoch()


      #
      # Staging information present (including SOAP/POPS)?
      #

      attach.staging()


      #
      # NAP derived metrics?
      #

      if (opt_nap) attach.nap.data()

      #
      # update control widgets
      #

      updateSelectInput(
        session,
        "sel.ch",
        choices = values$channels,
        label = paste(length(values$channels), "channels"),
        selected = 0
      )

      updateSelectInput(
        session,
        "sel.ann",
        choices = values$annots,
        label = paste(length(values$annots), "annotations"),
        selected = 0
      )

      updateSelectInput(
        session,
        "disp.ann",
        choices = values$annots,
        label = paste(length(values$annots), "annotations (list instances)"),
        selected = 0
      )


      #
      # queries EDF headers
      #

      isolate({
        values$eval <- leval("HEADERS & STAGE & HYPNO")
      })


      #
      # get SS (& aligned)
      #

      values$ss <- values$eval$STAGE$E

      values$ss.aligned <- leval("EPOCH align=W,N1,N2,N3,R & STAGE")$STAGE$E


      #
      # plot views (seconds)
      #

      values$epochs <- c(1, 1)
      values$zoom <- NULL
      values$raw.signals <- T
      values$yscale <- T # not used
      values$bandpass <- F

      #
      # SOAP tracker
      #

      values$soap.epoch <- 1
      values$soap <- NULL

      #
      # get channel units
      #

      isolate({
        values$units <- values$eval$HEADERS$CH$PDIM
        names(values$units) <- as.character(values$eval$HEADERS$CH$CH)

        values$sr <- as.integer(values$eval$HEADERS$CH$SR)
        names(values$sr) <- as.character(values$eval$HEADERS$CH$CH)
      })


      return(1)
    })



    #
    # annot-instance list selector
    #

    observe({
      req(values$annot.inst)
      flt <- values$annot.inst$ANNOT_INST_T1_T2$ANNOT %in% input$disp.ann
      if (sum(flt) > 0) {
        secs1 <- values$annot.inst$ANNOT_INST_T1_T2$START[flt]
        secs2 <- values$annot.inst$ANNOT_INST_T1_T2$STOP[flt]
        annot <- values$annot.inst$ANNOT_INST_T1_T2$ANNOT[flt]
        #      inst <- values$annot.inst$ANNOT_INST_T1_T2$INST[ flt ]
        vals <- paste(annot, secs1, sep = ": ")
        inst <- as.list(paste(secs1, secs2))
        names(inst) <- vals
        if (length(secs1) > 0) inst <- inst[order(secs2)]
        updateSelectInput(
          session,
          "sel.inst",
          choices = inst,
          label = paste(length(secs1), " instances,", length(input$disp.ann), "annotations"),
          selected = 0
        )
      }
    })


    observeEvent(input$button_prv, {
      req(attached.edf())
      curr_index <- match(values$ID, names(values$sl))
      if (curr_index > 1) {
        updateSelectizeInput(session, "edfs", choices = names(values$sl), selected = names(values$sl[curr_index - 1]))
      }
    })


    observeEvent(input$button_nxt, {
      req(attached.edf())
      curr_index <- match(values$ID, names(values$sl))
      if (curr_index < length(values$sl)) {
        updateSelectizeInput(session, "edfs", choices = names(values$sl), selected = names(values$sl[curr_index + 1]))
      }
    })


    # --------------------------------------------------------------------------------
    #
    # Inputs: staging
    #
    # --------------------------------------------------------------------------------


    attach.staging <- function() {

      #
      # has manual staging?
      #

      values$has_manual_staging <- !is.null(lstages())

      #
      # has automated staging? (luna_suds_POPS-)
      #

      values$has_pops_staging <- !is.null(values$data$luna_suds_POPS)
      values$has_soap_staging <- !is.null(values$data$luna_suds_SOAP)

      cat("has manual staging =", values$has_manual_staging, "\n")
      cat("has pops staging =", values$has_pops_staging, "\n")
      cat("has soap staging =", values$has_soap_staging, "\n")

      #
      # if neither manual nor POPS staging info available, remove Staging Panel completely
      #

      if (!(values$has_manual_staging | values$has_soap_staging | values$has_pops_staging)) {
        if (ml.globals$staging_panel_present) {
          removeTab("main_panel", "Staging", session = getDefaultReactiveDomain())
          ml.globals$staging_panel_present <- FALSE
        }
      } else {
        if (!ml.globals$staging_panel_present) {
          # if not running in NAP-mode, we will only have manual staging

          if (!opt_nap) {
            insertTab("main_panel",
                      tabPanel(
                        "Staging", br(),
                        textOutput("stage.num.epochs"), hr(),
                        plotOutput("stage.view", width = "100%", height = "100px"), hr(),
                        tableOutput("stage.summary")
                      ),
                      "Headers",
                      position = "after", select = FALSE, session = getDefaultReactiveDomain()
            )
          } else {

            # here, in NAP-mode, we may have
            #  Manual staging, SOAP and POPS
            #  No manual staging, and only POPS
            #  (unlikely, but possible?): only Staging, i.e. manual staging, but no EEG available

            if (values$has_manual_staging & values$has_soap_staging & values$has_pops_staging) {
              insertTab("main_panel",
                        tabPanel(
                          "Staging",
                          tabsetPanel(
                            tabPanel(
                              "Manual", br(),
                              textOutput("stage.num.epochs"), hr(),
                              plotOutput("stage.view", width = "100%", height = "100px"), hr(),
                              tableOutput("stage.summary")
                            ),
                            tabPanel(
                              "SOAP",
                              plotOutput("soap.view.orig", width = "100%", height = "100px"),
                              plotOutput("soap.view.hypno", width = "100%", height = "100px"),
                              plotOutput("soap.view.prob", width = "100%", height = "100px"),
                              plotOutput("soap.view.stgdur", width = "100%", height = "250px"),
                              hr(),
                              verbatimTextOutput("soap.summary")
                            ),
                            tabPanel(
                              "POPS",
                              plotOutput("pops.view.orig", width = "100%", height = "100px"),
                              plotOutput("pops.view.hypno", width = "100%", height = "100px"),
                              plotOutput("pops.view.prob", width = "100%", height = "100px"),
                              plotOutput("pops.view.stgdur", width = "100%", height = "250px"),
                              hr(),
                              verbatimTextOutput("pops.summary")
                            )
                          )
                        ),
                        "Headers",
                        position = "after", select = FALSE, session = getDefaultReactiveDomain()
              )
            }

            if (values$has_pops_staging & !values$has_manual_staging) {
              insertTab("main_panel",
                        tabPanel(
                          "POPS",
                          plotOutput("pops.view.orig", width = "100%", height = "100px"),
                          plotOutput("pops.view.hypno", width = "100%", height = "100px"),
                          plotOutput("pops.view.prob", width = "100%", height = "100px"),
                          plotOutput("pops.view.stgdur", width = "100%", height = "250px"),
                          hr(),
                          verbatimTextOutput("pops.summary")
                        ),
                        "Headers",
                        position = "after", select = FALSE, session = getDefaultReactiveDomain()
              )
            }
          }

          # denote that we now have staging available
          ml.globals$staging_panel_present <- TRUE
        }
      }
    }



    # --------------------------------------------------------------------------------
    #
    # Inputs: NAP
    #
    # --------------------------------------------------------------------------------

    if (opt_nap) {
      attach.nap.data <- function() {


        #
        # NAP issues
        #

        #
        # NAP issues (nap/{id}/nap.issues)
        #

        df <- data.frame(Issue = character(), Channel = character(), Notes = character())
        if (opt_nap) {
          issues.filename <- paste(nap.dir, values$ID, "nap.issues", sep = "/")
          if (file.exists(issues.filename)) {
            df <- read.table(issues.filename, header = F, stringsAsFactors = F)
            names(df) <- c("Issue", "Channel", "Notes")
          }
        }
        values$issuesData <- df




        #
        # any NAP tables?
        #

        nap.files <-
          list.files(paste(nap.dir, values$ID, sep = "/"),
                     full.names = T,
                     pattern = "*-tab.RData"
          )
        cat("dir", paste(nap.dir, values$ID, sep = "/"), "\n")
        print(nap.files)
        tmpenv <- new.env()
        invisible(lapply(nap.files, load, envir = tmpenv))
        isolate({
          values$data <- as.list(tmpenv)
        })
        rm(tmpenv)
        groups <- unlist(lapply(values$data, "[[", "desc"))
        d.groups <- as.list(names(groups))
        names(d.groups) <- unlist(groups)

        updateSelectInput(
          session,
          "sel.table.group",
          choices = d.groups,
          label = paste(length(d.groups), " groups")
        )


        #
        # any NAP figures?
        #

        nap.files <-
          list.files(paste(nap.dir, values$ID, sep = "/"),
                     full.names = T,
                     pattern = "*-fig.RData"
          )
        tmpenv <- new.env()
        invisible(lapply(nap.files, load, envir = tmpenv))
        values$figures <- as.list(tmpenv)
        rm(tmpenv)
        groups <- unlist(lapply(values$figures, "[[", "desc"))
        d.groups <- as.list(names(groups))
        names(d.groups) <- unlist(groups)

        updateSelectInput(
          session,
          "sel.figure.group",
          choices = d.groups,
          label = paste(length(d.groups), " groups")
        )

        #
        # Any dervied metrics? (only attach once)
        #

        derived.files <- list.files(nap.dir, full.names = T, pattern = "^_derived-.*.RData")

        if (opt_nap && length(values$derived_data) == 0) {
          if (identical(derived.files, character(0))) {
            if (ml.globals$sm_panel_present) {
              removeTab("main_panel", "Metrics", session = getDefaultReactiveDomain())
              ml.globals$sm_panel_present <- F
            }
          } else {
            if (!ml.globals$sm_panel_present) {
              appendTab("main_panel",
                        tabPanel(
                          "Metrics",
                          fluidRow(
                            column(
                              width = 4,
                              verticalLayout(selectInput("sel.derived.group", label = "Group", choices = list()),
                                             selectInput("sel.derived.table", label = "Sample metrics", choices = list()),
                                             selectInput("sel.derived.variables", label = "Variables", choices = list()),
                                             fluid = T
                              )
                            ),
                            column(width = 8, plotOutput("var_plot", height = "200px"))
                          ),
                          br(),
                          verbatimTextOutput("selected.info"),
                          dataTableOutput("derived.view")
                        ),
                        select = FALSE, session = getDefaultReactiveDomain()
              )
              ml.globals$sm_panel_present <- T
            }
          }


          #
          # attach derived data
          #

          derived_tmpenv <- new.env()
          invisible(lapply(derived.files, load, envir = derived_tmpenv))
          values$derived_data <- as.list(derived_tmpenv)
          rm(derived_tmpenv)
          derived_groups <- unlist(lapply(values$derived_data, "[[", "desc"))
          d.derived_groups <- as.list(names(derived_groups))
          names(d.derived_groups) <- unlist(derived_groups)

          updateSelectInput(
            session,
            "sel.derived.group",
            choices = d.derived_groups,
            label = paste(length(d.derived_groups), " group(s)")
          )
          # ml.globals$sm_allowChangeSelection <- T
        }
        ml.globals$sm_allowChangeSelection <- T


        #
        # MTM images
        #

        values$mtm.files <- list.files(paste(nap.dir, values$ID, sep = "/"),
                                       full.names = T, pattern = glob2rx("mtm-*.png")
        )


        #
        # attach pre-computed summary PSD, if exists
        #

        values$spsd <- NULL
        spsd <- NULL
        spsd.filename <- paste(nap.dir, values$ID, "nap.spsd.RData", sep = "/")
        if (file.exists(spsd.filename)) {
          # loads 'spsd'
          load(spsd.filename)
          values$spsd <- spsd
          rm(spsd)
        }

        #
        # attach pre-computed sigstats, if exists
        # (decoding with these are original or harmonized versions
        #

        values$sigstats <- NULL
        harm.sigstats <- NULL
        sigstats <- NULL
        sigstats.filename <- ifelse(input$harmedf, "nap.harm.sigstats.RData", "nap.sigstats.RData")
        sigstats.filename <- paste(nap.dir, values$ID, sigstats.filename, sep = "/")
        if (file.exists(sigstats.filename)) {
          # loads 'sigstats'
          load(sigstats.filename)
          if (input$harmedf) {
            values$sigstats <- harm.sigstats
            rm(harm.sigstats)
          } else {
            values$sigstats <- sigstats
            rm(sigstats)
          }
        }


        # NAP channel and annotation harmonizations
        #  tables: channel.mapping1, channel.mapping2  -> sigs.harm.map1 , sigs.harm.map2
        #        : annot.mapping1, annot.mapping2      -> annot.alias , annot.map

        chmap1.filename <- paste(nap.dir, values$ID, "nap.sig.map1.RData", sep = "/")
        chmap2.filename <- paste(nap.dir, values$ID, "nap.sig.map2.RData", sep = "/")

        bchmap1.filename <- paste(nap.dir, values$ID, "nap.sig.base.map1.RData", sep = "/")
        bchmap2.filename <- paste(nap.dir, values$ID, "nap.sig.base.map2.RData", sep = "/")

        amap1.filename <- paste(nap.dir, values$ID, "nap.annot.map.RData", sep = "/")
        amap2.filename <- paste(nap.dir, values$ID, "nap.annot.alias.RData", sep = "/")

        values$chmap1 <- values$chmap2 <- NULL
        values$bchmap1 <- values$bchmap2 <- NULL
        values$amap1 <- values$amap2 <- NULL
        sigs.harm.map1 <- NULL
        sigs.harm.map2 <- NULL
        sigs.base.map1 <- NULL
        sigs.base.map2 <- NULL
        annot.map <- NULL
        annot.alias <- NULL
        # harmonized EDF channels

        if (file.exists(chmap1.filename)) {
          load(chmap1.filename)
          values$chmap1 <- sigs.harm.map1
          rm(sigs.harm.map1)
        }

        if (file.exists(chmap2.filename)) {
          load(chmap2.filename)
          values$chmap2 <- sigs.harm.map2
          rm(sigs.harm.map2)
        }

        # base EDF channels

        if (file.exists(bchmap1.filename)) {
          load(bchmap1.filename)
          values$bchmap1 <- sigs.base.map1
          rm(sigs.base.map1)
        }

        if (file.exists(bchmap2.filename)) {
          load(bchmap2.filename)
          values$bchmap2 <- sigs.base.map2
          rm(sigs.base.map2)
        }

        # annotations

        if (file.exists(amap1.filename)) {
          load(amap1.filename)
          values$amap1 <- annot.map
          rm(annot.map)
        }

        if (file.exists(amap2.filename)) {
          load(amap2.filename)
          values$amap2 <- annot.alias
          rm(annot.alias)
        }
      }
    }



    # --------------------------------------------------------------------------------
    #
    # Output panels: phenotypes (NAP opt)
    #
    # --------------------------------------------------------------------------------

    if (opt_nap) {

      # Phenotype files are expected to be listed in the nap.dir/nap/ folder as
      # _pheno-*RData files where * represents 0 or more characters.
      #
      # Merge of phenotypes from multiple files is based on "ID" column matched with selected EDF(Sample) ID


      output$pheno.table <- renderDataTable(
        {
          req(attached.edf(), length(values$phenoData) != 0)
          v <- 1
          df_final <- NULL
          for (i in 1:length(values$phenoData)) {
            df <- values$phenoData[[i]]
            indiv_values <- df[df$ID == values$ID, ]
            if (is.data.frame(indiv_values) && !nrow(indiv_values) == 0) {
              if (v == 1) {
                df_final <- indiv_values
              } else {
                df_final <- merge(df_final, indiv_values, by = "ID")
              }
              v <- v + 1
            }
          }
          if (is.data.frame(df_final) && !nrow(df_final) == 0) {
            df_final <- as.data.frame(t(df_final[, -1]))
            df_final$Variable <- rownames(df_final)
            df_final <- df_final[c(2, 1)]
            colnames(df_final)[2] <- "Value"
          }
          df_final
        },
        rownames = FALSE,
        options = list(pageLength = 20, rownames = F, columnDefs = list(list(className = "dt-center", targets = "_all")))
      )
    }


    # --------------------------------------------------------------------------------
    #
    # Output panels: NAP tables
    #
    # --------------------------------------------------------------------------------

    if (opt_nap) {
      observeEvent(input$errLogsButton, {
        output$errLogs <- renderUI({
          verbatimTextOutput("errLogsText", placeholder = TRUE)
        })
        output$errLogsText <- renderText({
          filename <- file.path(nap.dir, values$ID, "nap.err")
          req(file.exists(filename))
          filename <- normalizePath(filename, mustWork = F)
          cat(filename, "\n")
          readChar(filename, file.info(filename)$size)
        })
      })

      observeEvent(input$refresh_nap_log, {
        req(attached.edf())
        if (opt_aws) {
          if (aws.cid == "") {
            proj_path <- paste(aws.user, aws.user, aws.runid, sep = "/", collapse = NULL)
          } else {
            proj_path <- paste(aws.user, aws.cid, aws.runid, sep = "/", collapse = NULL)
          }
          nap_files <- paste(proj_path, "nap", values$ID, sep = "/", collapse = NULL)
          withProgress(message = "Pulling latest NAP files", {
            file_index <- 1
            for (f in s3_bucket) {
              if (grepl(nap_files, paste(aws.user, f[["Key"]], sep = "/"))) {
                save_object(s3_bucket[[file_index]], file = paste(aws.user, f[["Key"]], sep = "/"), show_progress = TRUE)
              }
              incProgress(1 / length(s3_bucket))
              file_index <- file_index + 1
            }
          })
        }
      })


      read_nap_log <- reactive({
        filename <- file.path(nap.dir, values$ID, "nap.log")

        if (!file.exists(filename)) {
          return("NAP not initiated: refresh to update")
        }

        filename <- normalizePath(filename, mustWork = F)
        ml.globals$show_detailed_logs_butn(TRUE)
        readChar(filename, file.info(filename)$size)
      })

      output$errLogsButton <- renderUI({
        if (ml.globals$show_detailed_logs_butn()) {
          actionButton("errLogsButton", "View detailed log")
        }
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
          lapply(
            values$data[[input$sel.table.group]][names(values$data[[input$sel.table.group]]) != "desc"],
            "[[", "desc"
          )
        d.tables <- as.list(names(tables))
        names(d.tables) <- unlist(tables)

        updateSelectInput(
          session,
          "sel.table.table",
          choices = d.tables,
          label = paste(length(d.tables), " tables")
        )
      })


      output$table.table <- DT::renderDataTable(DT::datatable(
        {
          req(
            attached.edf(),
            input$sel.table.group,
            input$sel.table.table
          )

          data <-
            values$data[[input$sel.table.group]][[input$sel.table.table]]$data
          data
        },
        rownames = F,
        options = list(
          pageLength = 25,
          lengthMenu = list(c(25, 50, -1), c("20", "50", "All")),
          columnDefs = list(list(
            className = "dt-center", targets = "_all"
          ))
        )
      ))



      #
      # Figures tab
      #

      observe({
        req(input$sel.figure.group)
        fig.labels <-
          unlist(lapply(
            values$figures[[input$sel.figure.group]][names(values$figures[[input$sel.figure.group]]) != "desc"],
            "[[", "desc"
          ))
        fig.files <-
          lapply(
            values$figures[[input$sel.figure.group]][names(values$figures[[input$sel.figure.group]]) != "desc"],
            "[[", "figure"
          )
        names(fig.files) <- fig.labels
        updateSelectInput(
          session,
          "sel.figure.figure",
          choices = fig.files,
          label = paste(length(fig.files), " figures")
        )
      })

      # show figure (PNG)

      output$figure.view <- renderImage(
        {
          req(
            attached.edf(),
            input$sel.figure.group,
            input$sel.figure.figure
          )
          filename <- file.path(nap.dir, values$ID, input$sel.figure.figure)
          req(file.exists(filename))
          list(src = filename)
        },
        deleteFile = FALSE
      )
    }

    # --------------------------------------------------------------------------------
    #
    # Output panels: headers
    #
    # --------------------------------------------------------------------------------


    output$header.summary <- renderTable(
      {
        req(attached.edf())

        k <- values$eval$HEADERS$BL
        k$EPOCH <- k$TOT_DUR_SEC / 30.0

        df <- data.frame(t(k))

        df$VAR <- c(
          "ID",
          "ID (EDF header)",
          "EDF Type",
          "Number of records",
          "Number of selected signals",
          "Total number of signals",
          "Record duration (secs)",
          "Start date",
          "Start time",
          "Duration (hh:mm:ss)",
          "Duration (secs)",
          "Duration (epoch)"
        )

        # return value
        df[, c(2, 1)]
      },
      width = "100%",
      rownames = F,
      colnames = F,
      striped = T
    )



    output$header.channels <- renderDataTable(
      {
        req(attached.edf())

        k <- values$eval$HEADERS$CH

        k$ID <- NULL

        k <- k[, c("CH", "SR", "PDIM", "TRANS", "PMIN", "PMAX", "TYPE")]

        names(k) <- c("Channel", "Sample rate", "Unit", "Transducer", "Minimum", "Maximum", "Type")

        # return value
        k
      },
      rownames = FALSE,
      options = list(pageLength = 20, rownames = F, columnDefs = list(list(className = "dt-center", targets = "_all")))
    )


    #
    # Mapping tables
    #

    output$channel.mapping1 <- renderTable(
      {
        req(attached.edf())
        values$chmap1
        df <- values$chmap1[, c(2, 3, 6, 5, 4)]
        names(df) <- c("Harmonized", "Defined", "Original", "Re-referenced", "Notes")
        df0 <- df[df$Defined == 0, ]
        df1 <- df[df$Defined == 1, ]
        df <- rbind(df1[order(toupper(df1$Harmonized)), ], df0[order(toupper(df0$Harmonized)), ])
        df
      },
      width = "100%",
      rownames = F,
      colnames = T,
      striped = T
    )


    output$channel.mapping2 <- renderTable(
      {
        req(attached.edf())
        df <- values$chmap2
        df <- df[order(df$CH), ]
        df <- df[df$USED == 0, ]
        names(df)[2] <- "Original"
        df$Original
      },
      width = "100%",
      rownames = F,
      colnames = F,
      striped = T
    )


    output$channel.base.mapping1 <- renderTable(
      {
        req(attached.edf())
        df <- values$bchmap1[, c(2, 3, 6, 5, 7, 8, 4)]
        names(df) <- c("Harmonized", "Defined", "Original", "Re-referenced", "SR", "Units", "Notes")
        df0 <- df[df$Defined == 0, ]
        df1 <- df[df$Defined == 1, ]
        df <- rbind(df1[order(toupper(df1$Harmonized)), ], df0[order(toupper(df0$Harmonized)), ])
        df
      },
      width = "100%",
      rownames = F,
      colnames = T,
      striped = T
    )


    output$channel.base.mapping2 <- renderTable(
      {
        req(attached.edf())
        df <- values$bchmap2
        df <- df[order(df$CH), ]
        df <- df[df$USED == 0, ]
        names(df)[2] <- "Original"
        df$Original
      },
      width = "100%",
      rownames = F,
      colnames = F,
      striped = T
    )


    output$annot.mapping1 <- renderTable(
      {
        req(attached.edf(), values$amap1)
        df <- values$amap1
        names(df) <- c("Class", "Instance", "Mapped")
        df[order(df$Mapped, toupper(df$Class), toupper(df$Instance)), ]
      },
      width = "100%",
      rownames = F,
      colnames = T,
      striped = T
    )

    output$annot.mapping2 <- renderTable(
      {
        req(attached.edf(), values$amap2)
        df <- values$amap2
        names(df) <- c("Original", "Alias")
        df[order(toupper(df[, 1])), ]
      },
      width = "100%",
      rownames = F,
      colnames = T,
      striped = T
    )




    # --------------------------------------------------------------------------------
    #
    # Output panels: annotations
    #
    # --------------------------------------------------------------------------------

    output$annot.view <- renderPlot({
      req(attached.edf())
      # get annotationss
      df <- values$annot.inst$ANNOT_INST_T1_T2[, c("ANNOT", "START", "STOP")]
      df <- df[df$ANNOT %in% input$sel.ann, ]
      df$START <- df$START / 3600
      df$STOP <- df$STOP / 3600
      na <- length(unique(df$ANNOT))

      # length of recording
      k <- leval("HEADERS")
      recdur.hrs <- k$HEADERS$BL$TOT_DUR_SEC / 3600
      # main plot (-3600 puts 2 hr of time in the left axis for labels)
      par(mar = c(2.2, 0, 0, 0))
      plot(c(-2, recdur.hrs), c(0, 1), type = "n", axes = F, ylim = c(0, 1), ylab = "", xlab = "")
      axis(1, 0:round(recdur.hrs))
      # plot each annot (0.5 is spacer for top/bottom)
      py <- yinc <- 1 / (length(input$sel.ann) + 0.5)
      yidx <- 1
      for (ann in input$sel.ann) {
        cidx <- 1 + (yidx %% 10)
        flt <- df$ANNOT == ann
        # OLD: points( df$START[ flt ] , rep( 1 - py , sum( flt ) ) , pch="|" , cex=1 , col = ml.globals$pal10[cidx]  )
        for (aa in which(flt)) {
          rect(df$START[aa], 1 - py - 0.5 * yinc, df$STOP[aa], 1 - py + 0.5 * yinc, col = ml.globals$pal10[cidx], border = NA)
        }
        text(-2, 1 - py, ann, col = ml.globals$pal10[cidx], pos = 4)
        py <- py + yinc
        yidx <- yidx + 1
      }
    })


    output$annot.summary <- renderTable(
      {
        req(attached.edf())
        df <- values$annot.inst$ANNOT
        df$ID <- NULL
        df$AVG <- df[, 3] / df[, 2]
        df[, 2] <- as.integer(df[, 2])
        names(df) <- c("Annotation", "Count", "Total duration (secs)", "Average duration (secs)")
        df
      },
      width = "100%",
      striped = T,
      rownames = F,
      colnames = T
    )

    output$annot.table <- renderDataTable({
      req(attached.edf())
      df <-
        values$annot.inst$ANNOT_INST_T1_T2[, c("ANNOT", "INST", "START", "STOP")]
      df <- df[df$ANNOT %in% input$sel.ann, ]
      df$DUR <- round(df$STOP - df$START, 3)
      names(df) <-
        c("Annotation ID", "Instance ID", "Start (secs)", "Stop (secs)", "Dur (secs)")
      df
    })


    # --------------------------------------------------------------------------------
    #
    # Output panels: staging
    #
    # --------------------------------------------------------------------------------

    output$stage.view <- renderPlot({
      req(attached.edf())
      par(mar = c(2.2, 4, 1, 0))
      # get stages
      ss <- values$ss
      # hypnogram image
      plot(ss$E / 120, ss$STAGE_N, type = "l", lwd = 2, col = "gray", axes = F, ylim = c(-3, 2), ylab = "")
      points(ss$E / 120, ss$STAGE_N, col = lstgcols(ss$STAGE), type = "p", cex = 1, pch = 20)
      axis(1)
      axis(2, 2, "?", col.axis = "black", las = 2)
      axis(2, 1, "W", col.axis = lstgcols("wake"), las = 2)
      axis(2, 0, "R", col.axis = lstgcols("REM"), las = 2)
      axis(2, -1, "N1", col.axis = lstgcols("NREM1"), las = 2)
      axis(2, -2, "N2", col.axis = lstgcols("NREM2"), las = 2)
      axis(2, -3, "N3", col.axis = lstgcols("NREM3"), las = 2)
    })

    output$stage.summary <- renderTable(
      {
        req(attached.edf())
        # reset MASK
        # get hypnogram information
        ss <- values$eval$HYPNO$BL
        # hypnogram summary
        ss$ID <- NULL
        t(ss)
      },
      width = "100%",
      striped = T,
      rownames = T,
      colnames = F
    )



    # --------------------------------------------------------------------------------
    #
    # Output panels: POPS
    #
    # --------------------------------------------------------------------------------

    if (opt_nap) {
      fhypnogram <- function(e, sn, sstg, disc3 = NULL, disc5 = NULL) {
        sstg[is.na(sstg)] <- "?"
        # hypnogram image
        yh <- ifelse(is.null(disc3), 2, 4)
        e <- e / 120
        plot(e, sn, type = "l", lwd = 2, col = "gray", axes = F, ylim = c(-3, yh), ylab = "", yaxt = "n", xaxs = "i")
        points(e, sn, col = lstgcols(sstg), type = "p", cex = 1, pch = 20)
        axis(1)
        #  axis(2 , 2 , "?" , col.axis = "black" , las = 2)
        #  axis(2 , 1 , "W" , col.axis = lstgcols("wake") , las = 2)
        #  axis(2 , 0 , "R" , col.axis = lstgcols("REM") , las = 2)
        #  axis(2 ,-1 , "N1" , col.axis = lstgcols("NREM1") , las = 2)
        #  axis(2 ,-2 , "N2" , col.axis = lstgcols("NREM2") , las = 2)
        #  axis(2 ,-3 , "N3" , col.axis = lstgcols("NREM3") , las = 2)
        if (!is.null(disc3)) points(e[disc3 == 1], rep(3, length(e[disc3 == 1])), pch = "|", col = "red")
        if (!is.null(disc5)) points(e[disc5 == 1], rep(3.8, length(e[disc5 == 1])), pch = "|", col = "orange", cex = 0.8)
      }

      fstgn <- function(x) {
        x[x == "N1"] <- -1
        x[x == "N2"] <- -2
        x[x == "N3"] <- -3
        x[x == "R"] <- 0
        x[x == "W"] <- 1
        x[x == "?"] <- 2
        x[is.na(x)] <- 2
        as.numeric(x)
      }

      f100 <- function(x) {
        t <- numeric()
        if (any(is.na(x))) {
          return(rep(6, 100))
        }
        for (s in rev(order(x))) t <- c(t, rep(s, x[s]))
        t[1:100]
      }

      fphyp <- function(m) {
        e <- m[, 1]
        ne <- max(e)
        h <- m[, -1]
        xr <- c(1, ne)
        hh <- matrix(NA, nrow = max(e), ncol = 100)
        yy <- numeric(ne)
        h <- round(as.matrix(h), 2) * 100
        h[h < 0] <- 0
        h[h > 100] <- 100
        hh <- t(apply(h, 1, f100))
        stgpal <- c(lstgcols("N1"), lstgcols("N2"), lstgcols("N3"), lstgcols("R"), lstgcols("W"), "lightgray")
        # build pallete, taking only observed values
        stgpal <- stgpal[as.integer(names(table(hh)))]
        image(hh, col = stgpal, xaxt = "n", yaxt = "n", axes = F)
      }


      fstgdur <- function(d) {
        d <- d[order(d$SS), ]
        d2 <- t(as.matrix(d[dim(d)[1]:1, c("DUR_OBS", "DUR_PRD")]))
        barplot(matrix(as.numeric(d2), ncol = ncol(d2)),
                beside = T, horiz = T, col = lstgcols(rev(rep(d$SS, each = 2))),
                names = rev(d$SS), las = 2, density = c(30, NA),
                xlab = "Minutes", ylab = "Sleep Stage"
        )
      }


      #
      # SOAP
      #

      # original hypnogram (nb. using ALIGNED staging
      # which will match SOAP if non-zero offset used
      # i.e. EPOCH align=N1,N2,N3,W,R)

      output$soap.view.orig <- renderPlot({
        req(attached.edf(), values$data$luna_suds_SOAP)
        par(mar = c(2, 1, 1, 1))

        #  print( values$ss.aligned )
        # ss <- values$ss.aligned
        ss <- values$data$luna_suds_SOAP$"luna_suds_SOAP_E"$data
        # cat( "SOAP\n")
        # print( values$data$luna_suds_SOAP$"luna_suds_SOAP_E" )
        # cat( "POPS\n")
        # print( values$data$luna_suds_POPS$"luna_suds_POPS_E" )

        # hypnogram image
        fhypnogram(ss$E, lstgn(ss$PRIOR), ss$PRIOR)
      })

      # SOAP hypnogram (w/ discordance)

      output$soap.view.hypno <- renderPlot({
        req(attached.edf(), values$data$luna_suds_SOAP)
        par(mar = c(2, 1, 1, 1))
        sstg <- values$data$luna_suds_SOAP$"luna_suds_SOAP_E"$data$PRED
        epochs <- values$data$luna_suds_SOAP$"luna_suds_SOAP_E"$data$E
        disc3 <- values$data$luna_suds_SOAP$"luna_suds_SOAP_E"$data$DISC3
        disc5 <- values$data$luna_suds_SOAP$"luna_suds_SOAP_E"$data$DISC
        fhypnogram(epochs, fstgn(sstg), sstg, disc3, disc5)
      })

      # SOAP posteriors

      output$soap.view.prob <- renderPlot({
        req(attached.edf(), values$data$luna_suds_SOAP)
        par(mar = c(1, 1, 1, 1))
        epp <- values$data$luna_suds_SOAP$"luna_suds_SOAP_E"$data[, c("E", "PP_N1", "PP_N2", "PP_N3", "PP_R", "PP_W")]
        fphyp(epp)
      })

      # stage durations

      output$soap.view.stgdur <- renderPlot({
        req(attached.edf(), values$data$luna_suds_SOAP)
        par(mar = c(3, 3, 0, 1))
        dat <- values$data$luna_suds_SOAP$"luna_suds_SOAP_SS"$data[, c("SS", "DUR_OBS", "DUR_PRD")]
        fstgdur(dat)
      })


      #
      # SUDS
      #


      # original hypnogram (also uses aligned observed epochs, i.e. if missing values prior to first Wake/Sleep epoch)

      output$pops.view.orig <- renderPlot({
        req(attached.edf(), values$data$luna_suds_POPS, values$has_manual_staging)
        par(mar = c(2, 1, 1, 1))
        ss <- values$ss.aligned
        # hypnogram image
        fhypnogram(ss$E, ss$STAGE_N, ss$STAGE)
      })


      # POPS hypnogram (w/ discordance)

      output$pops.view.hypno <- renderPlot({
        req(attached.edf(), values$data$luna_suds_POPS)
        par(mar = c(2, 1, 1, 1))
        sstg <- values$data$luna_suds_POPS$luna_suds_POPS_E$data$PRED
        epochs <- values$data$luna_suds_POPS$luna_suds_POPS_E$data$E
        disc3 <- values$data$luna_suds_POPS$luna_suds_POPS_E$data$FLAG == 2
        disc5 <- values$data$luna_suds_POPS$luna_suds_POPS_E$data$FLAG == 1
        fhypnogram(epochs, fstgn(sstg), sstg, disc3, disc5)
      })

      # POPS posteriors

      output$pops.view.prob <- renderPlot({
        req(attached.edf(), values$data$luna_suds_POPS)
        par(mar = c(1, 1, 1, 1))
        epp <- values$data$luna_suds_POPS$luna_suds_POPS_E$data[, c("E", "PP_N1", "PP_N2", "PP_N3", "PP_R", "PP_W")]
        fphyp(epp)
      })


      output$pops.view.stgdur <- renderPlot({
        req(attached.edf(), values$data$luna_suds_POPS)
        par(mar = c(3, 3, 0, 1))
        dat <- values$data$luna_suds_POPS$luna_suds_POPS_SS$data[, c("SS", "OBS", "PRF")]
        dat$OBS[is.na(dat$OBS)] <- 0
        dat$PRF[is.na(dat$PRF)] <- 0
        names(dat)[-1] <- c("DUR_OBS", "DUR_PRD")
        fstgdur(dat)
      })
    }


    # --------------------------------------------------------------------------------
    #
    # Output panels: Signals
    #
    # --------------------------------------------------------------------------------

    output$signal.master <- renderPlot({
      req(attached.edf())
      session$resetBrush("master_brush")
      leval("MASK clear")

      # hypnogram image used to select from the above
      par(mar = c(0, 0, 0, 0))

      if (ml.globals$staging_panel_present) {
        # use manual staging, if available

        if (values$has_manual_staging) {
          plot(values$ss$E, rep(0.5, length(values$ss$E)),
               col = lstgcols(values$ss$STAGE), axes = F, ylim = c(0, 1), pch = "|", ylab = "", xaxs = "i", yaxs = "i"
          )
        } else if (values$has_pops_staging) {
          pops_ss <- values$data$luna_suds_POPS$luna_suds_POPS_E$data$PRED
          pops_ep <- values$data$luna_suds_POPS$luna_suds_POPS_E$data$E

          plot(pops_ep, rep(0.5, length(pops_ss)),
               col = lstgcols(pops_ss), axes = F, ylim = c(0, 1), pch = "|", ylab = "", xaxs = "i", yaxs = "i"
          )
        }
      } else {
        # just fill in blank
        plot(seq(1, values$ne), rep(0.5, values$ne), axes = F, ylim = c(0, 1), pch = "|", ylab = "", xaxs = "i", yaxs = "i")
      }
    })

    output$signal.master2 <- renderPlot({
      req(attached.edf())
      par(mar = c(0, 0, 0, 0))
      plot(values$epochs, c(0.5, 0.5), col = "black", lwd = 5, type = "l", axes = F, ylab = "", xlab = "", ylim = c(0, 1), xlim = c(1, values$ne), xaxs = "i", yaxs = "i")
    })


    #
    # Summary PSD (SPSD) plot
    #


    output$signal.spsd <- renderPlot({
      req(attached.edf(), values$spsd)
      par(mar = c(0, 0, 0, 0))

      fspsd()
    })


    #
    # Primary signals plot
    #

    output$signal.view <- renderPlot(
      {
        req(attached.edf(), c(input$sel.ch, input$sel.ann))

        # reset MASK
        lrefresh()

        epochs <- values$epochs
        zoom <- values$zoom
        bp <- values$bandpass

        isolate({

          #    cat( "\nin renderPlot()\n" )

          # epochs are the (30-second) spanning epochs which are fetched (that always)
          # if zoom is defined, then back calculate

          # should not happen, but if for some reason nothing is defined,
          # display the first epoch:
          if (is.null(epochs) & is.null(zoom)) {
            epochs <- c(1, 1)
            zoom <- c(0, 30)
            values$raw.signals <- T
          } else {
            if (is.null(epochs)) {
              epochs <- c(floor((zoom[1] / 30) + 1), floor((zoom[2] / 30) + 1))
            }

            if (is.null(zoom)) {
              zoom <- c((epochs[1] - 1) * 30, epochs[2] * 30)
            }

            epochs <- c(floor(epochs[1]), ceiling(epochs[2]))
          }

          # compile final values: epochs and seconds (always round to nearest whole second)
          secs <- c(floor(zoom[1]), ceiling(zoom[2]))

          # we should now have a) the spanning epochs (for ldata() ) in values$epochs
          # and the range to display in values$zoom (in seconds)

          #    cat( "epochs : " , epochs , "\n" )
          #    cat( "seconds: " , secs , "\n" )

          # update raw signals status as needed: if more than 5 mins, use summary stats
          # values$raw.signals <- ( epochs[2] - epochs[1] ) < 10
          values$raw.signals <- (zoom[2] / 30 - zoom[1] / 30) < 10

          annots <- input$sel.ann
          chs <- input$sel.ch
          na <- length(annots)
          nc <- length(chs)


          #
          # Plot parameters
          #

          # room for text on left (but w/in plot),
          # is 20% of main span
          x0 <- secs[1] - (secs[2] - secs[1]) * 0.2
          xr <- range(x0, secs[2])

          # y-axis
          cfac <- 3 # channel : annotation y-expansion factor
          sfac <- 1.5 #           spanning factor (only for raw signals, not summ stats)

          # i.e. give chs x3 vertical space; +1 is spacer
          yinc <- 1.0 / (cfac * nc + na + 1)

          # width of y-range (might be > yinc, i.e. for partial overlap)
          yspan <- yinc * sfac

          # initiate y-poinyter (half an increment up)
          #      yp <- yinc * 0.5
          yp <- 0
          yidx <- 1

          # initiate plot
          par(mar = c(2.2, 0, 0, 0))

          plot(c(0, 1),
               type = "n",
               ylim = c(0, 1),
               xlim = xr, xaxt = "n", yaxt = "n", axes = F,
               xlab = "", ylab = ""
          )

          axis(1, c(secs[1], secs[2]))

          #
          # Zoomed-in hypnogram at top
          #

          stgs <- values$ss$STAGE
          enum <- values$ss$E

          for (e in epochs[1]:epochs[2]) {
            s <- secs[1] + (e - epochs[1]) * 30
            if (s < secs[2]) {
              s_end <- s + 30
              if (s_end > secs[2]) {
                s_end <- secs[2]
              }
              rect(s, 0.99, s_end, 1.00,
                   col = lstgcols(stgs[enum == e]),
                   border = NA
              )
            }
          }

          #
          # Signals
          #

          if (nc) {


            #
            # For short intervals, plot original data
            #

            if (values$raw.signals) {

              #
              # Pull raw signal data
              #

              yidx <- 0
              for (ch in rev(chs)) {
                req(epochs[1] >= 1, epochs[2] <= values$ne)
                dat <- ldata(epochs[1]:epochs[2], chs = ch)
                dat <- dat[dat$SEC >= secs[1] & dat$SEC <= secs[2], ]
                ts <- dat$SEC
                dy <- dat[, 4]

                # filter?
                if (values$bandpass) {
                  dy <- ldetrend(dy)
                  dy <- lfilter(dy, values$sr[ch], 0.3, 35)
                }
                yr <- range(dy)
                # zero-centered signal?
                zc <- yr[1] < 0 & yr[2] > 0
                # mean center
                dy <- dy - mean(dy)
                # max absolute value
                yrmx <- max(abs(range(dy)))
                # if +/- signal, scale to -1, +1 ( 0 .. 1 ) based on max( |x| )
                dy <- dy / (2 * yrmx)
                # convert to plot co-oords
                dy <- (yp + (yinc * cfac) / 2) + dy * yspan * cfac
                # plot
                cidx <- yidx %% 10 + 1
                lines(ts, dy, lwd = 0.75, col = ml.globals$pal10[cidx])
                # labels
                text(x0, yp + (yinc * cfac) / 2,
                     paste(ch, "\n(", signif(yr[1], 3), ",", signif(yr[2], 3), values$units[ch], ")"),
                     pos = 4, col = ml.globals$pal10[cidx], cex = 0.9
                )
                # drop down to next channel
                yp <- yp + yinc * cfac
                yidx <- yidx + 1
              }
            }

            #
            # else, plot reduced form if longer interval (if sigstats available)
            #

            else if (!is.null(values$sigstats)) {

              # sigstats data
              sigstats <- NULL

              #
              # sigstats contains two stats: S1 and S2
              #  if both defined, S1 = Hjorth 1,  S2 = Hjorth 2
              #  if only S1 defiend , S1 = mean  ( S2 == NA )

              sigstats <- values$sigstats[values$sigstats$E >= epochs[1] &
                                            values$sigstats$E <= epochs[2] &
                                            values$sigstats$CH %in% chs, ]

              # palette for H2
              pal100 <- rev(lplasma(100))

              # reset
              yidx <- 0

              for (ch in rev(chs)) {
                cidx <- yidx %% 10 + 1

                no_summaries <- sum(sigstats$CH == ch) == 0


                #
                # No summary data available for this channel
                #

                if (no_summaries) {
                  text(x0 + 0.1 * (xr[2] - xr[1]), yp + (yinc * cfac) / 2,
                       "... no summary values available ... \n... select a smaller region to view this signal ... ",
                       pos = 4, col = ml.globals$pal10[cidx], cex = 1
                  )
                  text(x0, yp + (yinc * cfac) / 2,
                       ch,
                       pos = 4, col = ml.globals$pal10[cidx], cex = 0.9
                  )
                } else {

                  #
                  # Either show means or Hjorth paramters, depending if we have just S1 or S1+S2 in sigstats
                  #

                  cs1 <- sigstats$S1[sigstats$CH == ch]
                  cs2 <- sigstats$S2[sigstats$CH == ch]
                  cse <- sigstats$E[sigstats$CH == ch]

                  use_mean <- any(is.na(cs2))

                  min.S1 <- min(cs1, na.rm = T)
                  max.S1 <- max(cs1, na.rm = T)
                  min.S2 <- ifelse(use_mean, NA, min(cs2, na.rm = T))
                  max.S2 <- ifelse(use_mean, NA, max(cs2, na.rm = T))

                  if (use_mean) {

                    #
                    # Show epoch-wise mean
                    #
                    zoomed.epochs <- c(floor((secs[1] / 30) + 1), floor((secs[2] / 30) + 1))
                    secx <- seq(secs[1], secs[2], length.out = zoomed.epochs[2] - zoomed.epochs[1] + 1) # 30 second epochs
                    ydat <- cs1[cse >= zoomed.epochs[1] & cse <= zoomed.epochs[2]]
                    # scale to 0..1
                    ydat <- (ydat - min.S1) / ifelse(max.S1 - min.S1 > 0, max.S1 - min.S1, 1) # normalize within range(?)
                    # scale to pixel co-ords (nb. dropped yspan)
                    dy <- yp + ydat * yinc * cfac

                    cidx <- yidx %% 10 + 1

                    secx <- secx[1:length(dy)] # normalize both axes

                    lines(secx, dy, lwd = 2, col = ml.globals$pal10[cidx])

                    # labels
                    text(x0, yp + (yinc * cfac) / 2,
                         paste(ch, "\n(", signif(min.S1, 3), ",", signif(max.S1, 3), values$units[ch], ")"),
                         pos = 4, col = ml.globals$pal10[cidx], cex = 0.9
                    )
                  } else {

                    #
                    # Show H1/H2
                    #

                    for (e in epochs[1]:epochs[2]) {
                      secx <- secs[1] + 30 * (e - epochs[1])

                      if (secx < secs[2]) {
                        secx_end <- secx + 30
                        if (secx + 30 > secs[2]) {
                          secx_end <- secs[2]
                        }
                        ydat <- cs1[cse == e]
                        ydat <- (ydat - min.S1) / ifelse(max.S1 - min.S1 > 0, max.S1 - min.S1, 1)
                        #    cat( "minx = " , min.S1 , max.S1 , "\n" )
                        h2 <- cs2[cse == e]
                        if (max.S2 - min.S2 > 0) {
                          h2 <- (h2 - min.S2) / (max.S2 - min.S2)
                        } else {
                          h2 <- rep(0, length(h2))
                        }
                        ycol <- floor(100 * h2)
                        ycol[ycol < 1] <- 1
                        rect(secx, yp + (yinc * cfac) / 2 - yspan * ydat,
                             secx_end, yp + (yinc * cfac) / 2 + yspan * ydat,
                             col = pal100[ycol], border = pal100[ycol]
                        )
                      }
                    }
                    text(x0, yp + (yinc * cfac) / 2, ch, pos = 4, col = "black")
                  } # end of Hjorth summ
                } # end of summary choice (none / mean / Hjorth )

                # more major increment
                yp <- yp + yinc * cfac
                yidx <- yidx + 1
              } # next channel
            } # end of sigstats views

            else {

              #
              # otherwise, print message about data not present (i.e. no summary data )
              #

              yidx <- 0
              cidx <- 0

              for (ch in rev(chs)) {
                cidx <- yidx %% 10 + 1

                text(x0 + 0.1 * (xr[2] - xr[1]), yp + (yinc * cfac) / 2,
                     "... no summary values available ... \n... select a smaller region to view this signal ... ",
                     pos = 4, col = ml.globals$pal10[cidx], cex = 1.0
                )

                # labels
                text(x0, yp + (yinc * cfac) / 2,
                     ch,
                     pos = 4, col = ml.globals$pal10[cidx], cex = 0.9
                )

                # drop down to next channel
                yp <- yp + yinc * cfac
                yidx <- yidx + 1
              }
            }
          } # end of 'if-channels'

          #
          # Annotations (these are pre-loaded) [ will be plotted at top ]
          #

          if (na) {
            df <- values$annot.inst$ANNOT_INST_T1_T2[, c("ANNOT", "START", "STOP")]
            df <- df[df$ANNOT %in% annots, ]
            df <- df[(df$START <= secs[2] & df$STOP >= secs[1]), ]
            # left/right censor
            df$START[df$START < secs[1]] <- secs[1]
            df$STOP[df$STOP > secs[2]] <- secs[2]

            for (annot in rev(annots)) {
              # color
              cidx <- 1 + (yidx %% 10)
              flt <- which(df$ANNOT == annot)

              for (a in flt) {
                rect(df$START[flt], yp + yinc / 2 + yinc / 4,
                     df$STOP[flt], yp + yinc / 2 - yinc / 4,
                     col = ml.globals$pal10[cidx], border = NA
                )
              }
              # labels
              legend(x0, yp + yinc / 2, annot, yjust = 0.5, fill = ml.globals$pal10[cidx], cex = 0.9, border = NA)

              # drop down to next annotation
              yp <- yp + yinc
              yidx <- yidx + 1
            }
          } # end if annots

          session$resetBrush("zoom_brush")
        }) # isolate
      },
      height = "auto"
    )


    #
    # Handle signal plot interactions
    #

    # Clear Annotation Instance selection on interaction with master plot

    clear_sel_inst <- function() {
      if (!is.null(input$sel.inst)) {
        updateSelectInput(session, "sel.inst", selected = "")
      }
    }

    # single-click jumps to a single epoch
    observeEvent(input$master_click, {
      if (is.null(input$master_brush)) {
        clear_sel_inst()
        values$epochs <- c(floor(input$master_click$x), floor(input$master_click$x))
        values$zoom <- NULL
      }
    })

    # double-click clears all
    observeEvent(input$master_dblclick, {
      clear_sel_inst()
      values$epochs <- c(1, 1)
      values$zoom <- NULL
    })

    # brush will zoom in to range of epochs
    observeEvent(input$master_brush, {
      clear_sel_inst()
      brush <- input$master_brush
      if (!is.null(brush)) {
        if (brush$xmin < 1 || brush$xmax > values$ne) {
          session$resetBrush("master_brush")
        } else {
          values$epochs <- c(brush$xmin, brush$xmax)
          values$zoom <- NULL
        }
      } else {
        values$epochs <- values$zoom <- NULL
      }
    })

    # Full Length selection
    observeEvent(input$entire.record, {
      req(attached.edf())
      session$resetBrush("master_brush")
      clear_sel_inst()
      values$epochs <- c(1, values$ne)
      values$zoom <- NULL
    })

    #    # set Y-axis rescaling
    #    observeEvent(input$rescale.ylim, {
    #      req(attached.edf())
    #      values$yscale <- ! values$yscale
    #    })

    # Apply bandpass filter to all signals
    observeEvent(input$bandpass, {
      req(attached.edf())
      values$bandpass <- !values$bandpass
    })

    # drive by annotation instance box
    observeEvent(input$sel.inst, {
      xx <- range(as.numeric(unlist(strsplit(input$sel.inst, " "))))
      xx <- c(floor(xx[1] / 30) + 1, ceiling(xx[2] / 30))
      values$epochs <- xx
      values$zoom <- NULL
      session$resetBrush("master_brush")
      session$resetBrush("zoom_brush")
      #    session$setBrush(
      #      brushId = "master_brush",
      #      coords = list(xmin=xx[1], xmax=xx[2]) )
      #      panel = 1
    })


    #  observeEvent( input$zoom_click , {
    #    if ( is.null( input$zoom_click$zoom_brush) ) {
    #      session$resetBrush( "master_brush" )
    #      values$zoom = NULL
    #    }
    #  })

    observeEvent(input$zoom_dblclick, {
      session$resetBrush("zoom_brush")
      #    session$resetBrush( "master_brush" )
      #    values$epochs = NULL
      values$zoom <- NULL
    })

    observeEvent(input$zoom_brush, {
      brush <- input$zoom_brush
      epochs <- values$epochs
      if (!is.null(brush) && !(brush$xmin < (epochs[1] - 1) * 30 || brush$xmax > epochs[2] * 30)) {
        values$zoom <- c(brush$xmin, brush$xmax)
      } else {
        values$zoom <- NULL
      }
    })

    observeEvent(input$button_epoch_prv, {
      req(attached.edf())
      clear_sel_inst()
      curr_epochs <- values$epochs
      values$zoom <- NULL
      session$resetBrush("master_brush")
      if (curr_epochs[1] > 1 && curr_epochs[1] <= values$ne && curr_epochs[2] > 1 && curr_epochs[2] <= values$ne) {
        values$epochs <- c(curr_epochs[1] - 1, curr_epochs[2] - 1)
      }
    })

    observeEvent(input$button_epoch_nxt, {
      req(attached.edf())
      clear_sel_inst()
      curr_epochs <- values$epochs
      values$zoom <- NULL
      session$resetBrush("master_brush")
      if (curr_epochs[1] > 0 && curr_epochs[1] < values$ne && curr_epochs[2] > 0 && curr_epochs[2] < values$ne) {
        values$epochs <- c(curr_epochs[1] + 1, curr_epochs[2] + 1)
      }
    })

    output$info2 <-
      renderText({
        req(attached.edf())

        # zoom info to display?

        zoom_info <- NULL

        if (!is.null(values$zoom)) {
          brush <- input$zoom_brush
          zoom_info <- paste0(". Zoomed in epoch range is: ", floor(values$zoom[1] / 30), " to ", ceiling(values$zoom[2] / 30))
        }

        epochs <- values$epochs
        if (is.null(epochs)) epochs <- c(1, 1)
        hrs <- ((epochs[1] - 1) * 30) / 3600

        all_good <- TRUE
        max_epoch <- values$ne
        if ((epochs[1] < 1 || epochs[2] > max_epoch) && is.null(input$master_brush)) {
          all_good <- FALSE
        }
        if ((epochs[1] < 1 || epochs[1] > max_epoch) && (epochs[2] < 1 || epochs[2] > max_epoch) && !is.null(input$master_brush)) {
          all_good <- FALSE
        }

        if (all_good) {
          paste0(
            "Epoch ", floor(epochs[1]), " to ", ceiling(epochs[2]),
            " (", (ceiling(epochs[2]) - floor(epochs[1]) + 1) * 0.5, " minutes)",
            " ", signif(hrs, 2), " hours from start", zoom_info,
            ifelse(values$bandpass, " (w/ 0.3-35 Hz filter)", " (unfiltered)")
          )
        } else {
          paste0("Selected value is out of range")
        }
      })


    # SPSD plots

    fspsd <- function() {

      # resolution
      #    seconds:  30 - 600 = d1
      #              600 - 3600 = d5
      #              3600 - 7200 = d10
      #              7200 +      = d30

      #   d1    up to 10 minutes  --> 1 sec resol ( up to 600 datapoints;  one epoch = 30 data points)
      #   d5    up to 1 hour      --> 5 sec resol ( up to 720 datapoints)
      #   d10   up to 2 hours     --> 10 sec resol ( up to 720 datapoints)
      #   d30   over 2 hours      --> 30 sec resol ( e.g. 1200 data points for 10 hour study)

      epochs <- values$epochs
      if (is.null(epochs)) epochs <- c(1, 1)
      t1 <- (epochs[1] - 1) * 30
      t2 <- (epochs[2]) * 30
      td <- t2 - t1
      dd <- data.frame()

      cat("t1,t2", t1, t2, td, "\n")

      if (td <= 600) {
        dd <- values$spsd$d1[t1:t2, ]
      } else if (td <= 3600) {
        dd <- values$spsd$d5[((t1 - 1) * 6 + 1):((t2 - 1) * 6 + 1), ]
      } else if (td <= 7200) {
        dd <- values$spsd$d10[((t1 - 1) * 3 + 1):((t2 - 1) * 3 + 1), ]
      } else {
        dd <- values$spsd$d30[(epochs[1]:epochs[2]), ]
      }

      # cat("epoch",epochs,"\n")
      # print(dim(dd))
      # print(head(dd))
      # d <- d[ ceil( t1/sc ) : floor( t2/sc ) , ]

      # dd <- spsd$d30
      # neg: slow, delta, sigma
      # pos: alpha, beta, gamma

      pcol <- c(6, 7)
      ncol <- c(1, 2, 3, 4, 5)

      n <- dim(dd)[1]
      tt <- 1:n
      dd[, ncol] <- dd[, ncol] * -1
      yr <- range(c(apply(dd[, pcol], 1, sum), apply(dd[, ncol], 1, sum)))
      # yr <- c(-1,1) * max( abs( yr ) )

      # x-axis scaling (20% blank at left, to match signals)
      xr <- range(tt)
      x0 <- xr[1] - (xr[2] - xr[1]) * 0.2
      xr <- range(x0, xr[2])
      par(mar = c(0, 0, 0, 0))
      plot(tt, rep(0, n), ylim = yr, xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", type = "n", axes = F, xlab = "", ylab = "", main = "", xlim = xr)
      abline(h = 0)

      bpal <- grDevices::colorRampPalette(c("blue", "white", "red"))(7)
      # pos
      ycurr <- rep(0, n)
      for (b in pcol) {
        polygon(c(tt, rev(tt)), c(dd[, b] + ycurr, rev(ycurr)), col = bpal[b])
        ycurr <- ycurr + dd[, b]
      }

      # neg
      ycurr <- rep(0, n)
      for (b in rev(ncol)) {
        polygon(c(tt, rev(tt)), c(dd[, b] + ycurr, rev(ycurr)), col = bpal[b])
        ycurr <- ycurr + dd[, b]
      }
    }


    # --------------------------------------------------------------------------------
    #
    # Output panels: Power spectra
    #
    # --------------------------------------------------------------------------------

    # helper function to set height of PSD plots
    psdplot_height <- function() {
      values$psd.plot.height <-
        250 * max(1, ceiling(length(input$sel.ch) / 2))
      return(values$psd.plot.height)
    }

    # wrap plotOutput in renderUI
    output$ui_psdplot <- renderUI({
      req(attached.edf(), input$sel.ch)
      plotOutput("power.spectra", height = psdplot_height(), width = "100%")
    })


    output$power.spectra <- renderPlot({
      req(attached.edf(), input$sel.ch)

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
          cat(annot, "is ann\n")
        }
        unmask_annots <- paste(annots, collapse = ",")
        k_mask <- leval(paste("MASK unmask-if", unmask_annots, sep = "="))
        if (tail(sort(k_mask$MASK$EMASK$N_RETAINED), n = 1)) {
          leval("RESTRUCTURE")
        }
        alabel <- paste0(annots, collapse = ", ")
      }
      # get PSD
      k <- leval(paste(
        "PSD spectrum dB sig=",
        paste0(input$sel.ch, collapse = ","),
        " max=",
        input$sel.freq[2],
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
        if (length(pwr) > 0) {
          plot(
            frq,
            pwr,
            col = "darkgreen",
            type = "l",
            lwd = 2,
            xlim = input$sel.freq,
            main = ch,
            xlab = "Frequency (Hz)",
            ylab = "Power (dB)"
          )
        }
      }
    })



    # --------------------------------------------------------------------------------
    #
    # Output panels: multi-tapers
    #
    # --------------------------------------------------------------------------------

    output$ui_mtmplot <- renderUI({
      req(attached.edf(), values$mtm.files)
      mtspec_list <-
        lapply(
          1:length(values$mtm.files),
          function(i) {
            imagename <- paste0("mtspec", i)
            imageOutput(imagename)
          }
        )

      do.call(tagList, mtspec_list)
    })

    observe({
      if (identical(values$mtm.files, character(0))) {
        return(NULL)
      }
      for (i in 1:length(values$mtm.files))
      {
        local({
          my_i <- i
          imagename <- paste0("mtspec", my_i)
          output[[imagename]] <-
            renderImage(
              {
                list(
                  src = values$mtm.files[my_i],
                  alt = "Image failed to render"
                )
              },
              deleteFile = FALSE
            )
        })
      }
    })


    # --------------------------------------------------------------------------------
    #
    # Output panels: issues
    #
    # --------------------------------------------------------------------------------

    if (opt_nap) {
      output$issue.table <- renderDataTable(
        {
          req(attached.edf(), values$issuesData)
          values$issuesData
        },
        rownames = FALSE,
        options = list(pageLength = 20, rownames = F, columnDefs = list(list(className = "dt-center", targets = "_all")))
      )
    }



    # --------------------------------------------------------------------------------
    #
    # Output panels: derived metrics
    #
    # --------------------------------------------------------------------------------

    if (opt_nap) {

      # update derived tables depending on group

      observe({
        req(input$sel.derived.group)

        # extract names/desc for the tables in this group (skipping the group-level desc)
        # i.e. everyhting other than 'desc' keyword in the list is assumed to be a list(desc,data) object

        derived_tables <-
          lapply(
            values$derived_data[[input$sel.derived.group]][names(values$derived_data[[input$sel.derived.group]]) != "desc"],
            "[[", "desc"
          )
        d.derived_tables <- as.list(names(derived_tables))
        names(d.derived_tables) <- unlist(derived_tables)

        updateSelectInput(
          session,
          "sel.derived.table",
          choices = d.derived_tables,
          label = paste(length(d.derived_tables), " table(s)")
        )
      })

      observe({
        req(input$sel.derived.table)
        col_names <-
          colnames(values$derived_data[[input$sel.derived.group]][[input$sel.derived.table]]$data)
        if (!is.null(col_names) && length(col_names) > 1) {
          if ("DISP_ID" %in% col_names) {
            disp_id_col_index <- match("DISP_ID", col_names)
            col_names <- col_names[-c(1, disp_id_col_index)]
          } else {
            col_names <- col_names[-1]
          }
        }
        updateSelectInput(
          session,
          "sel.derived.variables",
          choices = col_names,
          label = paste(length(col_names), " variable(s)")
        )
      })



      output$derived.view <- DT::renderDataTable(DT::datatable(
        {
          req(
            attached.edf(),
            input$sel.derived.group,
            input$sel.derived.table
          )
          ml.globals$derived_data <-
            values$derived_data[[input$sel.derived.group]][[input$sel.derived.table]]$data
          col_names_dt <- colnames(ml.globals$derived_data)
          ID_col <- colnames(ml.globals$derived_data)[ml.globals$ID_col_index]
          pre_select_row_index <<- which(ml.globals$derived_data[ID_col] == values$ID)
          if ("DISP_ID" %in% col_names_dt) ml.globals$derived_data <- subset(ml.globals$derived_data, select = -ID)
          ml.globals$derived_data
        },
        rownames = F,
        selection = list(mode = "single", selected = list(rows = c(pre_select_row_index), cols = c()), target = "row+column"),
        options = list(
          pageLength = 5,
          lengthMenu = list(c(5, 10, 25, -1), c("5", "10", "25", "All")),
          columnDefs = list(list(
            className = "dt-center", targets = "_all"
          ))
        )
      ))


      output$var_plot <- renderPlot({
        req(input$sel.derived.table, input$sel.derived.variables != "")
        ID_col <- colnames(ml.globals$derived_data)[ml.globals$ID_col_index]
        selected_var <- input$sel.derived.variables
        plot_val <- as.numeric(ml.globals$derived_data[, selected_var])
        row_val <- as.numeric(ml.globals$derived_data[ml.globals$derived_data[, ID_col] == values$ID, selected_var])
        nu <- length(unique(plot_val[!is.na(plot_val)]))
        if (nu < 10) {
          tt <- table(plot_val)
          cols <- rep(rgb(0, 0, 100, 100, max = 255), length(tt))
          cols[names(tt) == row_val] <- "red"
          barplot(tt, main = "", xlab = selected_var, col = cols)
        }
        if (nu >= 10) {
          hist(plot_val, main = "", xlab = selected_var, breaks = 20, col = rgb(0, 0, 100, 100, max = 255))
          abline(v = row_val, lwd = 5, col = "red")
        }
      })


      # Load EDF if selected row's ID field value is present in cohort
      observeEvent(input$derived.view_rows_selected, {
        ID_col <- colnames(ml.globals$derived_data)[ml.globals$ID_col_index]
        ID_col_data <- ml.globals$derived_data[ID_col]
        selected_edf <- ID_col_data[input$derived.view_rows_selected, ]
        req((selected_edf != values$ID), ml.globals$sm_allowChangeSelection)
        ml.globals$sm_allowChangeSelection <<- FALSE
        withProgress(message = "if available, selection will be changed...", {
          if (selected_edf %in% names(values$sl)) {
            updateSelectInput(session, "edfs", selected = selected_edf)
          } else {
            ml.globals$sm_allowChangeSelection <<- TRUE
          }
          req(ml.globals$sm_allowChangeSelection)
        })
      })

      output$selected.info <- renderPrint({
        if (!is.null(input$derived.view_columns_selected)) {
          cat("Plotting chart for column: ")
          col_names <- colnames(ml.globals$derived_data)
          cat(col_names[input$derived.view_columns_selected + 1])
          cat("\n")
        }

        if (!is.null(input$derived.view_rows_selected)) {
          new_id <- ml.globals$derived_data[input$derived.view_rows_selected, ml.globals$ID_col_index]
          if (any(new_id %in% names(values$sl))) {
            cat("Attaching new EDF: ", new_id)
          } else {
            cat("Could not find", new_id, "in the attached sample list")
          }
        }
      })
    }


    # --------------------------------------------------------------------------------
    #
    # End of server() function
    #
    # --------------------------------------------------------------------------------
  } # end of server logic



  # --------------------------------------------------------------------------------
  #
  # Run the application (if local = T , assume in Docker container)
  #
  # --------------------------------------------------------------------------------

  if (!local) {
    shiny::shinyApp(ui = ui, server = server, options = list(host = "0.0.0.0", port = 3838, launch.browser = F))
  } else {
    shiny::shinyApp(ui = ui, server = server)
  }
}
