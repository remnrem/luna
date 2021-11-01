
# --------------------------------------------------------------------------------
#
# lunaR moonlight viewer 
# v0.03, 28-Oct-2021
# http://zzz.bwh.harvard.edu/luna/
#
# --------------------------------------------------------------------------------


# --------------------------------------------------------------------------------
#
# wrapper function to set up and initiate the moonlight shiny app
#  either in basic or NAP mode
#  (in the latter case, we expect a "nap/" folder in the project directory)
#
# --------------------------------------------------------------------------------


moonlight <- function( sample.list = NULL , 
	               proj.path = NULL ,
		       nap.mode = FALSE ,
		       environ.file = NULL )
{

#sample.list = "s.lst"
#proj.path = "~/luna-base/"
#nap.mode = F
#environ.file = NULL

# --------------------------------------------------------------------------------
#
# dependencies
#
# --------------------------------------------------------------------------------

require(shiny , quietly = T )
require(DT , quietly = T )
require(shinyFiles , quietly = T )
require(xtable , quietly = T )
require(shinydashboard , quietly = T )


# --------------------------------------------------------------------------------
#
# Global variables for this shiny instance
#
# --------------------------------------------------------------------------------

options( shiny.sanitize.errors = FALSE )
#options( shiny.reactlog=TRUE )

SESSION_SLST = "s.lst"
SESSION_PATH = "."

#
# If using environment variables to configure a server-based instance
#

if ( ! is.null( environ.file ) )
{

# nb. can swap this to let 'environ.file' directly specify the file, if neeeded
R_environ_vars_file_path <- "~/.Renviron"

readRenviron( R_environ_vars_file_path )

if ( Sys.getenv( "USE_NAP" ) == "TRUE" ) opt_nap <- T
if ( Sys.getenv( "USE_S3" ) == "TRUE" ) opt_aws <- T
if ( Sys.getenv( "USE_AWS" ) == "TRUE" ) opt_aws <- T

# i.e. allow moonlight() args to over-ride environment variables:
if ( is.null( sample.list ) ) SESSION_SLST <- Sys.getenv("SESSION_SLST")
if ( is.null( proj.path ) ) SESSION_PATH <- Sys.getenv( "SESSION_PATH" )

}

# or, over-ride by moonlight() args
if ( ! is.null( sample.list ) ) SESSION_SLST = sample.list
if ( ! is.null( proj.path ) ) SESSION_PATH = proj.path


#
# Other deployment options
#

if ( ! is.null( nap.mode ) ) opt_nap <- nap.mode
opt_aws <- F
opt_eris <- F



# --------------------------------------------------------------------------------
#
# ERIS Deployment : i.e. hard-coded values for local (BWH/HMS) NSRR instance
#
# --------------------------------------------------------------------------------

if ( Sys.getenv( "ON_ERIS") == "TRUE" ) opt_eris <- T

# nb. ERIS

eris.metadata_lst <- "/home/shiny/nsrr-dataset/luna-link/metadata_lst.txt"
eris.home_lst <- "/home/shiny/nsrr-dataset/luna-link/nap_sl/"
eris.base_output_dir <- "/home/shiny/nsrr-dataset/luna-link/output"

# ERIS implies NAP mode
if (opt_eris) opt_nap <- T

# ERIS implies not AWS
if ( opt_eris & opt_aws ) stop( "Cannot proceed with AWS mode with ERIS deployment" )


# FIX

if ( SESSION_PATH != "" && ! opt_aws ){
  fixed.sl <- paste( SESSION_PATH, SESSION_SLST , sep="/", collapse = NULL)
} else {
  fixed.sl <- ""
}


#
# Point to NAP output directory, where we look for any tables/figures under nap.dir/{id}/
#

nap.dir <- paste( SESSION_PATH, "nap/", sep="/", collapse = NULL)



# --------------------------------------------------------------------------------
#
# AWS deployment
#
# --------------------------------------------------------------------------------

if ( opt_aws )
{
 require(aws.s3 , quietly = T )
 require(lubridate , quietly = T )
 require(wkb , quietly = T )
 require(digest , quietly = T )
 s3BucketName <- "nap-nsrr"
 enc_key <- charToRaw(Sys.getenv('ENCRYPT_KEY'))
 enc_iv <- charToRaw(Sys.getenv('ENCRYPT_IV'))
 AWS_ACCESS_KEY_ID <- Sys.getenv("AWS_ACCESS_KEY_ID")
 AWS_SECRET_ACCESS_KEY <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
 AWS_DEFAULT_REGION <- Sys.getenv("AWS_DEFAULT_REGION")
 aws.user <- ""
 aws.runid <- ""
}



# --------------------------------------------------------------------------------
#
# UI: either core moonlight, or w/ additional NAP panels
#
# --------------------------------------------------------------------------------

source( ifelse( opt_nap , "ml_ui_nap.R" , "ml_ui_moonlight.R" ) , local = T )

# --------------------------------------------------------------------------------
#
# ERIS deployment specific options: fixed sample list values
#
# --------------------------------------------------------------------------------

if ( opt_eris ) {
  metadata_lst <- eris.metadata_lst
  home_lst <- eris.home_lst
  base_output_dir <- eris.base_output_dir
}

# --------------------------------------------------------------------------------
#
# Global variables to track presence/absence of certain panels
#
# --------------------------------------------------------------------------------

annots_panel_present <- TRUE

staging_panel_present <- TRUE

pheno_panel_present <- TRUE

if ( opt_nap ) {
  sm_panel_present <- TRUE
  sm_allowChangeSelection <- TRUE
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

source( "ml_inp.R" , local = T  ) 

source( "ml_inp_staging.R" , local = T )

source( "ml_inp_server.R" , local = T )

if ( opt_nap ) source( "ml_inp_nap.R" , local = T )



#
# output panels
#

if ( opt_nap ) source( "ml_tab_phenotype.R" , local = T)

if ( opt_nap ) source( "ml_tab_nap.R" , local = T )
  
source( "ml_tab_headers.R" , local = T )

source( "ml_tab_annots.R" , local = T )

source( "ml_tab_staging.R" , local = T )

#source( "ml_tab_soap.R" , local = T )
  
if ( opt_nap ) source( "ml_tab_suds.R" , local = T )

source( "ml_tab_signals.R" , local = T )

source( "ml_tab_spectral.R" , local = T )
  
source( "ml_tab_mtm.R" , local = T )

if ( opt_nap ) source( "ml_tab_issues.R" , local = T )
  
if ( opt_nap ) source( "ml_tab_derived_metrics.R" , local = T )

}

# --------------------------------------------------------------------------------
#
# Run the application 
#
# --------------------------------------------------------------------------------

shiny::shinyApp( ui = ui, server = server )

}


