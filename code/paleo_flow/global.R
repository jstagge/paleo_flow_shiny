###########################################################################
## Set the Paths
###########################################################################
### Path for Data and Output	
data_path <- "./data"
function_path <- "./functions"

###########################################################################
###  Load functions
###########################################################################
### Load these functions for all code
#require(colorout)
#require(assertthat)
require(tidyverse)

### Load these functions for this unique project
suppressPackageStartupMessages(library(googleVis))
library(dygraphs)
require(shinythemes)
require(lubridate)
require(xts)
#library(googleCharts)
library(MASS)
library(scales)
library(png)
require(shiny)
require(leaflet)
require(mailR)
require(shinyjs)
require(shinyBS)
require(digest)
require(tsbox)
require(shinycssloaders)
require(DT)
require(keyringr)

select <- dplyr::select

### Load project specific functions
#file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
#sapply(file.path(function_path, file.sources),source, local=TRUE)

source("./functions/create_site_list.R")
source("./functions/gof_ts.R")
source("./functions/read_in_paleo.R")
source("./functions/round_df.R")
source("./functions/dygraph-extra-shiny.R")
source("./functions/submit_funcs.R")
source("./functions/unit_conv.R")

source("external/pages/navbar_func.R")

###########################################################################
## Read in site information
###########################################################################
site_all <- readRDS(file.path(data_path,"site_all.rds"))


###########################################################################
## Read in flows
###########################################################################
#monthly_ts <- readRDS(file.path(data_path,"monthly_paleo/monthly_ts.rds"))
#annual_ts <- readRDS(file.path(data_path,"annual_paleo/annual_ts.rds"))

flow_db <- readRDS(file.path(data_path,"flow_db.rds"))

###########################################################################
## Needed for submissions
###########################################################################
# which fields get saved 
fieldsAll <- c("name", "user_email", "user_notes")

# which fields are mandatory
fieldsMandatory <- c("recon_name", "user_email", "upload")

# directory where responses get stored
responsesDir <- file.path("submissions")

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   error { color: red; }
  "

###########################################################################
## Read in email login	
###########################################################################
### Create a new password in linux
#login_pw  <- decrypt_gk_pw("user jhstagge domain ditigalocean")
login_pw <- scan("/home/pass.txt", character(), quote = "")



