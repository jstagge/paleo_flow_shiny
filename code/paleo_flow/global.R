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
require(reshape2)

### Load these functions for this unique project
require(ggplot2)
suppressPackageStartupMessages(library(googleVis))
library(dygraphs)
require(shinythemes)
require(lubridate)
require(xts)
#library(googleCharts)
library(MASS)
library(scales)
library(shinythemes)
library(png)
require(shiny)
require(leaflet)
require(mailR)
require(shinyjs)
require(shinyBS)
require(digest)

### Load project specific functions
#file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
#sapply(file.path(function_path, file.sources),source, local=TRUE)

source("./functions/create_site_list.R")
source("./functions/gof_ts.R")
source("./functions/read_in_paleo.R")
source("./functions/round_df.R")
source("./functions/dygraph-extra-shiny.R")
source("./functions/submit_funcs.R")

source("external/navbar_func.R")

###########################################################################
## Read in site information
###########################################################################
site_monthly <- read.table(file.path(data_path, "sites_monthly.txt"), sep="\t", header=TRUE,colClasses=c(rep("character", 10), rep("numeric",2)))
site_annual <- read.table(file.path(data_path, "sites_annual.txt"), sep="\t", header=TRUE,colClasses=c(rep("character", 10), rep("numeric",2)))

### Sort by group and then name
#site_monthly <- site_monthly[order(site_monthly$site_group, site_monthly$site_name),] 
#site_annual <- site_annual[order(site_annual$site_group, site_annual$site_name),] 

### Put USA first, external to US at end
usa_test <- substr(site_annual$site_group, 1, 3) == 'USA'

site_annual_usa <- site_annual[usa_test,] 
site_annual_usa <- site_annual_usa[order(site_annual_usa$site_group, site_annual_usa$site_name),]

site_annual_nonusa <- site_annual[!usa_test,] 
site_annual_nonusa <- site_annual_nonusa[order(site_annual_nonusa$site_group, site_annual_nonusa$site_name),]

site_annual <- rbind(site_annual_usa, site_annual_nonusa)


### Put USA first, external to US at end
usa_test <- substr(site_monthly$site_group, 1, 3) == 'USA'

site_monthly_usa <- site_monthly[usa_test,] 
site_monthly_usa <- site_monthly_usa[order(site_monthly_usa$site_group, site_monthly_usa$site_name),]

site_monthly_nonusa <- site_monthly[!usa_test,] 
site_monthly_nonusa <- site_monthly_nonusa[order(site_monthly_nonusa$site_group, site_monthly_nonusa$site_name),]

site_monthly <- rbind(site_monthly_usa, site_monthly_nonusa)

### Add resolution so can subset later
site_monthly$resolution <- "monthly"
site_annual$resolution <- "annual"

### Combine to create single lookup table with ids
site_all <- rbind(site_monthly, site_annual)
site_all <- data.frame(list_id=seq(1,dim(site_all)[1]), site_all)


###########################################################################
## Read in annual flows
###########################################################################

annual_flow_obs <- read.csv(file.path(data_path, "annual/flow_obs.csv"))
annual_flow_rec <- read.csv(file.path(data_path, "annual/flow_rec.csv"))


###########################################################################
## Read in monthly flows
###########################################################################

monthly_flow_obs <- read.csv(file.path(data_path, "monthly/flow_obs.csv"))
monthly_flow_rec_annual <- read.csv(file.path(data_path, "monthly/flow_rec_annual.csv"))
monthly_flow_rec_monthly <- read.csv(file.path(data_path, "monthly/flow_rec_month.csv"))


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
login_info <- read.csv("./private/login.csv", header=FALSE, colClasses = c("character"))

