
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
require(shiny)
library(dygraphs)
require(shinythemes)
require(lubridate)
require(xts)
library(googleCharts)
library(MASS)
library(scales)
library(shiny-incubator)

### Load project specific functions
#file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
#sapply(file.path(function_path, file.sources),source, local=TRUE)

source("./functions/create_site_list.R")
source("./functions/gof_ts.R")
source("./functions/read_in_paleo.R")
source("./functions/round_df.R")



###########################################################################
## Read in site information
###########################################################################
site_monthly <- read.table(file.path(data_path, "sites_monthly.txt"), sep="\t", header=TRUE,colClasses=rep("character", 11))
site_annual <- read.table(file.path(data_path, "sites_annual.txt"), sep="\t", header=TRUE,colClasses=rep("character", 11))

### Sort by group and then name
site_monthly <- site_monthly[order(site_monthly$site_group, site_monthly$site_name),] 
site_annual <- site_annual[order(site_annual$site_group, site_annual$site_name),] 

### Add resolution so can subset later
site_monthly$resolution <- "monthly"
site_annual$resolution <- "annual"

### Combine to create single lookup table with ids
site_all <- rbind(site_monthly, site_annual)
site_all <- data.frame(list_id=seq(1,dim(site_all)[1]), site_all)


