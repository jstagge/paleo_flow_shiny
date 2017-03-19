
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
require(googleVis)
require(shiny)
library(dygraphs)
library(datasets)
require(shinythemes)
require(lubridate)
require(xts)
library(googleCharts)
require(shiny)
require(shinythemes)


### Load project specific functions
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source, local=TRUE)

source("./functions/create_site_list.R")
source("./functions/gof_ts.R")
source("./functions/read_in_paleo.R")
source("./functions/round_df.R")



create_site_list <- function(file_name, res="monthly"){ 

### First, subset to only resolution
file_name_subset <- subset(file_name, resolution==res)

### Extract site_groups
site_group_list <- as.character(unique(file_name_subset$site_group))

site_list <- list()
site_list[[1]] <- c("Select site location"='')

### Loop through site_groups and create a unique list for each
for (n in seq(1, length(site_group_list))) {
	### Test for the group
	group_test <- file_name_subset$site_group == site_group_list[n]
	
	### Create an object with site name as header and site id as object
	site_list_temp <- file_name_subset$list_id[group_test]
	names(site_list_temp) <- file_name_subset$site_name[group_test]
	
	### Add to the longer list
	site_list[[(n+1)]] <- site_list_temp
	#if (n ==1) {
		
	#	site_list[[(n+1)]] <- site_list_temp
	#} else {
	#	site_list[[(n+1)]] <- site_list_temp
	#}
} 

### Name the groups in the full list
names(site_list) <- c(as.character("NA"), site_group_list)
	
return(site_list)
}


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


