# *------------------------------------------------------------------
# | PROGRAM NAME: 01-processing
# | FILE NAME: 01-processing.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  This code prepares flows to be read into the paleostreamflow app.
# | 
# | 
# | 
# *------------------------------------------------------------------
# | COMMENTS:               
# |
# |  1:  
# |  2: 
# |  3: 
# |*------------------------------------------------------------------
# | DATA USED:               
# | Streamflow reconstructions from Stagge et al. (2017) "Monthly paleostreamflow
# \ reconstruction from annual tree-ring chronologies." Journal of Hydrology
# |
# |*------------------------------------------------------------------
# | CONTENTS:               
# |
# |  PART 1:  
# |  PART 2: 
# |  PART 3: 
# *-----------------------------------------------------------------
# | UPDATES:               
# |
# |
# *------------------------------------------------------------------
 
### Clear any existing data or functions.
rm(list=ls())

###########################################################################
## Set the Paths
###########################################################################
### Path for Data and Output	
data_path <- "../../../data"
output_path <- "../../../output"
global_path <- "../../global_func"
function_path <- "./functions"

usgs_path <- file.path(data_path,"usgs_flow_data")

### Set the read-in folder
annual_folder <- file.path(data_path,"paleo_flow_annual")

### Set global output location
output_path_base <- file.path(output_path,"paleo_flow_shiny")

write_output_path <- file.path(output_path_base,"annual_paleo")
app_output_path <- "./paleo_flow/data"

### Set path for treeflow
output_path_treeflow <- file.path(output_path,"paleo_drivers_ccm")

### Create output folders
dir.create(output_path_base)
dir.create(write_output_path)
dir.create(app_output_path)
dir.create(file.path(app_output_path,"monthly"))
dir.create(file.path(app_output_path,"annual"))

###########################################################################
###  Load functions
###########################################################################
### Load these functions for all code
require(colorout)
require(assertthat)
require(reshape2)

### Load these functions for this unique project
library(datasets)
require(lubridate)
require(data.table)
require(ggplot2) 

### Load project specific functions
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)

### Load global functions
file.sources = list.files(global_path, pattern="*.R", recursive=TRUE)
sapply(file.path(global_path, file.sources),source)

require(staggefuncs)

###########################################################################
## Set Initial Values to Process Justin's Files
###########################################################################

### Set site data
site_id_list <- c("10109001", "10011500", "10128500")
site_name_list <- c("Logan Utah", "Bear River near Utah-Wyo", "Weber River")
recons_file_name_list <- c("logan2013flow.txt", "bear2015flow.txt", "weber2014flow.txt")

param_cd <- "00060"
wy_first_month <- 10 ### This is the default USGS water year, starting on Oct 1, it also follows Justin DeRose's reconstruction of MAF


###########################################################################
###  Save Daily Data
###########################################################################
### Loop through all site_ids and save time series data
for (n in seq(1,length(site_id_list))) {
	usgs_daily_dl(site_id_list[n], param_cd, dest_folder=usgs_path)
}


###########################################################################
###  Loop through all files calculate annual flow, merge, and export results
###########################################################################	
for (n in seq(1,length(site_id_list))) {

### Read in file information
site_id <- site_id_list[n]
site_name <- site_name_list[n]
recons_file_name <- recons_file_name_list[n]

col_name <- tolower(unlist(strsplit(recons_file_name, "flow.txt")))

###########################################################################
###  Read in Daily Data and Process to Annual
###########################################################################	
### Read in observed flows
flow_obs <- usgs_readin(site_id, param_cd="00060", destination_folder=usgs_path)

### Rename columns
colnames(flow_obs)[2] <- "site_id"
flow_obs$site_id <- as.factor(flow_obs$site_id)
colnames(flow_obs)[3] <- "date"
flow_obs$date <- as.Date(flow_obs$date)
colnames(flow_obs)[4] <- "flow_cfs"
colnames(flow_obs)[5] <- "cd"

### Apply a short name
flow_obs$short_name <- as.factor(site_name)

### Calculate date, month and year
flow_obs$month <- month(flow_obs$date)
flow_obs$year <- year(flow_obs$date)
	
### Calculate water year
flow_obs$water_year <- usgs_wateryear(year=flow_obs$year, month=flow_obs$month, first_month=wy_first_month)

### Convert to m3/s
flow_obs$flow_m3s <- as.numeric(flow_obs$flow_cfs) * ft3_to_m3 

### Create a datatable with keys to allow for Monthly and Annual mean calculations
flow_obs <- data.table(flow_obs)
setkey(flow_obs, site_id, month, year, water_year)

### Calculate mean annual flow based on water year and set water_year as key for sorting and merging
flow_obs_annual <- flow_obs[,list(annual_sum=sum(flow_m3s), annual_mean=mean(flow_m3s)),by="water_year"]
flow_obs_annual$site_id <- site_id
flow_obs_annual$site_name <- site_name
setkey(flow_obs_annual, water_year)

### Extract only the water year and annual mean
flow_obs <- data.frame(Year=flow_obs_annual$water_year, Observed=flow_obs_annual$annual_mean)
names(flow_obs) <- c("year", col_name)

###########################################################################
###  Read in Reconstructed Flows
###########################################################################
flow_recon <- read_table_wheaders(file.path(annual_folder,recons_file_name), sep="\t", na.string="-9999")

###########################
###  Extract the important parts of data prior to merging
###########################
if (site_id == "10109001"){
flow_recon <- data.frame(Year = flow_recon$age_AD, Annual_Recon = flow_recon$flow.rec.region.m3s)
} else if (site_id == "10128500") {
flow_recon <- data.frame(Year = flow_recon$age_AD, Annual_Recon = flow_recon$flow.rec.local.m3s)
} else {
flow_recon <- data.frame(Year = flow_recon$age_AD, Annual_Recon = flow_recon$flow.rec.m3s)
}
names(flow_recon) <- c("year", col_name)


###########################
###  Merge time series
###########################
if (n ==1) {
	flow_obs_merge <- flow_obs
	flow_recon_merge <- flow_recon
} else {
	flow_obs_merge <- merge(x=flow_obs_merge, y=flow_obs, by="year", all=TRUE)
	flow_recon_merge <- merge(x=flow_recon_merge, y=flow_recon, by="year", all=TRUE)
}


}



###########################################################################
## Process Rio Grande
###########################################################################
### Read in Data
flow_recon <- read_table_wheaders(file.path(annual_folder,"riogrande2013flow.txt"), sep="\t", na.string="-9999")

### Create data
flow_obs <- data.frame(year=flow_recon$age_AD, riogrande2013=flow_recon$flow.obs.m3s)
flow_recon <- data.frame(year=flow_recon$age_AD, riogrande2013=flow_recon$flow.rec.m3s)

### Merge data
flow_obs_merge <- merge(x=flow_obs_merge, y=flow_obs, by="year", all=TRUE)
flow_recon_merge <- merge(x=flow_recon_merge, y=flow_recon, by="year", all=TRUE)


###########################################################################
## Process Yerruu
###########################################################################
### Read in Data
flow_recon <- read_table_wheaders(file.path(annual_folder,"yeruu2012flow.txt"), sep="\t", na.string="-9999")

### Create data
flow_obs <- data.frame(year=flow_recon$age_AD, yeruu2012=flow_recon$streamflow.inst)
flow_recon <- data.frame(year=flow_recon$age_AD, yeruu2012=flow_recon$streamflow.recon)

### Merge data
flow_obs_merge <- merge(x=flow_obs_merge, y=flow_obs, by="year", all=TRUE)
flow_recon_merge <- merge(x=flow_recon_merge, y=flow_recon, by="year", all=TRUE)


###########################################################################
## Read in treeflow data
###########################################################################
### Read in Data
flow_obs <- read.csv(file.path(output_path_treeflow, "treeflow_obs.csv"))
flow_recon <- read.csv(file.path(output_path_treeflow, "treeflow_rec.csv"))

### Merge data
flow_obs_merge <- merge(x=flow_obs_merge, y=flow_obs, by="year", all=TRUE)
flow_recon_merge <- merge(x=flow_recon_merge, y=flow_recon, by="year", all=TRUE)




###########################################################################
## List all possible years and re-sort the dataframe
###########################################################################
### Create a dataframe of year sequence
min_years <- min(c(flow_recon_merge$year,flow_obs_merge$year), na.rm=TRUE)
max_years <- max(c(flow_recon_merge$year,flow_obs_merge$year), na.rm=TRUE)
all_years <- seq(min_years, max_years)
all_years <- data.frame(year = all_years)

### Merge back with years and re-sort
flow_recon_merge <- merge(x=all_years, y=flow_recon_merge, by="year", all.x=TRUE)
flow_recon_merge <- flow_recon_merge[with(flow_recon_merge, order(year)), ]

### Merge back with years and re-sort
flow_obs_merge <- merge(x=all_years, y=flow_obs_merge, by="year", all.x=TRUE)
flow_obs_merge <- flow_obs_merge[with(flow_obs_merge, order(year)), ]


###########################################################################
## Write to CSV files
###########################################################################

write.csv(flow_obs_merge, file.path(write_output_path, "flow_obs.csv"), row.names=FALSE)
write.csv(flow_recon_merge, file.path(write_output_path, "flow_rec.csv"), row.names=FALSE)





