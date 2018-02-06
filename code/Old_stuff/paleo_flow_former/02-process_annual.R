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
flow_obs$flow_m3s <- flow_obs$flow_cfs * ft3_to_m3 

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

###########################
###  Create a merged ts object
########################
flow_merge <- merge(x=flow_obs, y=flow_recon, by="Year", all=TRUE)

### Extract only important columns
#flow_plot <- data.frame(Observed=flow_plot_merge$monthly_mean, Annual_Recon = flow_plot_merge$annual_m3s, Monthly_Recon = flow_plot_merge$flow_rec_m3s) 
flow_plot <- data.frame(Month=1, flow_merge) 

###########################
###  Shorten to 20 years past end of monthly reconstruction
########################
date_df <- expand.grid(Year=seq(1,2017), Month=1)
flow_plot <- merge(x=date_df, y=flow_plot, by=c("Year", "Month"), all=TRUE)
### Re-sort to proper date order
flow_plot <- flow_plot[order(flow_plot$Year),] 

NonNAindex <- which(!is.na(flow_plot$Annual_Recon))
firstNonNA <- min(NonNAindex)

if ((firstNonNA -20) > 1){
	cut_series <- seq((firstNonNA-20), dim(flow_plot)[1])
	flow_plot <- flow_plot[cut_series, ]
}

###########################
###  Write to file
########################
write.csv(flow_plot, file.path(write_output_path,paste0("flow_",site_id,".csv")), row.names=FALSE)
write.csv(flow_plot, file.path(app_output_path,paste0("annual/flow_",site_id,".csv")), row.names=FALSE)

}




###########################################################################
## Process Rio Grande
###########################################################################
site_id <- "08220000"
### Read in Data
flow_recon <- read_table_wheaders(file.path(annual_folder,"riogrande2013flow.txt"), sep="\t", na.string="-9999")

### Cut to Correct format
flow_plot <- data.frame(Month=1, Year=flow_recon$age_AD, Observed=flow_recon$flow.obs.m3s, Annual_Recon=flow_recon$flow.rec.m3s)

###########################
###  Shorten to 20 years past end of monthly reconstruction
########################
date_df <- expand.grid(Year=seq(1,2017), Month=1)
flow_plot <- merge(x=date_df, y=flow_plot, by=c("Year", "Month"), all=TRUE)
### Re-sort to proper date order
flow_plot <- flow_plot[order(flow_plot$Year),] 

NonNAindex <- which(!is.na(flow_plot$Annual_Recon))
firstNonNA <- min(NonNAindex)

if ((firstNonNA -20) > 1){
	cut_series <- seq((firstNonNA-20), dim(flow_plot)[1])
	flow_plot <- flow_plot[cut_series, ]
}

###########################
###  Write to file
########################
write.csv(flow_plot, file.path(write_output_path,paste0("flow_",site_id,".csv")), row.names=FALSE)
write.csv(flow_plot, file.path(app_output_path,paste0("annual/flow_",site_id,".csv")), row.names=FALSE)





###########################################################################
## Process Yeruu
###########################################################################
site_id <- "yeruu"
### Read in Data
flow_recon <- read_table_wheaders(file.path(annual_folder,"yeruu2012flow.txt"), sep="\t", na.string="-9999")

### Cut to Correct format
flow_plot <- data.frame(Month=1, Year=flow_recon$age_AD, Observed=flow_recon$streamflow.inst, Annual_Recon=flow_recon$streamflow.recon)

###########################
###  Shorten to 20 years past end of monthly reconstruction
########################
date_df <- expand.grid(Year=seq(1,2017), Month=1)
flow_plot <- merge(x=date_df, y=flow_plot, by=c("Year", "Month"), all=TRUE)
### Re-sort to proper date order
flow_plot <- flow_plot[order(flow_plot$Year),] 

NonNAindex <- which(!is.na(flow_plot$Annual_Recon))
firstNonNA <- min(NonNAindex)

if ((firstNonNA -20) > 1){
	cut_series <- seq((firstNonNA-20), dim(flow_plot)[1])
	flow_plot <- flow_plot[cut_series, ]
}

###########################
###  Write to file
########################
write.csv(flow_plot, file.path(write_output_path,paste0("flow_",site_id,".csv")), row.names=FALSE)
write.csv(flow_plot, file.path(app_output_path,paste0("annual/flow_",site_id,".csv")), row.names=FALSE)








###########################################################################
## Process Snake River
###########################################################################
### Read in Data
flow_recon <- read_table_wheaders(file.path(annual_folder,"snake-flow2010.txt"), sep="\t", na.string="-9999")

13011000

## Convert to cfs
			unit_conv <- 35.31467
			### Convert to ac-ft per second
			unit_conv <- unit_conv * (1/43560)
			### Convert to ac-ft per month
			unit_conv <- unit_conv * 60*60*24*days_in_month(year_month())
			
			

### Cut to Correct format
flow_plot <- data.frame(Month=1, Year=flow_recon$age_AD, Observed=flow_recon$streamflow.inst, Annual_Recon=flow_recon$streamflow.recon)

###########################
###  Shorten to 20 years past end of monthly reconstruction
########################
date_df <- expand.grid(Year=seq(1,2017))
flow_plot <- merge(x=date_df, y=flow_plot, by="Year", all=TRUE)
### Re-sort to proper date order
flow_plot <- flow_plot[order(flow_plot$Year),] 

NonNAindex <- which(!is.na(flow_plot$Annual_Recon))
firstNonNA <- min(NonNAindex)

if ((firstNonNA -20) > 1){
	cut_series <- seq((firstNonNA-20), dim(flow_plot)[1])
	flow_plot <- flow_plot[cut_series, ]
}

###########################
###  Write to file
########################
write.csv(flow_plot, file.path(write_output_path,paste0("flow_",site_id,".csv")), row.names=FALSE)
write.csv(flow_plot, file.path(app_output_path,paste0("annual/flow_",site_id,".csv")), row.names=FALSE)




Snake river is similar, but requires converting from ac-ft to m3s

Same with yellowstone


