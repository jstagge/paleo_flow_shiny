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

### Set global output location
output_path_base <- file.path(output_path,"paleo_flow_shiny")

write_output_path <- file.path(output_path_base,"monthly_paleo")

### Create output folders
dir.create(output_path_base)
dir.create(write_output_path)

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

### Load project specific functions
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)

### Load global functions
file.sources = list.files(global_path, pattern="*.R", recursive=TRUE)
sapply(file.path(global_path, file.sources),source)


###########################################################################
## Set Initial Values
###########################################################################
### Set site data
site_id_list <- c("10109001", "10011500")
site_name_list <- c("Logan River", "Bear River near Utah-Wyo")
recons_file_name_list <- c("logan2013flow.txt", "bear2015flow.txt")

first_month_wy <- 10 ### Water Year starts on Oct 1
param_cd <- "00060"


###########################################################################
## Create data for Logan
###########################################################################
n <- 1


for (n in seq(1,length(site_id_list))) {

### Read in file information
site_id <- site_id_list[n]
site_name <- site_name_list[n]
recons_file_name <- recons_file_name_list[n]

### Create date information
date_df <- expand.grid(year=seq(1,2017), month=seq(1,12))
date_df$water_year <- usgs_wateryear(year=date_df$year, month=date_df$month)

### Read in reconst flows (use fread because of large header)

flow_recon <- read_table_wheaders(file.path(data_path,paste0("paleo_derose_annual_recon/",recons_file_name)), sep="\t", na.string="-9999")

flow_merge <- merge(date_df, flow_recon, by.x="water_year", by.y="age_AD", all.x=TRUE)
flow_merge$date <- as.Date(paste0(flow_merge$year, "-", flow_merge$month, "-01"))


### Read in observed flow and fix data type
obs_file_name <- paste0(site_id,"_",param_cd,"_mon_wy.csv")
flow_obs <- read.csv(file.path(output_path,paste0("paleo_monthly/observed_utah_flow/",obs_file_name)))
flow_obs$date <- as.Date(flow_obs$date)  
#head(flow_obs) # Review data frame


### Read in the reconstructed time series
reconst_location <- file.path(file.path(output_path,"paleo_monthly/paleo_monthly_gen"), "percentile_pred_model/10109001_percentile_pred_model_clim_pca_impute_concur_month_ts_rec_region.csv")
reconst_ts <- read.csv(reconst_location)

### Convert date field from character to date
reconst_ts$date <- as.Date(reconst_ts$date)
### Resort to proper date order
reconst_ts <- reconst_ts[order(reconst_ts$date),] 


### Read in the original annual time series
flow_recon <- read_table_wheaders(file.path(data_path,paste0("paleo_derose_annual_recon/",recons_file_name)), sep="\t", na.string="-9999")


###########################
###  Extract the important parts of data prior to merging
###########################
flow_obs_plot <- flow_obs[,c(seq(1,6),8)]
reconst_ts_plot <- reconst_ts[,c(seq(1,6),8)]
if (site_id == "10109001"){
annual_recon_plot <- data.frame(water_year = flow_recon$age_AD, annual_m3s = flow_recon$flow.rec.region.m3s)
} else {
annual_recon_plot <- data.frame(water_year = flow_recon$age_AD, annual_m3s = flow_recon$flow.rec.m3s)
}

###########################
###  Create a merged ts object
########################
flow_plot_merge <- merge(x=flow_obs_plot, y=reconst_ts_plot, by="date", all=TRUE)
### Add annual reconstruction
flow_plot_merge <- merge(x=flow_plot_merge, y=annual_recon_plot, by.x="water_year.y", by.y="water_year", all=TRUE)
### Re-sort to proper date order
flow_plot_merge <- flow_plot_merge[order(flow_plot_merge$date),] 

### Extract only important columns
#flow_plot <- data.frame(Observed=flow_plot_merge$monthly_mean, Annual_Recon = flow_plot_merge$annual_m3s, Monthly_Recon = flow_plot_merge$flow_rec_m3s) 
flow_plot <- data.frame(Month=month(flow_plot_merge$date), Year=year(flow_plot_merge$date), Observed=flow_plot_merge$monthly_mean, Annual_Recon = flow_plot_merge$annual_m3s, Monthly_Recon = flow_plot_merge$flow_rec_m3s) 

###########################
###  Shorten to 20 years past end of monthly reconstruction
########################
NonNAindex <- which(!is.na(flow_plot$Monthly_Recon))
firstNonNA <- min(NonNAindex)

if ((firstNonNA -20) > 1){
	cut_series <- seq((firstNonNA-20), dim(flow_plot)[1])
	flow_plot <- flow_plot[cut_series, ]
}

###########################
###  Write to file
########################
write.csv(flow_plot, file.path(write_output_path,paste0("flow_",site_id,".csv")), row.names=FALSE)

}

