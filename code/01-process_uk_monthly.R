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
uk_data_path <- file.path(data_path, "paleo_flow_annual/UK")
output_path_base <- file.path(output_path,"paleo_flow_shiny")

app_output_path <- "./paleo_flow/data"
write_output_path <- file.path(app_output_path,"monthly")


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
require(rnrfa)
require(zoo)
require(rgdal)

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
site_id_list <- c("Dee", "Der", "Ed1", "Ed2", "Exe", "Ich", "Med", "Ous", "Tee", "Tef", "Thm", "Tyn", "Whf", "Wns", "Wye")
nrfa_id_list <- c(67015, 28010, 76005, 76002, 45001, 42010, 40003, 33035, 25001, 62001, 39008, 23001, 27043, 34004, 55023)
site_id_df <- data.frame(id=site_id_list, nrfa=nrfa_id_list)


###########################################################################
## Read in Existing Flow Data
###########################################################################
flow_obs_merge <- read.csv(file.path(write_output_path,"flow_obs.csv"))
flow_month_recon_merge <- read.csv(file.path(write_output_path,"flow_rec_month.csv"))
flow_annual_recon_merge <- read.csv(file.path(write_output_path,"flow_rec_annual.csv"))


###########################################################################
## Loop through each UK site
###########################################################################
for (n in seq(1, dim(site_id_df)[1])) {

### Set ids
site_id_n <- site_id_df$id[n]
nrfa_id_n <- site_id_df$nrfa[n]
rec_file_n <- file.path(uk_data_path, paste0(site_id_n,"_recflow.txt"))

### Read in reconstruction
flow_month_recon <- read.table(rec_file_n, sep="\t", header=FALSE)
colnames(flow_month_recon) <- c("year", "month", as.character(site_id_n))
head(flow_month_recon)

### Assume annual reconstruction is NA
flow_annual_recon <- flow_month_recon
flow_annual_recon[,3] <- NA

### Read in observed
flow_obs <- gdf(id = nrfa_id_n)
### If there is observed data
if (length(flow_obs)>0){
	### Convert to monthly mean
	flow_obs <- aggregate(flow_obs, as.Date(as.yearmon(time(flow_obs))), mean)
	flow_obs <- data.frame(year=year(time(flow_obs)), month=month(time(flow_obs)), flow_m3s=as.numeric(flow_obs))
} else {
	flow_obs <- data.frame(year=1990, month=1, flow_m3s=NA)
}	
colnames(flow_obs) <- c("year", "month", as.character(site_id_n))
head(flow_obs)
	
###  Merge time series
flow_obs_merge <- merge(x=flow_obs_merge, y=flow_obs, by=c("month", "year"), all=TRUE)
flow_month_recon_merge <- merge(x=flow_month_recon_merge, y=flow_month_recon, by=c("month", "year"), all=TRUE)
flow_annual_recon_merge <- merge(x=flow_annual_recon_merge, y=flow_annual_recon, by=c("month", "year"), all=TRUE)


}



###########################################################################
## List all possible years and re-sort the dataframe
###########################################################################
### Create a dataframe of year sequence
min_years <- min(c(flow_month_recon_merge$year,flow_annual_recon_merge$year,flow_obs_merge$year), na.rm=TRUE)
max_years <- max(c(flow_month_recon_merge$year,flow_annual_recon_merge$year,flow_obs_merge$year), na.rm=TRUE)
all_years <- seq(min_years, max_years)
all_years <- expand.grid(month=seq(1,12), year = all_years)

### Merge back with years and re-sort
flow_obs_merge <- merge(x=all_years, y=flow_obs_merge, by=c("month","year"), all.x=TRUE)
flow_obs_merge <- flow_obs_merge[with(flow_obs_merge, order(year, month)), ]

### Merge back with years and re-sort
flow_month_recon_merge <- merge(x=all_years, y=flow_month_recon_merge, by=c("month","year"), all.x=TRUE)
flow_month_recon_merge <- flow_month_recon_merge[with(flow_month_recon_merge, order(year, month)), ]

### Merge back with years and re-sort
flow_annual_recon_merge <- merge(x=all_years, y=flow_annual_recon_merge, by=c("month","year"), all.x=TRUE)
flow_annual_recon_merge <- flow_annual_recon_merge[with(flow_annual_recon_merge, order(year, month)), ]



###########################################################################
## Write results
###########################################################################

write.csv(flow_obs_merge, file.path(write_output_path,"flow_obs.csv"), row.names=FALSE)
write.csv(flow_month_recon_merge, file.path(write_output_path,"flow_rec_month.csv"), row.names=FALSE)
write.csv(flow_annual_recon_merge, file.path(write_output_path,"flow_rec_annual.csv"), row.names=FALSE)



###########################################################################
## Display site data
###########################################################################

data(stationSummary)

### Cut to only our stations
station_data <- merge(site_id_df, stationSummary, by.x="nrfa", by.y="id", all.x=TRUE)
### Re-sort alphabetically
station_data <- station_data[with(station_data, order(id)), ]
### Drop long text columns
station_data <- subset(station_data, select=-c(stationDescription, catchmentDescription, categories))

write.csv( station_data, "uk_station_data.csv")


