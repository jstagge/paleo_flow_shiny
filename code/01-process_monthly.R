# *------------------------------------------------------------------
# | PROGRAM NAME: 01-process_monthly
# | FILE NAME: 01-process_monthly.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  This code processes all monthly data for use in the paleostreamflow app.
# |
# *------------------------------------------------------------------
 
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
### Load these functions for this unique project
library(datasets)
require(lubridate)
require(ggplot2)
require(tidyverse)

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
site_id_list <- c("10109001", "10011500", "10128500")
site_name_list <- c("Logan River", "Bear River near Utah-Wyo", "Weber River")
recons_file_name_list <- c("logan2013flow.txt", "bear2015flow.txt", "weber2014flow.txt")

first_month_wy <- 10 ### Water Year starts on Oct 1
param_cd <- "00060"


### Create list of monthly reconstruction
month_reconst_file_list <- file.path(file.path(output_path,"paleo_monthly/paleo_monthly_gen/percentile_pred_model"))
month_reconst_file_list <- c(file.path(month_reconst_file_list, "10109001_percentile_pred_model_clim_pca_impute_concur_month_ts_rec_region.csv"), 
file.path(month_reconst_file_list, "10011500_percentile_pred_model_clim_pca_impute_concur_month_ts_rec.csv"))
month_reconst_file_list[3] <- "../../../output/paleo_weber/paleo_reconst/ts/10128500/10128500_apr_rec_clim_pca_impute_postproc_ts.csv"
#paleo_weber_old/apr_model/10128500_apr_model_enso_pca_impute_std_concur_reconst_ts_rec_local.csv"

### Create list of observed flows
obs_file_list <- file.path(output_path,"paleo_monthly/observed_utah_flow")
obs_file_list <- paste0(obs_file_list, "/", site_id_list[1:2], "_",param_cd,"_mon_wy.csv")
obs_file_list[3] <- "../../../output/paleo_weber/observed_utah_flow/10128500_00060_mon_wy.csv"


###########################################################################
## Create data for Logan
###########################################################################

for (n in seq(1,length(site_id_list))) {

### Read in file information
site_id <- site_id_list[n]
site_name <- site_name_list[n]
recons_file_name <- recons_file_name_list[n]
month_reconst_file <- month_reconst_file_list[n]

col_name <- tolower(unlist(strsplit(recons_file_name, "flow.txt")))

### Read in the original annual time series
annual_recon <- read_table_wheaders(file.path(data_path,paste0("paleo_flow_annual/Derose/",recons_file_name)), sep="\t", na.string="-9999") %>%
	rename("water_year" = "age_AD") 

### Read in observed flow and fix data type
#obs_file_name <- paste0(site_id,"_",param_cd,"_mon_wy.csv")
month_obs <- read_csv(obs_file_list[n], 
	col_types = cols(
		site_id = col_character(),
		site_name = col_character(),
		water_year = col_double(),
		year = col_integer(),
  		month = col_integer(),
  		date = col_date(format = ""),
  		monthly_sum = col_double(),
		monthly_mean = col_double(),
  		annual_sum = col_double(),
  		annual_mean = col_double()
	)) %>%
	arrange(date)

### Read in the reconstructed time series
if (site_id == "10128500") {
month_recon <- read_csv(month_reconst_file, 
	col_types = cols(
		year = col_integer(),
		month = col_integer(),
		flow_est = col_double()
	)) %>%
	rename(flow_rec_m3s = flow_est)

} else {
month_recon <- read_csv(month_reconst_file, 
	col_types = cols(
		site_id = col_character(),
		site_name = col_character(),
		water_year = col_integer(),
		year = col_integer(),
		month = col_integer(),
		date = col_date(format = ""),
		monthly_norm = col_double(),
		flow_rec_m3s = col_double(),
		method = col_character()
	)) %>%
	arrange(date)
}	

### Read in the original annual time series
#annual_recon <- read_table_wheaders(file.path(data_path,paste0("paleo_flow_annual/Derose/",recons_file_name)), sep="\t", na.string="-9999")


###########################
###  Extract the important parts of data prior to merging
###########################
month_obs <- month_obs %>%
	select(month, year, monthly_mean) %>%
	rename(obs_m3s = monthly_mean) %>%
	mutate(water_year = usgs_wateryear(year=year, month=month)
)

month_recon <- month_recon %>%
	select(month, year, flow_rec_m3s) %>%
	rename(recon_m3s = flow_rec_m3s) %>%
	drop_na(recon_m3s) %>%
	complete(year, month, fill = list(recon_m3s = NA)) %>%
	arrange(year, month)

if (site_id == "10109001"){
	annual_recon <- annual_recon %>% mutate(annual_m3s = flow.rec.region.m3s)
} else if (site_id == "10128500") {
	annual_recon <- annual_recon %>% mutate(annual_m3s = flow.rec.local.m3s)
} else {
	annual_recon <- annual_recon %>% mutate(annual_m3s = flow.rec.m3s)
}



annual_dates <- expand.grid(month=seq(1,12), water_year = seq(min(annual_recon$water_year, na.rm=TRUE)-2, max(annual_recon$water_year,na.rm=TRUE)+2))

annual_dates <- annual_dates %>% 
	mutate(year = usgs_wateryear_inverse(water_year, month = month)) %>%
	expand(year, month) %>%
	arrange(year, month) %>%
	mutate(water_year = usgs_wateryear(year, month))

annual_recon <- annual_recon  %>%
	right_join(annual_dates, by = "water_year") %>%
	select(month, year, water_year, annual_m3s) %>%
	drop_na(annual_m3s)



###########################
###  Create a merged ts object
########################


monthly_temp <- month_obs %>%
	full_join(month_recon, by=c("year" = "year", "month" = "month")) %>%
	full_join(annual_recon, by=c("year" = "year", "month" = "month"))

monthly_temp$col_name <- col_name 

monthly_temp  <-  monthly_temp %>%
	select(col_name, year, month, annual_m3s, obs_m3s, recon_m3s) %>%
	arrange(year, month)

###########################
###  Merge time series
###########################
if (n ==1) {
	monthly_ts <- monthly_temp
} else {
	monthly_ts <- rbind(monthly_ts, monthly_temp)
}

rm(month_obs)
rm(month_recon)
rm(annual_recon)
rm(monthly_temp)

}


###########################
###  Arrange by date
###########################
monthly_ts <- monthly_ts %>%
	arrange(year, month)

### Could add in a date column


###########################
###  Test plots
###########################
### Double check plot
plot_df <- monthly_ts %>%
	mutate(date = as.Date(paste0(year, "-", month, "-01")))

ggplot(plot_df, aes(x=date, y=recon_m3s, colour=col_name)) + geom_line() + theme_classic() + facet_grid(rows=vars(col_name))


plot_df <- monthly_ts %>%
	filter(col_name == "weber2014") %>%
	mutate(date = as.Date(paste0(year, "-", month, "-01"))) %>%
	select(date, annual_m3s, obs_m3s, recon_m3s) %>%
	gather("var", "flow_m3s", -date) 
ggplot(plot_df, aes(x=date, y=flow_m3s, colour=var)) + geom_line() + theme_classic() + coord_cartesian(xlim=c(as.Date("1940-01-01"), as.Date("1960-01-01")))


###########################################################################
## Save as rds file
###########################################################################
saveRDS(monthly_ts, file.path(write_output_path,"monthly_ts.rds"))


