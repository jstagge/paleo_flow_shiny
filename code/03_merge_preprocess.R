# *------------------------------------------------------------------
# | PROGRAM NAME: 03-merge_preprocess
# | FILE NAME: 03-merge_preprocess.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  This code merges and pre-processes annual and monthly flow.
# | 			It saves it for use in the paleflow website.
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

### Set the read-in folder
shiny_data_path <- "./paleo_flow/data"


### Set global output location
output_path_base <- file.path(output_path,"paleo_flow_shiny")

###########################################################################
###  Load functions
###########################################################################
### Load these functions for this unique project
require(lubridate)
require(tidyverse)
require(ggplot2) 
require(readxl)


###########################################################################
## Define constants
###########################################################################
### m3 per acre-ft
m3_acft <- 1233.481837548
### seconds per year
time_sec <- 365.25*24*60*60  

###########################################################################
## Combine monthly and annual time series
###########################################################################

annual_ts <- readRDS(file.path(output_path_base,"annual_paleo/annual_ts.rds"))
monthly_ts <- readRDS(file.path(output_path_base,"monthly_paleo/monthly_ts.rds"))

monthly_ts <- monthly_ts %>%
	mutate(resolution = "monthly")

annual_ts <- annual_ts %>%
	mutate(annual_m3s = NA, month = NA) %>%
	mutate(resolution = "annual") %>%
	select("col_name", "year", "month", "annual_m3s", "obs_m3s", "recon_m3s", "resolution")

flow_db <- rbind(monthly_ts, annual_ts)

saveRDS(flow_db, file.path(shiny_data_path,"flow_db.rds"))
saveRDS(flow_db, file.path(output_path_base,"flow_db.rds"))


###########################################################################
## Read in site data
###########################################################################
site_annual <-read_excel(file.path(shiny_data_path, "sites_annual.xlsx"))
site_monthly <-read_excel(file.path(shiny_data_path, "sites_monthly.xlsx"))

###########################################################################
## Read in Missou GOF and add to sites
###########################################################################
missou_sites <- read_table2(file.path(data_path, "paleo_flow_annual/missouri2016sites.txt"))

### Select only relevant columns
missou_sites <- missou_sites %>%
	mutate(col_name = paste0("usgs_", STAID)) %>%
	select(col_name, STANAME, LAT_GAGE, LNG_GAGE, adj.r2, RE_median, CE_median)

### Join with sites data
site_annual <- site_annual %>%
	left_join(missou_sites, by = c("col_name" = "col_name"))

### Insert lat, lon, and goodness of fit info
site_annual <- site_annual %>%
	mutate(lat = case_when(
		!is.na(STANAME) ~ LAT_GAGE,
		TRUE  ~ as.numeric(lat))) %>%
	mutate(lon = case_when(
		!is.na(STANAME) ~ LNG_GAGE,
		TRUE  ~ as.numeric(lon))) %>%
	mutate(reported_cal_r2 = case_when(
		!is.na(STANAME) ~ adj.r2,
		TRUE  ~ as.numeric(reported_cal_r2))) %>%
	mutate(reported_val_ce = case_when(
		!is.na(STANAME) ~ RE_median,
		TRUE  ~ as.numeric(reported_val_ce))) 

### Remove columns
site_annual <- site_annual %>%
	select(-STANAME, -LAT_GAGE, -LNG_GAGE, -adj.r2, -RE_median, -CE_median)

###########################################################################
## Combine site data
###########################################################################
### Modify the annual site table, resort and add an annual column
site_annual <- site_annual %>%
	mutate(site_group_split = site_group) %>%
	separate(col="site_group_split", sep=",", into=c("country", "region"), extra="warn") %>% ### Separate the group into country and region columns
	mutate(resolution = "annual") %>%	### Add resolution so can subset later
	mutate(usa_test = country == "USA") %>%   ### Create a column to keep USA first for ease of data finding
	arrange(-usa_test, country, region, site_name)		### Sort by USA, country, region, site name

## Modify the monthly site table, resort and add an monthly column
site_monthly <- site_monthly %>%
	mutate(site_group_split = site_group) %>%
	separate(col="site_group_split",  sep=",", into=c("country", "region"), extra="warn") %>% ### Separate the group into country and region columns
	mutate(resolution = "monthly") %>% ### Add resolution so can subset later
	mutate(usa_test = country == "USA") %>%   ### Create a column to keep USA first for ease of data finding
	arrange(-usa_test, country, region, site_name)		### Sort by USA, country, region, site name

### Combine to create single lookup table with ids
site_all <- rbind(site_monthly, site_annual)


###########################################################################
## Convert units for reported GOF
###########################################################################
### Split ranges of SEE into columns
site_all <- site_all %>%
	separate(reported_cal_see, c("reported_cal_see_min", "reported_cal_see_max"), sep="-", remove=FALSE, convert=TRUE) %>%
	mutate(reported_cal_see = case_when (reported_cal_see_max > 0 ~ NA_character_ ,
		TRUE ~ reported_cal_see)) %>%
	mutate(reported_cal_see = as.numeric(reported_cal_see))

### Split ranges of RMSE into columns
site_all <- site_all %>%
	separate(reported_val_rmse, c("reported_val_rmse_min", "reported_val_rmse_max"), sep="-", remove=FALSE, convert=TRUE) %>%
	mutate(reported_val_rmse = case_when (reported_val_rmse_max > 0 ~ NA_character_ ,
		TRUE ~ reported_val_rmse)) %>%
	mutate(reported_val_rmse = as.numeric(reported_val_rmse))

### A function to convert to m3s
scale_tom3s <- function(x, reported_units, na.rm = FALSE)  (case_when (reported_units == "af" ~ x * (m3_acft/time_sec),
				reported_units == "kaf" ~ x * 1E3 * (m3_acft/time_sec),
				reported_units == "maf" ~ x * 1E6 * (m3_acft/time_sec),
				reported_units == "cfs" ~ x / 35.31467,
				reported_units == "m3s" ~ x,
				TRUE ~ x)	)

### Convert all to m3s
site_all <- site_all %>% 
	mutate(reported_units = case_when(reported_units == "m3/s" ~ "m3s", TRUE ~ reported_units)) %>%
	mutate_at(c("reported_cal_see", "reported_cal_see_min", "reported_cal_see_max", "reported_val_rmse", "reported_val_rmse_min", "reported_val_rmse_max"), scale_tom3s, reported_units = site_all$reported_units)



###########################################################################
## Recode validation methods
###########################################################################
site_all <- site_all %>% 
	mutate(val_method = recode(val_method, loo = "Leave-One-Out", 'split-sample' = "Split Sample"))

###########################################################################
## Save annual data to rds file
###########################################################################

saveRDS(site_annual, file.path(shiny_data_path,"site_annual.rds"))
saveRDS(site_monthly, file.path(shiny_data_path,"site_monthly.rds"))

saveRDS(site_all, file.path(shiny_data_path,"site_all.rds"))
saveRDS(site_all, file.path(output_path_base,"site_all.rds"))



###########################################################################
## Double check for missing column names
###########################################################################
yup <- site_annual %>%
	full_join(select(flow_db, col_name, recon_m3s), by ="col_name") %>%
	group_by(col_name) %>%
	summarise(n = sum(recon_m3s > -9999, na.rm=TRUE), n_naflow = sum(is.na(recon_m3s)), n_na_sitename = sum(is.na(site_name)))


### 81 places in flow_db with an NA in the col_names
	
### 135 sites with no flow	
yup %>% 
	filter(n == 0)

missing_flow <- yup %>%
	filter(n == 0) %>%
	arrange(col_name) %>%
	select(col_name) %>%
	unlist()
	
missing_flow

### 6 places with flow but no column name (including NA for column names)
missing_sitedata <- yup %>%
	filter(n_na_sitename != 0) %>%
	arrange(col_name) %>%
	select(col_name) %>%
	unlist()
	
missing_sitedata