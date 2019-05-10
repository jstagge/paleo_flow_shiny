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
### Load these functions for all code
require(colorout)
require(assertthat)
require(reshape2)

### Load these functions for this unique project
require(lubridate)
require(tidyverse)
require(ggplot2) 

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


###########################################################################
## Combine site data
###########################################################################

site_monthly <- read.table(file.path(shiny_data_path, "sites_monthly.txt"), sep="\t", header=TRUE,colClasses=c(rep("character", 10), rep("numeric",2)))
site_annual <- read.table(file.path(shiny_data_path, "sites_annual.txt"), sep="\t", header=TRUE,colClasses=c(rep("character", 10), rep("numeric",2)))

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

saveRDS(site_all, file.path(shiny_data_path,"site_all.rds"))





