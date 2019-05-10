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
library(dataRetrieval)
require(lubridate)
require(tidyverse)
require(ggplot2) 

### Load project specific functions
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)

### Load global functions
file.sources = list.files(global_path, pattern="*.R", recursive=TRUE)
sapply(file.path(global_path, file.sources),source)

require(staggefuncs)



###########################################################################
## Define constants
###########################################################################
### m3 per acre-ft
m3_acft <- 1233.481837548
### seconds per year
time_sec <- 365.25*24*60*60  

###########################################################################
## Set Initial Values to Process Justin's Files
###########################################################################

### Set site data
site_id_list <- c("10109001", "10109001", "10011500", "10128500")
site_name_list <- c("Logan Utah Local", "Logan Utah Regional","Bear River near Utah-Wyo", "Weber River")
recons_file_name_list <- c("logan2013flow.txt", "logan2013flow.txt","bear2015flow.txt", "weber2014flow.txt")
col_name_list <- c("logan2013_local", "logan2013_regional", "bear2015", "weber2014")

param_cd <- "00060"
wy_first_month <- 10 ### This is the default USGS water year, starting on Oct 1, it also follows Justin DeRose's reconstruction of MAF

###########################################################################
###  Loop through all files calculate annual flow, merge, and export results
###########################################################################	
for (n in seq(1,length(site_id_list))) {

### Read in file information
site_id <- site_id_list[n]
site_name <- site_name_list[n]
recons_file_name <- recons_file_name_list[n]
col_name <- col_name_list[n]

recons_file_name <- paste0("Derose/",recons_file_name)

###########################################################################
###  Read in Daily Data and Process to Annual
###########################################################################	
### Read in observed flows
parameterCd <- "00060" # Discharge
statCd <- "00003"  # Mean
startDate <- "1851-01-01"
endDate <-Sys.Date()

flow_obs <- readNWISdv(site_id, parameterCd, startDate, endDate, statCd=statCd) 

### Rename columns and create month / year cols
flow_obs <- flow_obs %>%
	select(-agency_cd) %>%
	rename(date = Date) %>%
	rename(site_id = site_no) %>%
	rename(flow_cfs = X_00060_00003) %>%
	rename(cd = X_00060_00003_cd) %>%
	mutate(month = month(date)) %>%
	mutate(year = year(date))

### Calculate water year
flow_obs$water_year <- usgs_wateryear(year=flow_obs$year, month=flow_obs$month, first_month=wy_first_month)

### Convert to m3/s
flow_obs$flow_m3s <- as.numeric(flow_obs$flow_cfs) * ft3_to_m3 

### Calculate mean annual flow based on water year 
flow_obs_annual <- flow_obs %>%
	group_by(water_year) %>%
	summarise(annual_sum=sum(flow_m3s, na.rm=TRUE), annual_mean=mean(flow_m3s, na.rm=TRUE), n_na=sum(is.na(flow_m3s))) %>%
	filter(n_na < 20)

### Add columns
flow_obs_annual$site_id <- site_id
flow_obs_annual$site_name <- site_name
flow_obs_annual$col_name <- col_name

### Extract only the water year and annual mean
flow_obs_annual <- flow_obs_annual %>%
	select(col_name, water_year, annual_mean) %>%
	rename(year = water_year) %>%
	rename(obs_m3s = annual_mean)


###########################################################################
###  Read in Reconstructed Flows
###########################################################################
flow_recon_temp <- read_table_wheaders(file.path(annual_folder,recons_file_name), sep="\t", na.string="-9999")

###########################
###  Extract the important parts of data prior to merging
###########################
flow_recon <- tibble(col_name = col_name, year = flow_recon_temp$age_AD)

if (site_id == "10109001"){
	if(col_name == "logan2013_local"){
		flow_recon$recon_m3s <- flow_recon_temp$flow.rec.local.m3s
	} else {
		flow_recon$recon_m3s <- flow_recon_temp$flow.rec.region.m3s
	}
} else if (site_id == "10128500") {
flow_recon$recon_m3s <- flow_recon_temp$flow.rec.local.m3s
} else {
flow_recon$recon_m3s <- flow_recon_temp$flow.rec.m3s
}

rm(flow_recon_temp)

###########################
###  Create a merged ts object
########################
annual_temp <- flow_obs_annual %>%
	full_join(flow_recon, by=c("year" = "year", "col_name" = "col_name")) %>%
	arrange(year)

###########################
###  Merge time series
###########################
if (n ==1) {
	annual_ts <- annual_temp
} else {
	annual_ts <- rbind(annual_ts, annual_temp)
}

rm(flow_obs_annual)
rm(flow_recon)
rm(annual_temp)

}

### Save to RDS file
saveRDS(annual_ts, file.path(write_output_path,"annual_ts.rds"))

###########################################################################
## Process Rio Grande
###########################################################################
annual_ts <- readRDS(file.path(write_output_path,"annual_ts.rds"))

### Read in Data
annual_temp <- read_table2(file.path(annual_folder,"riogrande2013flow.txt"), skip=105, na="-9999" )

annual_temp <- annual_temp %>%
	mutate(col_name = "riogrande2013") %>%
	rename("recon_m3s" = 'flow-rec-m3s') %>%
	rename("obs_m3s" = "flow-obs-m3s") %>%
	rename("year" = 'age_AD') %>%
	select(col_name, year, obs_m3s, recon_m3s)

### Merge data
annual_ts <- rbind(annual_ts, annual_temp)
rm(annual_temp)

### Save to RDS file
saveRDS(annual_ts, file.path(write_output_path,"annual_ts.rds"))

###########################################################################
## Process Yerruu
###########################################################################
annual_ts <- readRDS(file.path(write_output_path,"annual_ts.rds"))

### Read in Data
annual_temp <- read_delim(file.path(annual_folder,"Mongolia/yeruu2012flow.txt"), delim="\t", skip=93, na="-9999")

### Create data
annual_temp <- annual_temp %>%
	mutate(col_name = "yeruu2012") %>%
	rename("obs_m3s" = 'streamflow-inst') %>%
	rename("recon_m3s" = 'streamflow-recon') %>%
	rename("year" = 'age_AD') %>%
	select(col_name, year, obs_m3s, recon_m3s)

### Merge data
annual_ts <- rbind(annual_ts, annual_temp)

rm(annual_temp)


### Save to RDS file
saveRDS(annual_ts, file.path(write_output_path,"annual_ts.rds"))

###########################################################################
## Process Kherlen
###########################################################################
annual_ts <- readRDS(file.path(write_output_path,"annual_ts.rds"))

### Read in Data
annual_temp <- read_delim(file.path(annual_folder,"Mongolia/kherlen2013flow.txt"), delim="\t", skip=94)

### Create data
annual_temp <- annual_temp %>%
	mutate(col_name = "kherlen2013") %>%
	mutate(obs_m3s = NA) %>%
	rename("recon_m3s" = 'flow-rec-m3s') %>%
	rename("year" = 'age_AD') %>%
	select(col_name, year, obs_m3s, recon_m3s)

### Merge data
annual_ts <- rbind(annual_ts, annual_temp)

rm(annual_temp)


### Save to RDS file
saveRDS(annual_ts, file.path(write_output_path,"annual_ts.rds"))

###########################################################################
## Process Selenge
###########################################################################
annual_ts <- readRDS(file.path(write_output_path,"annual_ts.rds"))

### Read in Data
annual_temp <- read_table(file.path(annual_folder,"Mongolia/selenge-riverflow.txt"), skip=72)

### Create data
annual_temp <- annual_temp %>%
	mutate(col_name = "selenge") %>%
	mutate(obs_m3s = NA) %>%
	rename("recon_m3s" = 'Recon') %>%
	rename("year" = 'YEAR') %>%
	select(col_name, year, obs_m3s, recon_m3s)

### Merge data
annual_ts <- rbind(annual_ts, annual_temp)

rm(annual_temp)



### Save to RDS file
saveRDS(annual_ts, file.path(write_output_path,"annual_ts.rds"))

###########################################################################
## Process Australia
###########################################################################
annual_ts <- readRDS(file.path(write_output_path,"annual_ts.rds"))

### Read in Data
annual_temp <- read_table2(file.path(annual_folder,"Australia/burdekin_flow_edit.txt"), skip=50)

### Process reconstruction
### Burdekin at Clare 120006B
burdekin_area <-129875 #### drainage area in km2
burdekin_area <- burdekin_area * 1000
#### Lat lon  -19.6 147.4 
#totalled for the water year October to September. 

### Create data
annual_temp <- annual_temp %>%
	mutate(col_name = "burdekin") %>%
	rename("recon_mm_year" = 'Burdekin') %>%
	mutate(recon_m_year = recon_mm_year / 1000) %>%
	mutate(recon_m3_year = recon_m_year * burdekin_area) %>%
	mutate(recon_m3s = recon_m3_year /time_sec) %>%
	rename("year" = 'Year') %>%
	select(col_name, year, recon_m3s)

### Process Burdekin Observed

burdekin_obs <- read_csv(file.path(annual_folder,"Australia/csv.w00066.20190510102434.120006B.csv"), skip=9)

burdekin_obs <- burdekin_obs %>%
	mutate(Timestamp_tz = with_tz(Timestamp, tzone="Australia/Brisbane")) %>%
	mutate(month = month(Timestamp_tz), year = year(Timestamp_tz))

### Calculate water year
burdekin_obs$water_year <- usgs_wateryear(year=burdekin_obs$year, month=burdekin_obs$month, first_month=10)

burdekin_obs_mean <- burdekin_obs %>%
	group_by(water_year) %>%
	summarise(obs_m3s = mean(Value, na.rm=TRUE), n_na=sum(is.na(obs_m3s))) %>%
	filter(n_na < 20) %>%
	rename("year" = "water_year")

### Combine obs with recon
annual_temp <- annual_temp %>%
	full_join(burdekin_obs_mean, by= "year") %>%
	select(col_name, year, obs_m3s, recon_m3s) %>%
	arrange(year)

### It appears there is a 1,000 fold error in the reconstuction
plot(annual_temp$year, annual_temp$recon_m3s, type="l", ylim=c(0,2500)) 
lines(annual_temp$year, annual_temp$recon_m3s*1000, col="blue") 
lines(annual_temp$year, annual_temp$obs_m3s, col="red")

### Make this adjustment
annual_temp <- annual_temp %>%
	mutate(recon_m3s = recon_m3s * 1000)

### Merge data
annual_ts <- rbind(annual_ts, annual_temp)

rm(annual_temp)

### Save to RDS file
saveRDS(annual_ts, file.path(write_output_path,"annual_ts.rds"))

###########################################################################
## Process Chile
###########################################################################
annual_ts <- readRDS(file.path(write_output_path,"annual_ts.rds"))

recons_file_name_list <- c("atuel_flow.txt", "limay_flow.txt", "neuquen_flow.txt")

###  Loop through all files calculate annual flow, merge, and export results
for (n in seq(1,length(site_id_list))) {

### Read in file information
recons_file_name <- recons_file_name_list[n]
col_name <- tolower(unlist(strsplit(recons_file_name, "_flow.txt")))

### Read in Data
annual_temp <- read_table2(file.path(annual_folder,paste0("Chile/",recons_file_name)), skip=1, guess_max=1000)
annual_temp <- annual_temp %>%
	rename("year" = "Year")

year_vec <- annual_temp %>%
	select(year) 
year_vec 

flow_alone <- annual_temp %>%
	select(-year) %>%
	as.data.frame()

flow_alone <- as.vector(t(flow_alone))
flow_alone <- as.numeric(na.omit(flow_alone))

if (col_name != "atuel"){
	flow_alone <- flow_alone * 1E6  ### convert from hectometers 3 to m3
	flow_alone <- flow_alone / time_sec  ### Convert from m3/year to m3/s
}

year_vec <- seq(min(year_vec,na.rm=TRUE), length.out=length(flow_alone))

### Create data
annual_temp <- tibble(col_name = col_name, year=year_vec, obs_m3s=NA, recon_m3s = flow_alone)


### Merge data
annual_ts <- rbind(annual_ts, annual_temp)

rm(annual_temp, year_vec, flow_alone)

}


### Save to RDS file
saveRDS(annual_ts, file.path(write_output_path,"annual_ts.rds"))


###########################################################################
## Process New Zealand
###########################################################################
annual_ts <- readRDS(file.path(write_output_path,"annual_ts.rds"))

### Read in reconstruction
annual_temp <- read_table2(file.path(annual_folder,"New_Zealand/hurunui_flow.txt"), skip=1, guess_max=1000)
annual_temp <- annual_temp %>%
	rename("year" = "Year")

year_vec <- annual_temp %>%
	select(year) 
year_vec 

flow_alone <- annual_temp %>%
	select(-year) %>%
	as.data.frame()

flow_alone <- as.vector(t(flow_alone))
flow_alone <- as.numeric(na.omit(flow_alone))

year_vec <- seq(min(year_vec,na.rm=TRUE), length.out=length(flow_alone))

### Create data
annual_temp <- tibble(col_name = "hurunui", year=year_vec, recon_m3s = flow_alone)

### File lists flow as m2/s * 10, assume this is typo meant to be m3/s * 10 becasue it lines up well
annual_temp <- annual_temp %>%
	mutate(recon_m3s = recon_m3s/10)


### Process Hurunui Observed
### Hurunui at Mandamus gauge 65104
### Nov - Jan flow
hurunui_obs <- read_csv(file.path(annual_folder,"New_Zealand/65104_obs.csv"), skip=1)

hurunui_obs <- hurunui_obs %>%
	rename("Timestamp" = `Timestamp (UTC+12:00)`) %>%
	rename("obs_m3s" = `Value (Cubic Metres Per Second)`) %>%
	mutate(Timestamp_tz = with_tz(Timestamp, tzone="Pacific/Auckland")) %>%
	mutate(date = date(Timestamp_tz))

### Calculate daily mean 
hurunui_obs <- hurunui_obs %>%
	group_by(date) %>%
	summarise(obs_m3s = mean(obs_m3s, na.rm=TRUE))

ggplot(hurunui_obs, aes(x = date, y= obs_m3s)) + geom_line() + theme_classic()

### Calculate mean nov - jan flow
hurunui_obs_annual <- hurunui_obs %>%
	mutate(month = month(date), year = year(date)) %>%
	filter(month == 11 | month == 12 | month == 1) %>%
	group_by(year) %>%
	summarise(obs_m3s = mean(obs_m3s, na.rm=TRUE), n_na=sum(is.na(obs_m3s))) %>%
	filter(n_na < 20)

### Combine obs with recon
annual_temp <- annual_temp %>%
	full_join(hurunui_obs_annual, by= "year") %>%
	select(col_name, year, obs_m3s, recon_m3s) %>%
	arrange(year)

### Doublecheck plot
plot(annual_temp$year, annual_temp$recon_m3s, type="l") #, ylim=c(0,2500)) 
lines(annual_temp$year, annual_temp$obs_m3s, col="red")

### Merge data
annual_ts <- rbind(annual_ts, annual_temp)

rm(annual_temp)

### Save to RDS file
saveRDS(annual_ts, file.path(write_output_path,"annual_ts.rds"))



###########################################################################
## Process Potomac
###########################################################################
annual_ts <- readRDS(file.path(write_output_path,"annual_ts.rds"))

### Read in Data
annual_temp <- read_table(file.path(annual_folder,"Potomac/potomac2011flow.txt"), skip=102)

### Create data
annual_temp <- annual_temp %>%
	mutate(col_name = "potomac2011") %>%
	rename("recon_m3s" = 'Recon MJJInst.') %>%
	rename("obs_m3s" = 'Mean MJJAS') %>%
	rename("year" = 'Year') %>%
	select(col_name, year, obs_m3s, recon_m3s)

### Merge data
annual_ts <- rbind(annual_ts, annual_temp)

rm(annual_temp)


### Save to RDS file
saveRDS(annual_ts, file.path(write_output_path,"annual_ts.rds"))


###########################################################################
## Process White River
###########################################################################
annual_ts <- readRDS(file.path(write_output_path,"annual_ts.rds"))

### Read in Data
annual_temp <- read_table(file.path(annual_folder,"White_river/whiteriv.rec.txt"), skip=49)

### Create data
annual_temp <- annual_temp %>%
	mutate(col_name = "white1989") %>%
	rename("recon_m3s" = 'COLUMN') %>%
	mutate("obs_m3s" = NA) %>%
	rename("year" = 'YEAR') %>%
	select(col_name, year, obs_m3s, recon_m3s)

### Convert km3/year to m3/s
annual_temp <- annual_temp %>%
	mutate(recon_m3s = (recon_m3s * 1E9)/time_sec)

### Merge data
annual_ts <- rbind(annual_ts, annual_temp)
rm(annual_temp)

### Read in Data
annual_temp <- read_table(file.path(annual_folder,"White_river/whiteriver_flow.txt"), skip=62, col_types = cols(
  year = col_double(),
  Col1 = col_double(),
  Col2 = col_double(),
  Col3 = col_double()
))

### Create data
annual_temp <- annual_temp %>%
	mutate(col_name = "white2000") %>%
	rename("recon_m3s" = 'Col1') %>%
	rename("obs_m3s" = 'Col3') %>%
	mutate(recon_m3s = recon_m3s * 1000) %>%
	mutate(obs_m3s = obs_m3s * 1000) %>%
	select(col_name, year, obs_m3s, recon_m3s)

### Merge data
annual_ts <- rbind(annual_ts, annual_temp)
rm(annual_temp)


### Save to RDS file
saveRDS(annual_ts, file.path(write_output_path,"annual_ts.rds"))

###########################################################################
## Process Yellowstone
###########################################################################
annual_ts <- readRDS(file.path(write_output_path,"annual_ts.rds"))

### Read in Data
annual_temp <- read_table2(file.path(annual_folder,"Yellowstone/yellowstone_flow.txt"), skip=102)

### Create data
annual_temp <- annual_temp %>%
	mutate(col_name = "yellowstone") %>%
	rename("recon_m3s" = 'C2') %>%
	rename("obs_m3s" = "C6") %>%
	rename("year" = 'Year') %>%
	mutate(recon_m3s = (recon_m3s * 1E9)/time_sec) %>%
	select(col_name, year, obs_m3s, recon_m3s)

### Merge data
annual_ts <- rbind(annual_ts, annual_temp)
rm(annual_temp)



### Save to RDS file
saveRDS(annual_ts, file.path(write_output_path,"annual_ts.rds"))

###########################################################################
## Process Missouri
###########################################################################
annual_ts <- readRDS(file.path(write_output_path,"annual_ts.rds"))

### Read in Data
annual_temp <- read_table2(file.path(annual_folder,"missouri2016flow.txt"), skip=163)

annual_temp <- annual_temp %>%
	rename("year"  = "YearAD/StationID") %>%
	gather("col_name","recon_m3s",-year) %>%
	mutate(col_name = paste0("0", col_name))

unique_colnames <- unique(annual_temp$col_name)

### limited to 10 sites, easier to just loop through
### This study is based on calendar year, rather than water year
for (i in seq(1,length(unique_colnames))){

obs_temp <- readNWISstat(siteNumbers=unique_colnames[i],
                   parameterCd=c("00060"),
                   statReportType="annual") ### If water year, add statYearType=water

if (i == 1) {
	obs_stats <- obs_temp
} else {
	obs_stats <- rbind(obs_stats,obs_temp)
}

}

obs_stats <- obs_stats %>%
	select(site_no, year_nu, mean_va) %>%
	rename("year" = "year_nu") %>%
	mutate(col_name = paste0("usgs_", site_no)) %>%
	mutate(obs_m3s = mean_va * ft3_to_m3 ) %>%
	select(-mean_va) %>%
	as_tibble()

annual_temp <- annual_temp %>%
	mutate(col_name = paste0("usgs_", col_name)) %>%
	full_join(obs_stats, by=c("col_name", "year")) %>%
	arrange(col_name, year)

### Merge data
annual_ts <- rbind(annual_ts, annual_temp)
rm(annual_temp)

### Save to RDS file
saveRDS(annual_ts, file.path(write_output_path,"annual_ts.rds"))


#### LOOKS LIKE THERE ARE SOME STRANGE PEAKS IN THE MISSOURI RECONSTRUCTION DATA
#### DOUBLE CHECK THIS


###########################################################################
## Process Snake River
###########################################################################
annual_ts <- readRDS(file.path(write_output_path,"annual_ts.rds"))

### Read in Data
annual_temp <- read_table2(file.path(annual_folder,"snake-flow2010.txt"), skip=96)

recon_temp <- annual_temp %>%
	rename("year" = 'Year') %>%
	select(year, JCK_recon, PALI_recon, HEII_recon) %>%
	rename("snake_jck" = 'JCK_recon', "snake_pali" = 'PALI_recon', "snake_heii" = 'HEII_recon')

recon_temp <- recon_temp %>%
	gather("col_name", "recon_acft", -year)

obs_temp <- annual_temp %>%
	rename("year" = 'Year') %>%
	select(year, JCK_inst, PALI_inst, HEII_inst) %>%
	rename("snake_jck" = 'JCK_inst', "snake_pali" = 'PALI_inst', "snake_heii" = 'HEII_inst')

obs_temp <- obs_temp %>%
	gather("col_name", "obs_acft", -year)

annual_temp <- recon_temp %>%
	full_join(obs_temp, by=c("year", "col_name")) %>%
	arrange(col_name, year) %>%
	mutate( obs_m3s = (obs_acft * m3_acft)/time_sec) %>%
	mutate( recon_m3s = (recon_acft * m3_acft)/time_sec) %>%
	select(col_name, year, obs_m3s, recon_m3s)

### Merge data
annual_ts <- rbind(annual_ts, annual_temp)
rm(annual_temp)

### Save to RDS file
saveRDS(annual_ts, file.path(write_output_path,"annual_ts.rds"))




















###########################################################################
## Double Check plot
###########################################################################

plot_df <- annual_ts %>%
	gather("var", "flow_m3s", -year, -col_name) 

ggplot(plot_df , aes(x=year, y=flow_m3s, colour=var)) + geom_line() + theme_classic() + facet_grid(rows=vars(col_name), scales="free_y")


ggplot(plot_df , aes(x=year, y=flow_m3s, colour=var)) + geom_line() + theme_classic() + facet_wrap(. ~ col_name, scales="free_y")


plot_df <- annual_temp %>%
	gather("var", "flow_m3s", -year, -col_name) 



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





