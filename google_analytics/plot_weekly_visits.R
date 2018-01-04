# *------------------------------------------------------------------
# | PROGRAM NAME: plot_weekly_visits
# | FILE NAME: plot_weekly_visits.R
# | DATE: 11/09/2017
# | CREATED BY:  James Stagge         
# *----------------------------------------------------------------
# | PURPOSE:     
# |				 
# *------------------------------------------------------------------
# | DATA USED:               
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

data_path <- file.path(data_path, "paleo_flow_shiny_analytics")

### Create output folders
write_output_base_path <- file.path(output_path, "paleo_flow_shiny_analytics")
write_figures_base_path <- write_output_base_path

### Create output folders
dir.create(write_output_base_path)
dir.create(write_figures_base_path)

###########################################################################
###  Load functions
###########################################################################
### Load these functions for all code
require(colorout)
require(assertthat)
require(testthat)

### Load these functions for this unique project
#require(monthlypaleo)
require(staggefuncs)


################################################
### Read google analytics data
#################################################

weekly_visits <- read.csv(file.path(data_path, "weekly_visits.csv"))

weekly_visits$Date <- as.Date(weekly_visits$Date, format="%m/%d/%y")




################################################
### Sessions Plot
#################################################
p <- ggplot(weekly_visits, aes(x=Date, y=Sessions))
p <- p + geom_line()
p <- p + theme_classic_new()
p <- p + scale_x_date (date_breaks = "1 month", date_labels = "%m-%Y")
p

### Save figure
file_name <- "paleoflow_sessions_weekly"

ggsave(paste0(file.path(write_figures_base_path, file_name), ".png"), p, width=5, height=3, dpi=600)
ggsave(paste0(file.path(write_figures_base_path, file_name), ".svg"), p, width=5, height=3)
ggsave(paste0(file.path(write_figures_base_path, file_name), ".pdf"), p, width=5, height=3)


################################################
### Users Plot
#################################################
p <- ggplot(weekly_visits, aes(x=Date, y=Users))
p <- p + geom_line()
p <- p + theme_classic_new()
p <- p + scale_x_date (date_breaks = "1 month", date_labels = "%m-%Y")
p

### Save figure
file_name <- "paleoflow_users_weekly"

ggsave(paste0(file.path(write_figures_base_path, file_name), ".png"), p, width=5, height=3, dpi=600)
ggsave(paste0(file.path(write_figures_base_path, file_name), ".svg"), p, width=5, height=3)
ggsave(paste0(file.path(write_figures_base_path, file_name), ".pdf"), p, width=5, height=3)


################################################
### Pageviews Plot
#################################################
p <- ggplot(weekly_visits, aes(x=Date, y=Pageviews))
p <- p + geom_line()
p <- p + theme_classic_new()
p <- p + scale_x_date (date_breaks = "1 month", date_labels = "%m-%Y")
p

### Save figure
file_name <- "paleoflow_pageviews_weekly"

ggsave(paste0(file.path(write_figures_base_path, file_name), ".png"), p, width=5, height=3, dpi=600)
ggsave(paste0(file.path(write_figures_base_path, file_name), ".svg"), p, width=5, height=3)
ggsave(paste0(file.path(write_figures_base_path, file_name), ".pdf"), p, width=5, height=3)




The user should X, Y, Z (each a phrase starting with a verb) to do W. The first sentence (line 60) follows this pattern, but later the text wanders. Avoid "can" "may", etc. Instead present a more definitive set of directions the reader should follow (as they read) to explore all the tool's features (that you wish to discuss in the manuscript).






