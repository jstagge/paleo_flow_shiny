# *------------------------------------------------------------------
# | PROGRAM NAME: 02-paleo_app
# | FILE NAME: 02-paleo_app.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  This code creates the paleostreamflow app and uploads to the
# |				Shinyapps website.
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

###########################################################################
###  Load functions
###########################################################################
### Load these functions for all code
require(colorout)
require(assertthat)
require(reshape2)


#devtools::install_github("jcheng5/googleCharts")

### Load these functions for this unique project
require(shiny)
require(rsconnect)
require(googleCharts)
require(ggplot2)
require(googleVis)
require(shiny)
library(dygraphs)
library(datasets)
require(shinythemes)
require(lubridate)
require(xts)

###########################################################################
## Set Initial Values
###########################################################################



###########################################################################
## Run the app on personal computer in diagnostic mode
###########################################################################
runApp("paleo_flow", display.mode = "showcase")

runApp("paleo_flow")



###########################################################################
##  Publish to Shiny website
###########################################################################
rsconnect::deployApp("paleo_flow")





