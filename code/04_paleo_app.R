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
 
 
###########################################################################
## Set the Paths
###########################################################################

###########################################################################
###  Load functions
###########################################################################
### Load these functions for all code
#require(colorout)
require(assertthat)
require(reshape2)

### Needed for my laptop
#require(later)

#devtools::install_github("jcheng5/googleCharts")

### Load these functions for this unique project
require(rsconnect)
require(googleCharts)
require(ggplot2)
require(googleVis)
library(dygraphs)
library(datasets)
require(shinythemes)
require(lubridate)
require(xts)
require(dplyr)
require(shiny)
require(leaflet)
library(rJava)
library(mailR)
library(timeDate)
require(shinyjs)
require(shinyBS)

### For ubuntu
#sudo apt-get install -y default-jre
#sudo apt-get install -y default-jdk
#sudo R CMD javareconf
#install.packages("rJava")

### Had to install github version of googleCharts

### For arch linux
#https://biostatsr.blogspot.com/2017/09/install-rjava-on-archlinux.html


###########################################################################
## Set Initial Values
###########################################################################



###########################################################################
## Run the app on personal computer in diagnostic mode
###########################################################################
#runApp("paleo_flow", display.mode = "showcase")

runApp("paleo_flow")

##runApp("my_example5")


###########################################################################
##  Publish to Shiny website/
###########################################################################
##rsconnect::deployApp("paleo_flow")





