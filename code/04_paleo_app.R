# *------------------------------------------------------------------
# | PROGRAM NAME: 04-paleo_app
# | FILE NAME: 04-paleo_app.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  This code runs the paleoflow website locally to test the website.
# |
# *------------------------------------------------------------------
 
###########################################################################
###  Load functions
###########################################################################
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
## Run the app locally to test
###########################################################################
### Run the website locally
runApp("paleo_flow")

### Adds a side panel for diagnostics
#runApp("paleo_flow", display.mode = "showcase")

