# *------------------------------------------------------------------
# | PROGRAM NAME: 00-prepare_file_system
# | FILE NAME: 00-prepare_file_system.R
# | DATE: 
# | CREATED BY:  Jim Stagge         
# *----------------------------------------------------------------
# | PURPOSE:  This code installs all required packages to run the Paleoflow app.
# | 
# | 
# *------------------------------------------------------------------



###########################################################################
###  Check for necessary packages and install if needed
###########################################################################
### Set a list of packages
list_of_packages <- c("datasets", "lubridate", "tidyverse", "ggplot2", "dataRetrieval", "devtools", "readxl", "rsconnect", "shiny", "googleVis", "dygraphs", "shinythemes", "xts", "dplyr", "leaflet", "rJava", "mailR", "timeDate", "shinyjs", "shinyBS", "MASS", "scales", "png", "digest", "tsbox", "shinycssloaders", "DT", "keyringr")

	
### Determine which packages are missing
package_list <- installed.packages()[,"Package"]
installed_test <- (list_of_packages %in% package_list)
packages_needed <- list_of_packages[!installed_test]

### If packages are missing, install them
if(length(packages_needed)) install.packages(packages_needed)

### A few packages have issues in the CRAN repository, must be installed from github
require(devtools)

devtools::install_github("jcheng5/googleCharts")
devtools::install_github("jstagge/staggefuncs")
