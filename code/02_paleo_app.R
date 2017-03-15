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
### Path for Data and Output	
data_path <- "../../../data"
output_path <- "../../../output"
global_path <- "../../global_func"
function_path <- "./functions"

### Set global output location
output_path_base <- file.path(output_path,"paleo_flow_shiny")

write_output_path <- file.path(output_path_base,"monthly_paleo")

### Create output folders
dir.create(output_path_base)
dir.create(write_output_path)

###########################################################################
###  Load functions
###########################################################################
### Load these functions for all code
require(colorout)
require(assertthat)
require(reshape2)

### Load these functions for this unique project
require(ggplot2)
require(googleVis)
require(shiny)
library(dygraphs)
library(datasets)
require(shinythemes)

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
site_id_list <- c("10109001", "10011500")
site_name_list <- c("Logan River", "Bear River near Utah-Wyo")
recons_file_name_list <- c("logan2013flow.txt", "bear2015flow.txt")

first_month_wy <- 10 ### Water Year starts on Oct 1
param_cd <- "00060"



###########################################################################
## Read in data from Processing step
###########################################################################
### Read in site id
n <- 1
site_id_n <- site_id_list[[n]]

### Read in flow data
flow_ts <- read.csv(file.path(write_output_path,paste0("flow_",site_id_n,".csv")))

### Read in first month 
start_ts <- read.csv(file.path(write_output_path,paste0("start_",site_id_n,".csv")))
start_ts <- as.numeric(unlist(start_ts))

### Create a monthly time series
flow_ts <- ts(flow_ts, start=start_ts, frequency=12)



#####
max_y <- max(c(flow_ts), na.rm=TRUE)


###########################
###  Create Shiny App
###########################

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("Reconstructed Streamflow Explorer"),
  
  sidebarLayout(
    sidebarPanel(
    
   ### Input for time resolution
 	selectizeInput(
        'time_resolution', 'Resolution', choices = c(`Monthly` = 'monthly'),multiple = FALSE),  
      
  ### Input for site location
 	selectizeInput(
        'site_name', 'Site Location', choices = list(
      'Utah, USA' = c(`Logan River` = '10109001', `Bear River near Utah-Wyo` = '10011500'),
      Other = c(`...` = 'NA')
    ),
    multiple = FALSE,
        options = list(
          placeholder = 'Please select a site below',
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),     

   ### Input for subset
 	selectizeInput(
        'time_subset', 'Date Subset', choices = c(`Full` = 'full', `Jan (1)` = '1', `Feb (2)` = '2'),multiple = FALSE),  
      
          
      tags$div(class="header", checked=NA,
               tags$p("Reconstructions based on Stagge et al. (2017)."),
               tags$p("Data is based on USGS gauges at the ", tags$a(href="https://waterdata.usgs.gov/usa/nwis/uv?site_no=10109000", "Logan River"), " and the ", tags$a(href="https://waterdata.usgs.gov/usa/nwis/uv?site_no=10011500", "Bear River"))
	  ), 
      br(),
      helpText("Click and drag to zoom in (double click to zoom back out).")
    ),
    mainPanel(
    tabsetPanel(
        tabPanel("Time Series", dygraphOutput("tsPlot")),
        tabPanel("Extremes",  verbatimTextOutput('site_out')), 
        tabPanel("Goodness of Fit", tags$p("Place Holder"))
      )
    )
  )
)

server <- function(input, output) {

### Set initial values
site_id_list <- c("10109001", "10011500")
site_name_list <- c("Logan River", "Bear River near Utah-Wyo")

### Read in data
paleo_list <- read_in_paleo(site_id_list=site_id_list, site_name_list=site_name_list)


### Determine which one of the read in time series from the number
list_id <- reactive({ 
	list_id_temp <- which(input$site_name == site_id_list)
	if (length(list_id_temp) == 1) {
		list_id_temp
	} else {
		"None"
	}
})

### Extract Site Name
site_name <- reactive({
	if (list_id()=="None"){
		"Please Select Site"
	} else {
		paleo_list[[list_id()]]$site_name
	}
})
### Extract time series
paleo_ts <- reactive({
	if (list_id()=="None"){
		paleo_ts_temp <- data.frame(Observed=rep(NA,300), Annual_Recon=rep(NA,300), Monthly_Recon=rep(NA,300))
		ts(as.matrix(paleo_ts_temp), start=c(1700,1), frequency=12)
	} else {
		paleo_list[[list_id()]]$flow_ts
	}
})

### Test to print the Site ID Number  
   output$site_out <- renderPrint({
    input$site_name
    
  })


### Time Series plot
  output$tsPlot <- renderDygraph({
    dygraph(paleo_ts(), main = site_name()) %>%
    dyRangeSelector(dateWindow = c("1650-01-01", "1995-01-01")) %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE) %>%
    dyLegend(show = "always", hideOnMouseOut = FALSE, labelsSeparateLines=TRUE) %>%
    dyAxis("y", label = "Monthly Mean Discharge (m3/s)", valueRange=c(0, max_y)) %>%
    dySeries("Observed", color="#e41a1c")  %>%
    dySeries("Monthly_Recon", color="#404040", strokeWidth = 1.5) %>%
    dySeries("Annual_Recon", color="#377eb8", strokeWidth = 2, strokePattern = "dashed")
  })
  
}

shinyApp(ui = ui, server = server)


