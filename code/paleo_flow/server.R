function(input, output) {

###########################################################################
## Set the Paths
###########################################################################
### Path for Data and Output	
data_path <- "./data"
function_path <- "./functions"

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
require(lubridate)

### Load project specific functions
file.sources = list.files(function_path, pattern="*.R", recursive=TRUE)
sapply(file.path(function_path, file.sources),source)



###########################################################################
## Initial Values
###########################################################################
### Set initial values
site_id_list <- c("10109001", "10011500")
site_name_list <- c("Logan River", "Bear River near Utah-Wyo")

### Read in data
paleo_list <- read_in_paleo(site_id_list=site_id_list, site_name_list=site_name_list, path=file.path(data_path, "monthly"))

###########################################################################
## Determine the site
###########################################################################
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

###########################################################################
## Process time series
###########################################################################

### Extract the subset information
subset_input <- reactive({ as.numeric(input$time_subset) })

### Extract the flow units
flow_units <- reactive({ input$flow_units })

		
### Extract time series
paleo_ts <- reactive({
	if (list_id()=="None"){
		### Generate a blank graph
		paleo_ts_temp <- data.frame(Observed=rep(NA,300), Annual_Recon=rep(NA,300), Monthly_Recon=rep(NA,300))
		ts(as.matrix(paleo_ts_temp), start=c(1700,1), frequency=12)
	} else {
		### Read time series from list 
		paleo_ts_temp <- paleo_list[[list_id()]]$flow_ts
		
		### Create date vector and apply to time series
		date_vec <- as.POSIXct(paste0(paleo_ts_temp$Year,"/",paleo_ts_temp$Month, "/15"), format="%Y/%m/%d")
		paleo_ts_temp <- xts(paleo_ts_temp, date_vec)
		
		### If it is not a full time series, subset it and NA out the annual timeseries
		if (subset_input() > 0) {
			paleo_ts_temp <- subset(paleo_ts_temp, Month==subset_input())
			paleo_ts_temp$Annual_Recon <- NA
		}

		### Convert Units
		if (flow_units() == "cfs"){
		paleo_ts_temp <- paleo_ts_temp * 35.31467
		}
		if (flow_units() == "ac-ft"){
		year_month <- as.Date(paste0(paleo_ts_temp$Year,"-", paleo_ts_temp$Month,"-15"))
		## Convert to cfs
		paleo_ts_temp <- paleo_ts_temp * 35.31467
		### Convert to ac-ft per second
		paleo_ts_temp <- paleo_ts_temp * (1/43560)
		### Convert to ac-ft per month
		paleo_ts_temp <- paleo_ts_temp * 60*60*24*days_in_month(year_month)
		}
		
		### Remove the monthly and annual columns before plotting
		paleo_ts_temp <- paleo_ts_temp[ ,!(colnames(paleo_ts_temp) %in% c("Month", "Year"))]

		
		### Return time series for plot
	paleo_ts_temp

	}
})


### Calculate the maximum flow
y_lims <- reactive({ 
	max_y <- max(c(paleo_ts()), na.rm=TRUE)
	max_y <- 1.1*ceiling(max_y)
	c(0,max_y) 
	})



###########################################################################
## Output to extremes tab
########################################################################### 
   output$site_out <- renderPrint({
    y_lims()
    
  })


###########################################################################
## Output to time series plot
########################################################################### 
  output$tsPlot <- 
    renderDygraph({
    dygraph(paleo_ts(), main = site_name()) %>%
    dyRangeSelector(dateWindow = c("1850-01-01", "1995-01-01")) %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE, titleHeight= 28) %>%
    dyLegend(show = "always", hideOnMouseOut = FALSE, labelsSeparateLines=TRUE) %>%
    dyAxis("y", label = paste0("Monthly Mean Discharge (",flow_units(),")"), valueRange=y_lims()) %>%
    dySeries("Observed", color="#e41a1c")  %>%
    dySeries("Monthly_Recon", color="#404040", strokeWidth = 1.5) %>% 
    dySeries("Annual_Recon", color="#377eb8", strokeWidth = 2, strokePattern = "dashed")
  	})    
  
  
  
  
###########################################################################
## Output to goodness of fit plot
###########################################################################   

  output$DistribPlot <- renderPlot({
  		selected_data <- data.frame(paleo_ts())

  		p <- ggplot(selected_data, aes(x=Observed, y=Monthly_Recon))
  		p <- p + geom_hline(yintercept = 0, colour="black")
  		p <- p + geom_vline(xintercept = 0, colour="black")
  		p <- p + geom_abline(intercept = 0, slope = 1, colour="red")
  		p <- p + geom_point()
  		p <- p + theme_light()
  		p <- p + scale_x_continuous(name="Observed Flow (m3/s)")
  		p <- p + scale_y_continuous(name="Reconstructed Flow (m3/s)")
 		p <- p + coord_equal(ratio=1)
  		p
   })	
     
  
  
  
  
}
    