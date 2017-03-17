function(input, output, session) {

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
## Read in site information
###########################################################################
site_monthly <- read.table(file.path(data_path, "sites_monthly.txt"), sep="\t", header=TRUE,colClasses=rep("character", 11))
site_annual <- read.table(file.path(data_path, "sites_annual.txt"), sep="\t", header=TRUE,colClasses=rep("character", 11))

### Sort by group and then name
site_monthly <- site_monthly[order(site_monthly$site_group, site_monthly$site_name),] 
site_annual <- site_annual[order(site_annual$site_group, site_annual$site_name),] 

###########################################################################
## Read in data in background (so it doesn't have to be read every time)
###########################################################################
### Set initial values
#site_id_list <- c("10109001", "10011500")
#site_name_list <- c("Logan River", "Bear River near Utah-Wyo")

### Read in monthly and annual
monthly_paleo_list <- read_in_paleo(site_id_list=site_monthly$site_id, site_name_list=site_monthly$site_name, path=file.path(data_path, "monthly"))
annual_paleo_list <- read_in_paleo(site_id_list=site_annual$site_id, site_name_list=site_annual$site_name, path=file.path(data_path, "annual"))

### Read in data
paleo_list <- read_in_paleo(site_id_list=site_id_list, site_name_list=site_name_list, path=file.path(data_path, "monthly"))


monthly_paleo_list <- read_in_paleo(site_id_list=site_monthly$site_id, site_name_list=site_monthly$site_name, path=file.path(data_path, "monthly"))


###########################################################################
## Dynamic Input for Sites
###########################################################################
 observe({
    x <- input$time_resolution

    if (x == "annual"){
		choice_list <- 
    # Can also set the label and select items
    updateSelectizeInput(session, "site_name", "Site Location",
      	choices = create_site_list(site_annual)
		)
    }
    
    if (x == "monthly"){
    # Can also set the label and select items
    updateSelectizeInput(session, "time_subset", "Date Subset",
      	choices = create_site_list(site_monthly)
		)
    }
  })



###########################################################################
## Dynamic Input for Date Subset
###########################################################################
 observe({
    x <- input$time_resolution

    if (x == "annual"){
    # Can also set the label and select items
    updateSelectInput(session, "time_subset", "Date Subset",
      	choices = c(`Annual` = '0')
		)
    }
    
    if (x == "monthly"){
    # Can also set the label and select items
    updateSelectizeInput(session, "time_subset", "Date Subset",
      	choices = c(`Full Timeseries` = '0', `January` = '1', `February` = '2', `March` = '3', `April` = '4', `May` = '5', `June` = '6', `July` = '7', `August` = '8', `September` = '9', `October` = '10', `November` = '11', `December` = '12' )
		)
    }
  })




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
## Extract the subset information and flow units
###########################################################################
subset_input <- reactive({ as.numeric(input$time_subset) })

### Extract the flow units
flow_units <- reactive({ input$flow_units })

		
###########################################################################
## Process the time series, keep date columns for later
###########################################################################
### Include a catch for before you select a site (blank plot)
paleo_ts_temp <- reactive({
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

	paleo_ts_temp
	}
})

###########################################################################
## Extract dates from time series
###########################################################################
paleo_ts_dates <- reactive({
	paste0("", paleo_ts_temp()$Month, " / ", paleo_ts_temp()$Year, "")
})

###########################################################################
## Process the time series for plotting, remove date columns and convert units
###########################################################################
paleo_ts <- reactive({
		### Remove the monthly and annual columns before plotting
		paleo_ts_temp <- paleo_ts_temp()[ ,!(colnames(paleo_ts_temp()) %in% c("Month", "Year"))]
		
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
		### Return time series for plot
	paleo_ts_temp
})

###########################################################################
## Calculate maximum flow for plotting range
###########################################################################
y_lims <- reactive({ 
	max_y <- max(c(paleo_ts()), na.rm=TRUE)
	max_y <- 1.1*max_y
	c(0,max_y) 
	})


###########################################################################
## Produce a dataframe holding observed and reconstructed values
###########################################################################

gof_df_temp <- reactive({
	### Test if there are values
  	if(max(paleo_ts()$Observed, na.rm=TRUE) > 0) {
  		### Create dataframe
  		gof_df <- data.frame(paleo_ts())
  		gof_df <- data.frame(Observed=gof_df$Observed, Reconstructed=gof_df$Monthly_Rec, Month=paleo_ts_temp()$Month, Year=paleo_ts_temp()$Year)
	
  	  	### Cut to common reference period
  	  	refer_test <- complete.cases(gof_df[,c("Observed","Reconstructed")])
 	  	gof_df <- gof_df[refer_test,]

  		### Add a column for tooltips
  		### Had to add an extra span because the results kept wrapping around
 		paleo_dates <- paste0("", gof_df$Month, " / ", gof_df$Year, "")
 		gof_df$Reconstructed.tooltip <- paste0("<b>", paleo_dates,"</b><br>Observed: ", signif(gof_df$Observed,3),"<br>Reconstructed: ", signif(gof_df$Reconstructed,2), " <span style='display:inline-block; width: 5;'></span>" )
 	  	gof_df
 	  	}		
	})	
	

gof_df <- reactive({
	### Test if there are values
  	if(max(paleo_ts()$Observed, na.rm=TRUE) > 0) {
  	  	gof_df <- gof_df_temp()  
  	  	gof_df <- gof_df[,c("Observed", "Reconstructed", "Reconstructed.tooltip")]		
  		### Add a blank column with two enpoints to produce the 1:1 line
  	  	gof_df$abline <- NA
  	  	gof_df <- rbind(gof_df, data.frame(Observed=y_lims(), Reconstructed=c(NA, NA), Reconstructed.tooltip=c("", ""), abline=y_lims()))
  	  	  	  	  	  	
  	  	### Assign to variable
  	  	gof_df
  	  	}		
	})	
	
###########################################################################
## Produce a dataframe for density
###########################################################################
	
gof_distr_df <- reactive({
	### Melt the dataframe
 		gof_distr_df <- data.frame(Flow = gof_df()$Observed, Data="Observed")
 		gof_distr_df <- rbind(gof_distr_df, data.frame(Flow = gof_df()$Reconstructed, Data="Reconstructed"))
 		gof_distr_df
	})	
	
	
	
###########################################################################
## Produce a dataframe for gof table
###########################################################################
	
gof_table_df <- reactive({
	### Calculate goodness of fit statistics for entire time series
 		gof_table_df <- gof_ts(gof_df_temp()$Reconstructed, gof_df_temp()$Observed)
 		
 	### Calculate goodness of fit statistics for each month
 	for (j in seq(1,12)) {
 		month_subset <- subset(gof_df_temp(), Month==j)
 		gof_table_df <- rbind(gof_table_df, gof_ts(month_subset$Reconstructed, month_subset$Observed))
 	}

# 	gof_table_df <- as.numeric(gof_table_df)
 	gof_table_df <- apply(gof_table_df,2, as.numeric)
 	
 	month_names <- format(ISOdatetime(2000,1:12,1,0,0,0),"%b")
# 	as.character(c("Full", seq(1:12)))
 	gof_table_df <- data.frame(Period=c("Full", seq(1:12)), gof_table_df)
 	
 	gof_table_df <- signif_df(gof_table_df,3)
 	
 		gof_table_df
	})	

###########################################################################
## Output to extremes tab
########################################################################### 
   output$site_out <- renderPrint({
   site_monthly
    
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
## Output to goodness of fit (Obs vs Reconstr) plot
###########################################################################   
 output$gof_scatter <- renderGvis({
	gvisComboChart(gof_df(), xvar = "Observed", yvar = c("Reconstructed", "Reconstructed.tooltip", "abline"),
          options=list(seriesType='scatter',
 			title="Observed vs. Reconstructed Flow",
 			series='{1: {type:\"line\"}}',
            explorer="{actions: ['dragToZoom' , 'rightClickToReset'], maxZoomIn:0.05}",
            legend="none",
            tooltip="{isHtml:'True'}",                                                                 
            vAxis=paste0("{title:'Reconstructed Flow (", flow_units(), ")'}"),                        
            hAxis=paste0("{title:'Observed Flow (", flow_units(), ")'}"),                    
            width='100%', height=500,
            chartArea= "{'width': '80%', 'height': '80%'}"),
            )                   
  })
     

###########################################################################
## Output to distribution (Obs vs Reconstr) plot
###########################################################################   
 output$gof_distr <-renderPlot({
 		
 		### Create the plot
  		p <- ggplot(gof_distr_df(), aes(x=Flow, fill=Data))
  		p <- p + geom_density(alpha=0.3)
  		p <- p + scale_fill_brewer(name="Data Source", palette="Dark2")
  		p <- p + scale_x_continuous(name=paste0("Streamflow (",flow_units(),")"), expand=c(0,0))
  		p <- p + scale_y_continuous(name="Density")
  		p <- p + theme_light()
  		p
   })	



###########################################################################
## Output gof to a table
###########################################################################   
#output$gof_table <-renderTable({
#    gof_table_df()
#  })

  output$gof_table <- DT::renderDataTable({
   DT::datatable(gof_table_df(), plugins='natural', rownames=FALSE, 
    	options = list(pageLength = 13, dom='t', columnDefs = list(list(type = 'natural', targets = 0)) ))
  })

  
}
    