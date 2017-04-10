function(input, output, session) {

###########################################################################
## Dynamic Input for Date Subset and Sites
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
    
 	choice_list <- 
    # Can also set the label and select items
    updateSelectizeInput(session, "site_name", "Site Location",
      	choices = create_site_list(site_all, res=x)
		)   
  })


###########################################################################
## Extract the subset information
###########################################################################
subset_input <- reactive({ as.numeric(input$time_subset) })

### Extract time resolution
time_resolution <- reactive({input$time_resolution})

###########################################################################
## Determine site information
###########################################################################
### Determine which one of the read in time series from the number
list_id <- reactive({ 
	if (input$site_name == "") {
		"None"
	} else {
		input$site_name
	}
})

### Extract Site Info
site_info <- reactive({
	if (list_id()=="None"){
		blank_site <- site_all[1,]
		blank_site[1,seq(1,length(blank_site))] <- NA
		blank_site$site_name <- "Please Select Site"
		blank_site
	} else {
		site_all[list_id(),]
	}
})

### Extract Site Name
site_name <- reactive({ site_info()$site_name })

###########################################################################
## Extract citation information
###########################################################################
### For reconstruction
output$recon_name_text <- renderUI({ HTML(paste0("<strong>Name</strong> :   ",site_info()$recon_name)) })

output$recon_author_text <- renderUI({ HTML(paste0("<strong>Author/Originator(s)</strong> :   ",site_info()$recon_originator)) })

output$recon_link_text <- renderUI({ HTML(paste0('<strong>Source Link </strong> :   <a href="',site_info()$recon_link, '">',site_info()$recon_link,'</a>')) })

output$recon_citation_text <- renderText({site_info()$recon_citation})

### For base data
output$base_name_text <- renderUI({ HTML(paste0("<strong>Name</strong> :   ",site_info()$base_name)) })

output$base_author_text <- renderUI({ HTML(paste0("<strong>Author/Originator(s)</strong> :   ",site_info()$base_originator)) })

output$base_link_text <- renderUI({ HTML(paste0('<strong>Source Link </strong> :   <a href="',site_info()$base_link, '">',site_info()$base_link,'</a>')) })



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
		paleo_ts_location <- file.path(data_path, site_info()$resolution)
		paleo_ts_location <- file.path(paleo_ts_location, paste0("flow_",site_info()$site_id,".csv"))
		paleo_ts_temp <- read.csv(paleo_ts_location) 
		
		### Create date vector and apply to time series
		date_vec <- as.POSIXct(paste0(paleo_ts_temp$Year,"/",paleo_ts_temp$Month, "/15"), format="%Y/%m/%d")
		paleo_ts_temp <- xts(paleo_ts_temp, date_vec)

		### Return temp
		paleo_ts_temp
	}
})
		
###########################################################################
## Extract dates from time series
###########################################################################
paleo_ts_dates <- reactive({
	paste0("", paleo_ts_temp()$Month, " / ", paleo_ts_temp()$Year, "")
})

year_month <- reactive({
		year_month <- as.Date(paste0(as.numeric(paleo_ts_temp()$Year),"-", as.numeric(paleo_ts_temp()$Month),"-15"))
})

###########################################################################
## Extract Unit Conversion
###########################################################################
### Extract the flow units
flow_units <- reactive({ input$flow_units })

### Calculate flow scaling factor
unit_conversion <- reactive({
	### If m3/s
	unit_conv <- 1
	### If cfs
		if (flow_units() == "cfs"){
			unit_conv <- 35.31467
		}
	### If acre-feet
		if (flow_units() == "ac-ft"){
			## Convert to cfs
			unit_conv <- 35.31467
			### Convert to ac-ft per second
			unit_conv <- unit_conv * (1/43560)
			### Convert to ac-ft per month
			unit_conv <- unit_conv * 60*60*24*days_in_month(year_month())
		}
	unit_conv
})

###########################################################################
## Create full (no subset) time series with correct units
###########################################################################
### Columns that aren't month and year - to scale and ultimately plot
cols_to_scale <- reactive({ !(names(paleo_ts_temp()) %in% c("Month", "Year")) })

paleo_ts_full <- reactive({
	### Scale columns for units
	paleo_ts_full <- paleo_ts_temp()
	paleo_ts_full[,cols_to_scale()] <- paleo_ts_full[,cols_to_scale()]*unit_conversion()
	paleo_ts_full
})

paleo_ts_subset <- reactive({
	### If it is not a full time series, subset it and NA out the annual timeseries
	if (subset_input() > 0) {
		paleo_ts_subset <- subset(paleo_ts_full(), Month==subset_input())
		paleo_ts_subset$Annual_Recon <- NA
		paleo_ts_subset
	### Otherwise return the full time series
	} else {
		paleo_ts_full()
	}
})

###########################################################################
## Process the time series for plotting, remove date columns and convert units
###########################################################################
paleo_ts_plot <- reactive({
	if (list_id()=="None"){
		### Generate a blank graph
		paleo_ts_temp <- data.frame(Observed=rep(NA,300), Annual_Recon=rep(NA,300), Monthly_Recon=rep(NA,300))
		paleo_ts_plot <- ts(as.matrix(paleo_ts_temp), start=c(1700,1), frequency=12)
	} else {
		### Remove the monthly and annual columns before plotting
		paleo_ts_plot <- paleo_ts_subset()[,cols_to_scale()]
	}
	### Return time series for plot
	paleo_ts_plot
})

###########################################################################
## Calculate maximum flow for plotting range
###########################################################################
y_lims <- reactive({ 
	max_y <- max(c(paleo_ts_plot()), na.rm=TRUE)
	max_y <- 1.1*max_y
	c(0,max_y) 
	})


###########################################################################
## Produce a dataframe holding observed and reconstructed values
###########################################################################
gof_df_temp <- reactive({
	### Test if there are values
  	if(max(paleo_ts_subset()$Observed, na.rm=TRUE) > 0) {
  		### Create dataframe
  		gof_df <- data.frame(paleo_ts_subset())
  		gof_df <- data.frame(Observed=gof_df$Observed, Reconstructed=gof_df$Monthly_Rec, Month=paleo_ts_subset()$Month, Year=paleo_ts_subset()$Year)
	
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
  	if(max(paleo_ts_subset()$Observed, na.rm=TRUE) > 0) {
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
 	
 	### If Date subset is something other than the full period
 	### Calculate goodness of fit statistics for that month also
 	if (subset_input() > 0) {
 		### Determine month name and create dataframe
 		period_name <- format(ISOdatetime(2000,subset_input(),1,0,0,0),"%b")
	} else {
		period_name <- "Full"
	}
 		### Make all columns numeric
 		gof_table_df <- data.frame(gof_table_df)
 		gof_table_df <- signif_df(gof_table_df,3)
 		### Create Data frame
 		gof_table_df <- data.frame(Period=period_name, gof_table_df)
 		### Cut the extraneous columns out
 		gof_table_df <- gof_table_df[ , !(names(gof_table_df) %in% c("MAE", "R.spear"))]
 		### Rename Columns
 		names(gof_table_df) <- c("Period", "Mean Error", "Root Mean Sq Error (RMSE)", "Nash-Sutcliffe Eff", "Pearson Corr (R)")
 
 	gof_table_df
	})	



###########################################################################
## Create the extremes input
###########################################################################
### Suggest the maximum flow in subset
min_suggest <- reactive({ floor(min(paleo_ts_subset()$Monthly_Recon, na.rm=TRUE)) })
max_suggest <- reactive({ ceiling(max(paleo_ts_subset()$Monthly_Recon, na.rm=TRUE)) })

### Suggest an extreme flow based on the 15th or 85 percentile (depending if gt or less than)
suggest_extreme <- reactive({ if (input$extreme_direction == "gt") {
			quantile(paleo_ts_subset()$Monthly_Recon, 0.85, na.rm=TRUE) 
		} else {
			quantile(paleo_ts_subset()$Monthly_Recon, 0.15, na.rm=TRUE) 
		}
	})
	
### Suggest the size of steps	
suggest_steps <- reactive({ signif(max_suggest()/200,1) })	

### Create the extreme threshold input	
output$extreme_flow <- renderUI({
sliderInput("extreme_flow", label = "Extreme flow", min = min_suggest(), 
        max = max_suggest(), value = suggest_extreme(), step = suggest_steps())
})        


###########################################################################
## Create the periods input
###########################################################################
current_year <-  as.integer(format(Sys.Date(), "%Y"))

first_year <- reactive({ min(paleo_ts_subset()$Year, na.rm=TRUE) })
last_year <- reactive({ max(paleo_ts_subset()$Year, na.rm=TRUE) })

rec_slider_2_start <- reactive({ceiling(first_year()/100)*100} )

### Create the Period 1 input slider
output$period_slider_1 <- renderUI({
sliderInput("period_slider_1", label = "Period 1", min = first_year(), 
        max = last_year(), value = c(1900,2000), sep = "")
})        
  
### Create the Period 2 input slider
output$period_slider_2 <- reactiveUI(function() {
sliderInput("period_slider_2", label = "Period 2 (years)", min = first_year(), 
        max = last_year(), value = c(rec_slider_2_start(),rec_slider_2_start()+diff(input$period_slider_1)), sep = "")
})      

###########################################################################
## Calculate extreme output
###########################################################################
extremes_table <- reactive({ 
	### If greater than, subset to greater than and sort in descending order
		if (input$extreme_direction == "gt") {
			### Subset table 
			extremes_table <- subset(paleo_ts_subset(), Monthly_Recon > input$extreme_flow)
			### Resort table
			extremes_table <- data.frame(extremes_table)
			extremes_table <- extremes_table[with(extremes_table, order(Monthly_Recon, Year)),]			
	### If less than, subset to greater than and sort in descending order
		} else {
			### Subset table 
			extremes_table <- subset(paleo_ts_subset(), Monthly_Recon < input$extreme_flow)
			### Resort table
			extremes_table <- data.frame(extremes_table)
			extremes_table <- extremes_table[with(extremes_table, order(Monthly_Recon, Year)),]
		}
		
	### Reorganize columns
	date_column <- as.Date(paste0(extremes_table$Year, "/",extremes_table$Month, "/15"))
	date_column <- format(date_column, "%b %Y")
	extremes_table <- data.frame(Date=date_column, Year=extremes_table$Year, Month=extremes_table$Month, "Reconst_Flow"=signif(extremes_table$Monthly_Recon,4), Observed=signif(extremes_table$Observed,4))	
		
	### Return extremes table
	extremes_table 
})

### Calculations for Extreme summary
most_extreme <- reactive({extremes_table()$Reconst_Flow[1]})
date_most_extreme <- reactive({as.character(extremes_table()$Date[1])})

threshold_exceed <- reactive({dim(extremes_table())[1]})
length_time_series <- reactive({sum(paleo_ts_subset()$Monthly_Rec > 0, na.rm=TRUE)})
freq_threshold_exceed <- reactive({threshold_exceed()/(1+length_time_series())})
return_per <- reactive({1/freq_threshold_exceed()})

### Output for Extreme summary
output$threshold_text <- renderUI({ HTML(paste0("<strong>Threshold</strong> :   ",input$extreme_flow, " ", flow_units())) })
output$most_extreme_text <- renderUI({ HTML(paste0("<strong>Most Extreme Flow</strong> :   ",most_extreme(), " ", flow_units())) })
output$date_most_extreme_text <- renderUI({ HTML(paste0("<strong>Date of Most Extreme Flow</strong> :   ",date_most_extreme())) })
output$threshold_exceed_text <- renderUI({ HTML(paste0("<strong>Threshold Exceedances</strong> :   ",threshold_exceed())) })
output$freq_threshold_exceed_text <- renderUI({ HTML(paste0("<strong>Likelihood of Threshold Exceedance</strong> :   ",signif(100*freq_threshold_exceed(),3), " %")) })
output$return_per_text <- renderUI({ HTML(paste0("<strong>Approximate (Empirical) Return Period</strong> :   ",signif(return_per(),2), " years")) })


###########################################################################
## Calculate Period Comparison
###########################################################################
### Extract Periods 1 and 2
period_1_subset <- reactive({ 
 	period_1_subset <- subset(paleo_ts_subset(), Year >= input$period_slider_1[[1]] & Year <= input$period_slider_1[[2]])
 	period_1_subset <- data.frame(period_1_subset)
 	date_column <- as.Date(paste0(period_1_subset$Year, "/",period_1_subset$Month, "/15"))
	period_1_subset$Date <- format(date_column, "%b %Y")
	period_1_subset
})

period_2_subset <- reactive({ 
 	period_2_subset <- subset(paleo_ts_subset(), Year >= input$period_slider_2[[1]] & Year <= input$period_slider_2[[2]])
 	period_2_subset <- data.frame(period_2_subset)
 	date_column <- as.Date(paste0(period_2_subset$Year, "/",period_2_subset$Month, "/15"))
	period_2_subset$Date <- format(date_column, "%b %Y") 	
	period_2_subset
})

period_vec <- reactive({c(paste0(input$period_slider_1[[1]], " - ", input$period_slider_1[[2]]), paste0(input$period_slider_2[[1]], " - ", input$period_slider_2[[2]]))})

### Create a dataframe for Period Information
period_info_df <- reactive({  
	### Create duration data
	duration_vec <- c((input$period_slider_1[[2]] - input$period_slider_1[[1]]), (input$period_slider_2[[2]] - input$period_slider_2[[1]]))		
  ### Compose data frame
    data.frame(
      Measure = c("Period", 
               "Number of Years"),
      Period_1 = as.character(c(period_vec()[1], 
      						duration_vec[1]
      						)),
      Period_2 = as.character(c(period_vec()[2], 
                             duration_vec[2]                           
                             )), 
      stringsAsFactors=FALSE)
})
      
### Create a dataframe for Period Extreme Comparison
period_extreme_df <- reactive({  
	### Max flows
   max_p1 <- signif(max(period_1_subset()$Monthly_Recon, na.rm=TRUE), 4)
   max_p2 <- signif(max(period_2_subset()$Monthly_Recon, na.rm=TRUE), 4)
   
   date_max_p1 <- period_1_subset()$Date[which.max(period_1_subset()$Monthly_Recon)]
   date_max_p2 <- period_2_subset()$Date[which.max(period_2_subset()$Monthly_Recon)]

	### Min flows
   min_p1 <- signif(min(period_1_subset()$Monthly_Recon, na.rm=TRUE), 4)
   min_p2 <- signif(min(period_2_subset()$Monthly_Recon, na.rm=TRUE), 4)
   
   date_min_p1 <- period_1_subset()$Date[which.min(period_1_subset()$Monthly_Recon)]
   date_min_p2 <- period_2_subset()$Date[which.min(period_2_subset()$Monthly_Recon)]
   
  ### Compose data frame
    data.frame(
      Measure = c("Period",
               paste0("Maximum Flow (",flow_units(),")"),
               "Date of Maximum",
               paste0("Minimum Flow (",flow_units(),")"),
               "Date of Minimum"),
      Period_1 = as.character(c(period_vec()[1], 
      						max_p1,date_max_p1,min_p1,date_min_p1
      						)),
      Period_2 = as.character(c(period_vec()[2], 
                             max_p2,date_max_p2,min_p2,date_min_p2                        
                             )), 
      stringsAsFactors=FALSE)
})





period_threshold_df <- reactive({  
   ### Threshold Exceedances
   if (input$extreme_direction == "gt") {
		threshold_exceed_p1 <- sum(period_1_subset()$Monthly_Recon > input$extreme_flow, na.rm=TRUE)
		threshold_exceed_p2 <- sum(period_2_subset()$Monthly_Recon > input$extreme_flow, na.rm=TRUE)
	} else {
		threshold_exceed_p1 <- sum(period_1_subset()$Monthly_Recon < input$extreme_flow, na.rm=TRUE)
		threshold_exceed_p2 <- sum(period_2_subset()$Monthly_Recon < input$extreme_flow, na.rm=TRUE)
	}
	
	length_p1 <- sum(period_1_subset()$Monthly_Recon > 0, na.rm=TRUE)
	length_p2 <- sum(period_2_subset()$Monthly_Recon > 0, na.rm=TRUE)
	
	freq_threshold_p1 <- threshold_exceed_p1/(1+length_p1)
	freq_threshold_p2 <- threshold_exceed_p2/(1+length_p2)
	
	freq_threshold_p1 <- paste0(signif(100*freq_threshold_p1,3), " %")
	freq_threshold_p2 <- paste0(signif(100*freq_threshold_p2,3), " %")

  ### Compose data frame
    data.frame(
      Measure = c("Period",
               paste0("Threshold (",flow_units(),")"),
               "Threshold Exceedances",
               "Likelihood of Exceedance"),
      Period_1 = as.character(c(period_vec()[1], 
      						input$extreme_flow,threshold_exceed_p1,freq_threshold_p1
      						)),
      Period_2 = as.character(c(period_vec()[2], 
                             input$extreme_flow,threshold_exceed_p2,freq_threshold_p2                           
                             )), 
      stringsAsFactors=FALSE)
})
      

###########################################################################
## Create Data for Period Comparison plot
########################################################################### 
period_compar_dist_df <- reactive({
		### Read in subsets
		period_1_subset <- period_1_subset()
		period_2_subset <- period_2_subset()
		
		### Remove NAs
		period_1_subset <- period_1_subset[!is.na(period_1_subset$Monthly_Recon),]
		period_2_subset <- period_2_subset[!is.na(period_2_subset$Monthly_Recon),]
				
		### Calculate plotting position
		period_1_subset$rank <- rank(period_1_subset$Monthly_Recon)
		period_1_subset$plot_pos <- period_1_subset$rank/(dim(period_1_subset)[1] + 1)
		period_1_subset$Period <- period_vec()[1]
		
		period_2_subset$rank <- rank(period_2_subset$Monthly_Recon)
		period_2_subset$plot_pos <- period_2_subset$rank/(dim(period_2_subset)[1] + 1)
		period_2_subset$Period <- period_vec()[2]
		
		### Return result
		rbind(period_1_subset, period_2_subset)
})


###########################################################################
## Output to extremes tab
########################################################################### 
   output$site_out <- renderPrint({
   period_compar_dist_df()
  })


###########################################################################
## Output to time series plot
########################################################################### 
  output$tsPlot <- 
    renderDygraph({
    dygraph(paleo_ts_plot(), main = site_name()) %>%
    dyRangeSelector()  %>%
   # dyRangeSelector(dateWindow = c("1850-01-01", "1995-01-01")) %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE, titleHeight= 28) %>%
    dyLegend(show = "always", hideOnMouseOut = FALSE, labelsSeparateLines=TRUE) %>%
    dyAxis("y", label = paste0("Monthly Mean Discharge (",flow_units(),")"), valueRange=y_lims()) %>%
    dySeries("Observed", color="#e41a1c", strokeWidth=0.8)  %>%
    dySeries("Monthly_Recon", color="#404040", strokeWidth = 0.8) %>% 
    dySeries("Annual_Recon", color="#377eb8", strokeWidth = 1.2, strokePattern = "dashed")
  	})    
    
###########################################################################
## Output to goodness of fit (Obs vs Reconstr) plot
###########################################################################   
 output$gof_scatter <- renderGvis({
	gvisComboChart(gof_df(), xvar = "Observed", yvar = c("Reconstructed", "Reconstructed.tooltip", "abline"),
          options=list(seriesType='scatter',
 			title=NULL,
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
### create range for density plot
	density_range <- reactive({
		l <- density(gof_distr_df()$Flow, na.rm=TRUE)
		range(l$x)
	})
	
 output$gof_distr <-renderPlot({
 		
 		### Create the plot
  		p <- ggplot(gof_distr_df(), aes(x=Flow, fill=Data))
  		p <- p + geom_density(alpha=0.3)
  		#p <- p + scale_fill_brewer(name="Data Source", palette="Dark2")
  		p <- p + scale_fill_manual(name="Data Source", values=c("#d95f02", "#1b9e77"))
  		p <- p + scale_x_continuous(name=paste0("Streamflow (",flow_units(),")"))
  		p <- p + scale_y_continuous(name="Density", expand=c(0,0))
  		p <- p + xlim(density_range())
  		p <- p + theme_light()
  		p
   })	



###########################################################################
## Output gof to a table
###########################################################################   
output$gof_table_simple <-renderTable({
    gof_table_df()
  }, digits=3)




###########################################################################
## Output extreme table
###########################################################################   

  output$extreme_table <- DT::renderDataTable({
   DT::datatable(extremes_table(), plugins='natural', rownames=FALSE, 
    	options = list(pageLength = 10, columnDefs = list(list(type = 'natural', targets = 0)) ))
  })


###########################################################################
## Output to distribution (Obs vs Reconstr) plot
###########################################################################   
 output$extreme_distr <-renderPlot({
 		### Create the plot
  		p <- ggplot(gof_df_temp(), aes(x=Reconstructed))
  		p <- p + geom_density(fill="#1b9e77", alpha=0.4)
  		p <- p + geom_vline(xintercept = input$extreme_flow, colour="red", linetype="longdash")
  		p <- p + scale_x_continuous(name=paste0("Streamflow (",flow_units(),")"))
  		p <- p + scale_y_continuous(name="Density", expand=c(0,0))
  		p <- p + xlim(density_range())
  		p <- p + theme_light()
  		p
   })	




###########################################################################
## Output Period Comparisons to tables
###########################################################################   
output$period_info_table <-renderTable({
    period_info_df()
  })
  
output$period_extreme_table <-renderTable({
    period_extreme_df()
  })
  
output$period_threshold_table <-renderTable({
    period_threshold_df()
  })
  
  
  
###########################################################################
## Output to Period distribution plot
###########################################################################   
 output$period_compar_dist <-renderPlot({
 		### Create the plot
  		p <- ggplot(period_compar_dist_df(), aes(x=plot_pos*100, y=Monthly_Recon, colour=Period))
  		p <- p + geom_line()
  		p <- p + scale_x_continuous(name="Percentile (%)")
  		#p <- p + scale_y_log10(name=paste0("Streamflow (",flow_units(),")"))
  		p <- p + scale_y_log10(name=paste0("Streamflow (",flow_units(),")"), breaks = c(1,5, 10,50,100,1000,10000))#trans_breaks("log10", function(x) 10^x))
        p <- p + annotation_logticks(sides="l")
  		p <- p + theme_light()
  		p
   })	


  
  
  
}

    