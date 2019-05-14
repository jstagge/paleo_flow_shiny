
###########################################################################
## Dynamic Input for Date Subset and Sites
###########################################################################
### Create selector for site id
observe({
	### For client to pass data through the URL and have the site pre-selected.
	### Example http://127.0.0.1:4516/?site_id=white2000 will have the location "White River (Summer)" pre-selected.
	query <- parseQueryString(session$clientData$url_search)
	if (!is.null(query[['site_id']])) {
		updateSelectInput(session, "site_name", label = NULL, choices = NULL, selected = query[['site_id']])
	}
	output$ui <- renderUI({
		### If no time resolution is selected
	    if (is.null(input$time_resolution))
	      return()
		### If time resolution is monthly
		if(input$time_resolution=='monthly'){
			selectizeInput("site_name", 'Site Location',
	        	choices = create_site_list(site_all, res="monthly"),
	   			selected = NULL,
	   			multiple = FALSE,
	   			options = list(placeholder = 'Select site location'),
	   			verbatimTextOutput("value")
	   		)	
		### If time resolution is annual
		} else {
			selectizeInput("site_name", 'Site Location',
	 	       choices = create_site_list(site_all, res="annual"),
	    		selected = NULL,
	   			multiple = FALSE,
	   			options = list(placeholder = 'Select site location'),
	   			verbatimTextOutput("value")
	   		)
		}
	})	        
})


### Create selector for time subset
output$time_subset <- renderUI({
	### If time resolution is monthly
	if(input$time_resolution=='monthly'){
		selectizeInput('time_subset', 'Date Subset', 
 			choices = c(`Full Timeseries` = '0', `January` = '1', `February` = '2', `March` = '3', `April` = '4', `May` = '5', `June` = '6', `July` = '7', `August` = '8', `September` = '9', `October` = '10', `November` = '11', `December` = '12' ),
 			multiple = FALSE) 
   	} else {
   		selectizeInput('time_subset', 'Date Subset', 
 			choices = c(`Full Timeseries` = '0'),
 			multiple = FALSE)
   	}	
})

###########################################################################
## Extract the subset information
###########################################################################
### Extract time resolution
resolution <- reactive({ as.numeric(input$time_resolution) })

### Set name of column for reconstruction
### No longer needed
#rec_col_name <- reactive({
#	if (input$time_resolution == "monthly") {
#		"Monthly_Recon"
#	} else if (input$time_resolution == "annual") {
#		"Annual_Recon"
#	}
#})



###########################################################################
## Determine site information
###########################################################################
### Determine which one of the read in time series from the number
col_name <- reactive({ 
	if (is.null(input$site_name)) {
		"None"
	} else {
		input$col_name
	}
})


#site_info <- reactive({
#	site_all %>%
#			filter(col_name == input$site_name & resolution == input$time_resolution) %>%
#			as.data.frame()
#})

### Extract Site Info
site_info <- reactive({
	if (is.null(input$site_name)){
		blank_site <- site_all[1,]
		blank_site[1,seq(1,length(blank_site))] <- NA
		blank_site$site_name <- "Please Select Site"
		blank_site
	} else {
		site_all %>%
			filter(col_name == input$site_name & resolution == input$time_resolution) %>%
			as.data.frame()
	}
})


### Extract Site Name
site_name <- reactive({ site_info()$site_name })
#col_name <- reactive({ site_info()$col_name }) ### I'm not sure this is needed

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
### This previously didn't happen

paleo_ts_temp <- reactive({
	### Filter to the correct site and resolution, create date columns	
	paleo_ts_temp <- flow_db %>%
			filter(col_name == input$site_name & resolution == input$time_resolution) %>%	### Filter to correct data
			mutate(month = case_when(resolution == "annual" ~ as.integer(1),
                        TRUE  ~ month)) %>%   ### Convert month to 1 if it is annual data
			mutate(date = as.Date(paste0(year, "-", month, "-01")))  ### Add a date column for plotting and other calculations

	### Perform unit conversion
	if(input$flow_units != "m3/s"){
		paleo_ts_temp <- paleo_ts_temp %>%
			mutate_at(c("annual_m3s", "obs_m3s", "recon_m3s"),  unit_conv, new_unit=input$flow_units, date=.$date, temp_resolution=.$resolution) ### Add in the unit scaling
	}

	### Create complete time series and sort by date
	if (input$time_resolution == "annual"){	
		paleo_ts_temp <- paleo_ts_temp %>%
			complete(date = seq.Date(min(date, na.rm=TRUE), max(date, na.rm=TRUE), by="year")) %>%
			arrange(date)
	} else if (input$time_resolution == "monthly"){
		paleo_ts_temp <- paleo_ts_temp %>%
			complete(date = seq.Date(min(date, na.rm=TRUE), max(date, na.rm=TRUE), by="month")) %>%
			arrange(date)
	}

	### Export the results as a dataframe
	paleo_ts_temp %>%
		as.data.frame()
})


### In the bear monthly, there are a bunch of NA dates with annual flow - check this
### The annual doesn't go for the full length and has a weird thing at the end where it jumps from annual to monthly - something with the read in


### For plotting, need to convert to an xts?
#xts(paleo_ts_temp, date_vec)



###########################################################################
## Create the periods input
###########################################################################
current_year <-  as.integer(format(Sys.Date(), "%Y"))

first_year <- reactive({ min(paleo_ts_temp()$year, na.rm=TRUE) })
last_year <- reactive({ max(paleo_ts_temp()$year, na.rm=TRUE) })

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
## Process for time series plotting, remove date columns and convert units
###########################################################################

paleo_ts_plot <- reactive({
	if (is.null(input$site_name)){
		### Generate a blank graph
		paleo_ts_temp <- data.frame(Observed=rep(NA,300), Annual_Recon=rep(NA,300), Monthly_Recon=rep(NA,300))
		paleo_ts_plot <- ts(as.matrix(paleo_ts_temp), start=c(1700,1), frequency=12)
	} else {
		if(input$time_resolution == "monthly") {	
			paleo_ts_plot <- paleo_ts_temp() %>%
				select(date, obs_m3s, annual_m3s, recon_m3s) %>%
				rename("Observed" = "obs_m3s") %>%
				rename("Annual_Recon" = "annual_m3s") %>%
				rename("Monthly_Recon" = "recon_m3s")
		} else {
			paleo_ts_plot <- paleo_ts_temp() %>%
				select(date, obs_m3s, recon_m3s) %>%
				rename("Observed" = "obs_m3s") %>%
				rename("Annual_Recon" = "recon_m3s")
		}

	paleo_ts_plot <- paleo_ts_plot %>%		
		ts_long() %>%
		ts_xts()
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
## Create the extremes input
###########################################################################
### Suggest the maximum flow in subset
min_suggest <- reactive({ floor(min(c(paleo_ts_plot()), na.rm=TRUE)) })
max_suggest <- reactive({ ceiling(max(c(paleo_ts_plot()), na.rm=TRUE)) })

### Suggest an extreme flow based on the 15th or 85 percentile (depending if gt or less than)
suggest_extreme <- reactive({ if (input$extreme_direction == "gt") {
			signif(min_suggest() + 0.85*(max_suggest() - min_suggest()), 2)
		} else {
			signif(min_suggest() + 0.15*(max_suggest() - min_suggest()), 2)
		}
	})
	
### Suggest the size of steps	
suggest_steps <- reactive({ 
	suggest_steps <- (max_suggest() - min_suggest())/200
	signif(suggest_steps, 2)
	})	

### Create the extreme threshold input	
output$extreme_flow <- renderUI({
	sliderInput("extreme_flow", label = "Extreme flow", min = min_suggest(), 
        max = max_suggest(), value = suggest_extreme(), step = suggest_steps())
})        


###########################################################################
## Calculate extreme output
###########################################################################
extreme_table <- reactive({ 
	
	extreme_table <- paleo_ts_temp() %>%
		select(date, year, month, obs_m3s, recon_m3s)

	if(input$extreme_direction == "gt") {	
		extreme_table <- extreme_table %>%
			filter(recon_m3s > input$extreme_flow) %>%
			arrange(-recon_m3s)
	} else {
		extreme_table <- extreme_table %>%
			filter(recon_m3s < input$extreme_flow) %>%
			arrange(recon_m3s)
	}

	### Reorganize columns
	extreme_table <- extreme_table %>%
		mutate(recon_m3s = signif(recon_m3s,4)) %>%
		mutate(obs_m3s = signif(obs_m3s,4)) 

	if(input$time_resolution == "monthly") {	
		extreme_table <- extreme_table %>%
			mutate(date = format(date, "%b %Y")) %>%
			rename("Year" = "year")  %>%
			rename("Month" = "month")  %>%
			rename("Date" = "date") 
	} else {
		extreme_table <- extreme_table %>%
			select(-date, -month) %>%
			rename("Year" = "year")
	}
#paste0("Reconstructed" , input$flow_units)
#df %>% rename(!!variable := name_of_col_from_df)

	### Return extremes table
	extreme_table %>%
		as.data.frame()
})

### Calculations for Extreme summary
most_extreme <- reactive({extreme_table()$recon_m3s[1]})
date_most_extreme <- reactive({ if(input$time_resolution == "monthly") {
		as.character(extreme_table()$Date[1])
	} else {
		as.character(extreme_table()$Year[1])
	}
})

threshold_exceed <- reactive({dim(extreme_table())[1]})
length_time_series <- reactive({sum(paleo_ts_temp()$recon_m3s > 0, na.rm=TRUE)})
freq_threshold_exceed <- reactive({threshold_exceed()/(1+length_time_series())})
return_per <- reactive({1/freq_threshold_exceed()})

### Output for Extreme summary
output$threshold_text <- renderUI({ HTML(paste0("<strong>Threshold</strong> :   ",input$extreme_flow, " ", input$flow_units)) })

output$most_extreme_text <- renderUI({ if(input$extreme_direction == "gt") {
		HTML(paste0("<strong>Maximum Flow</strong> :   ",most_extreme(), " ", input$flow_units))
	} else {
		HTML(paste0("<strong>Minimum Flow</strong> :   ",most_extreme(), " ", input$flow_units))
	}
})

output$date_most_extreme_text <- renderUI({ if(input$extreme_direction == "gt") {
		HTML(paste0("<strong>Date of Maximum Flow</strong> :    ",date_most_extreme()))
	} else {
		HTML(paste0("<strong>Date of Minimum Flow</strong> :    ",date_most_extreme()))
	}
})

output$threshold_exceed_text <- renderUI({ HTML(paste0("<strong>Threshold Exceedances</strong> :   ",threshold_exceed())) })
output$freq_threshold_exceed_text <- renderUI({ HTML(paste0("<strong>Likelihood of Threshold Exceedance</strong> :   ",signif(100*freq_threshold_exceed(),3), " %")) })
output$return_per_text <- renderUI({ HTML(paste0("<strong>Approximate (Empirical) Return Period</strong> :   ",signif(return_per(),2), " years")) })



###########################################################################
## Calculate Period Comparison
###########################################################################
### Extract Periods 1 and 2
period_1_subset <- reactive({ 
 	paleo_ts_temp() %>%
		filter(year >= input$period_slider_1[[1]] & year <= input$period_slider_1[[2]])
})

period_2_subset <- reactive({ 
 	paleo_ts_temp() %>%
		filter(year >= input$period_slider_2[[1]] & year <= input$period_slider_2[[2]])
})

period_vec <- reactive({c(paste0(input$period_slider_1[[1]], " - ", input$period_slider_1[[2]]), paste0(input$period_slider_2[[1]], " - ", input$period_slider_2[[2]]))})

### Create a dataframe for Period Information
period_info_df <- reactive({  
	### Create duration data
	duration_vec <- c((input$period_slider_1[[2]] - input$period_slider_1[[1]]), (input$period_slider_2[[2]] - input$period_slider_2[[1]]))		
  ### Compose data frame
    data.frame(
      Measure = c("Period", "Number of Years"),
      Period_1 = as.character(c(period_vec()[1], duration_vec[1] )),
      Period_2 = as.character(c(period_vec()[2], duration_vec[2] )), 
      stringsAsFactors=FALSE)
})
      

### Create a dataframe for Period Extreme Comparison
period_extreme_df <- reactive({  
	### Max flows
   max_p1 <- signif(max(period_1_subset()$recon_m3s, na.rm=TRUE), 4)
   max_p2 <- signif(max(period_2_subset()$recon_m3s, na.rm=TRUE), 4)
   
	### Min flows
   min_p1 <- signif(min(period_1_subset()$recon_m3s, na.rm=TRUE), 4)
   min_p2 <- signif(min(period_2_subset()$recon_m3s, na.rm=TRUE), 4)
   
	if(input$time_resolution == "monthly") {
		date_max_p1 <- period_1_subset()$date[which.max(period_1_subset()$recon_m3s)]
   		date_max_p2 <- period_2_subset()$date[which.max(period_2_subset()$recon_m3s)]
		date_min_p1 <- period_1_subset()$date[which.min(period_1_subset()$recon_m3s)]
   		date_min_p2 <- period_2_subset()$date[which.min(period_2_subset()$recon_m3s)]
	} else {
		date_max_p1 <- period_1_subset()$year[which.max(period_1_subset()$recon_m3s)]
   		date_max_p2 <- period_2_subset()$year[which.max(period_2_subset()$recon_m3s)]
		date_min_p1 <- period_1_subset()$year[which.min(period_1_subset()$recon_m3s)]
   		date_min_p2 <- period_2_subset()$year[which.min(period_2_subset()$recon_m3s)]
	}
   
  ### Compose data frame
    data.frame(
      Measure = c("Period", paste0("Maximum Flow (",input$flow_units,")"),  "Date of Maximum",  paste0("Minimum Flow (",input$flow_units,")"), "Date of Minimum"),
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
		threshold_exceed_p1 <- sum(period_1_subset()$recon_m3s > input$extreme_flow, na.rm=TRUE)
		threshold_exceed_p2 <- sum(period_2_subset()$recon_m3s > input$extreme_flow, na.rm=TRUE)
	} else {
		threshold_exceed_p1 <- sum(period_1_subset()$recon_m3s < input$extreme_flow, na.rm=TRUE)
		threshold_exceed_p2 <- sum(period_2_subset()$recon_m3s < input$extreme_flow, na.rm=TRUE)
	}
	
	length_p1 <- sum(period_1_subset()$recon_m3s > 0, na.rm=TRUE)
	length_p2 <- sum(period_2_subset()$recon_m3s > 0, na.rm=TRUE)
	
	freq_threshold_p1 <- threshold_exceed_p1/(1+length_p1)
	freq_threshold_p2 <- threshold_exceed_p2/(1+length_p2)
	
	freq_threshold_p1 <- paste0(signif(100*freq_threshold_p1,3), " %")
	freq_threshold_p2 <- paste0(signif(100*freq_threshold_p2,3), " %")

  ### Compose data frame
    data.frame(
      Measure = c("Period",
               paste0("Threshold (",input$flow_units,")"),
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

		period_1_subset <- period_1_subset() %>% 
			drop_na(recon_m3s) %>%
			mutate(rank = rank(recon_m3s)) %>%
			mutate(plot_pos = rank/(length(recon_m3s) + 1)) %>%
			mutate(period = period_vec()[1])

		period_2_subset <- period_2_subset() %>% 
			drop_na(recon_m3s) %>%
			mutate(rank = rank(recon_m3s)) %>%
			mutate(plot_pos = rank/(length(recon_m3s) + 1)) %>%
			mutate(period = period_vec()[2])

		### Return result
		rbind(period_1_subset, period_2_subset)
})

###########################################################################
## Prepare Output for download
###########################################################################
 output$downloadData <- downloadHandler(
    filename = function() { paste(site_info()$col_name, '_',input$flow_units,'.csv', sep='') },
    content = function(file) {
      write.csv(paleo_ts_temp(), file)
    }
  )

###########################################################################
## Output to time series plot
########################################################################### 
output$tsPlot <-  renderDygraph({
	### If the user selects a site	
	if(nchar(input$site_name) > 1) {

 		p <- dygraph(paleo_ts_plot(), main = site_info()$site_name)

		if(input$time_resolution == "monthly") {	
			p <- p %>%
				dyAxis("y", label = paste0("Monthly Mean Discharge (",input$flow_units,")"), valueRange=y_lims()) %>%
				dySeries("Observed", color="#e41a1c", strokeWidth=0.8)  %>%
 				dySeries("Monthly_Recon", color="#404040", strokeWidth = 0.8) %>% 
 				dySeries("Annual_Recon", color="#377eb8", strokeWidth = 1.2, strokePattern = "dashed")
		} else {
			p <- p %>%
				dyAxis("y", label = paste0("Annual Mean Discharge (",input$flow_units,")"), valueRange=y_lims()) %>%
#				dySeries("Observed", color="#4477AA", strokeWidth=0.8)  %>% 
#				dySeries("Annual_Recon", color="#EE6677", strokeWidth = 0.8) 
				dySeries("Observed", color="#e41a1c", strokeWidth=0.8)  %>% 
				dySeries("Annual_Recon", color="#404040", strokeWidth = 0.8) 
		
#E69F00 orange
#56B4E9 sky blue
#009E73 greenish
#RColorBrewer::brewer.pal(3, "Set2")

		}	

	} else {
		### create a blank plot Before the user has selected a site
		blank_ts <- data.frame(Observed=rep(NA,300), Annual_Recon=rep(NA,300))
		blank_plot <- ts(as.matrix(blank_ts), start=c(1700,1), frequency=1)

		p <- dygraph(blank_plot) %>%
				dyAxis("y", label = paste0("Annual Mean Discharge (",input$flow_units,")")) %>%
				dySeries("Observed", color="#e41a1c", strokeWidth=0.8)  %>% 
				dySeries("Annual_Recon", color="#404040", strokeWidth = 0.8) 

	}
	### Format the time series plot 
	p  %>% 
	 	dyRangeSelector() %>% 
  		dyUnzoom() %>% 
		dyCrosshair(direction = "vertical") %>%
 		dyOptions(axisLineWidth = 1.5, drawGrid = FALSE, titleHeight= 28, animatedZooms = TRUE) %>%
		dyHighlight(highlightSeriesOpts = list(strokeWidth = 0.9), highlightSeriesBackgroundAlpha = 0.9, hideOnMouseOut = TRUE) %>%
  		dyLegend(show = "always", hideOnMouseOut = FALSE, labelsSeparateLines=TRUE) %>%
    	dyAxis(name="x",axisLabelFormatter = "function(d){ return d.getFullYear() }"  ) %>%
    	dyCallbacks(drawCallback = dyRegister())
})


###########################################################################
## Output to Map
###########################################################################   
selected_icon <- makeAwesomeIcon(icon = "star", library = "fa", markerColor = "lightgreen")

site_map <- reactive({ 

	map_df <- site_all %>% 
		filter(resolution == input$time_resolution) %>%
		select(site_name, lat, lon) %>%
		rename("long" = "lon") %>%
		rename("name" = "site_name") %>%
		as.data.frame()
	
	leaf_map <- leaflet(data = map_df) %>% 
		addMarkers(~long, ~lat, popup = ~as.character(name), label = ~as.character(name)) %>%
		#map <- map  %>% addTiles()
		addProviderTiles(providers$Esri.NatGeoWorldMap)
	#	addProviderTiles(providers$Stamen.Terrain)

	if(nchar(input$site_name) > 1) {
		leaf_map <- leaf_map  %>%
			#addCircleMarkers(~long, ~lat, popup = ~as.character(name), label = ~as.character(name)) %>%
			addAwesomeMarkers(lng=site_info()$lon, lat=site_info()$lat, label=site_info()$name, icon = selected_icon) %>%
			setView(lng = site_info()$lon, lat = site_info()$lat, zoom = 8) 
	}
	leaf_map
})

output$mymap <- renderLeaflet({ site_map() })

###########################################################################
## Output extreme table
###########################################################################   

  output$extreme_table <- DT::renderDataTable({

	recon_header <- paste0("Reconstructed " , input$flow_units)
	obs_header <- paste0("Observed " , input$flow_units)

	extreme_out <- extreme_table() %>%
		rename(!!obs_header := obs_m3s) %>%
		rename(!!recon_header := recon_m3s) 

   DT::datatable(extreme_out, plugins='natural', rownames=FALSE, 
    	options = list(pageLength = 10, columnDefs = list(list(type = 'natural', targets = 0)) ))
  })

###########################################################################
## Output to distribution (Obs vs Reconstr) plot
###########################################################################   
### create range for density plot
	density_range <- reactive({
		l <- density(paleo_ts_temp()$recon_m3s, na.rm=TRUE)
		range(l$x)
	})

 output$extreme_distr <-renderPlot({
 		### Create the plot
  		p <- ggplot(paleo_ts_temp(), aes(x=recon_m3s)) %>%
  			+ geom_density(fill="#1b9e77", alpha=0.4) %>%
  			+ geom_vline(xintercept = input$extreme_flow, colour="red", linetype="longdash") %>%
  			+ scale_x_continuous(name=paste0("Streamflow (", input$flow_units,")")) %>%
  			+ scale_y_continuous(name="Density", expand=c(0,0)) %>%
  			+ xlim(density_range()) %>%
  			+ theme_light()
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
  		ggplot(period_compar_dist_df(), aes(x=plot_pos*100, y=recon_m3s, colour=period)) %>%
			+ geom_line() %>%
			+ scale_colour_brewer(name = "Period", type="qual", palette="Dark2") %>%
			+ scale_x_continuous(name="Percentile (%)") %>%
			+ scale_y_log10(name=paste0("Streamflow (",input$flow_units,")"), breaks = c(1,5, 10,50,100,1000,10000)) %>% #trans_breaks("log10", function(x) 10^x))
			+ annotation_logticks(sides="l") %>%
			+ theme_light()
})	

###########################################################################
## For troubleshooting
###########################################################################   

#output$text1 <- renderText({ 
 #     paste("list_id=",list_id(),"site_info=", site_info())
#    })

output$text1 <- renderText({ extremes_table()$Reconst_Flow[1] })

output$testing_table <- renderDataTable(period_compar_dist_df())
#output$testing_table <- renderDataTable(paleo_ts_temp())
  
  
 