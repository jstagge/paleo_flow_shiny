
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
## For troubleshooting
###########################################################################   

#output$text1 <- renderText({ 
 #     paste("list_id=",list_id(),"site_info=", site_info())
#    })

output$text1 <- renderText({ input$site_name })

output$testing_table <- renderDataTable(paleo_ts_temp())
#output$testing_table <- renderDataTable(paleo_ts_temp())
  
  
 