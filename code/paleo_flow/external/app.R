
###########################################################################
## Dynamic Input for Date Subset and Sites
###########################################################################
### Create selector for site id
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
   			options = list(placeholder = 'Select site location')
   		)	
	### If time resolution is annual
	} else {
		selectizeInput("site_name", 'Site Location',
 	       choices = create_site_list(site_all, res="annual"),
    		selected = NULL,
   			multiple = FALSE,
   			options = list(placeholder = 'Select site location')
   		)
	}
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
	if (input$site_name == "") {
		"None"
	} else {
		input$site_name
	}
})



### Extract Site Info
site_info <- reactive({
	if (col_name()=="None"){
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
col_name <- reactive({ site_info()$col_name }) ### I'm not sure this is needed

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
## Extract Unit Conversion
###########################################################################
### Extract the flow units
flow_units <- reactive({ input$flow_units })

### Need year month to make ac-ft work

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
					
			### If Monthly, convert to ac-ft per month
			if (input$time_resolution=='monthly') {
				unit_conv <- unit_conv * 60*60*24*days_in_month(year_month())
			### If annual, convert to ac-ft per year
			} else if (input$time_resolution=='annual') {
				unit_conv <- unit_conv * 60*60*24*(365+as.numeric(leap_year(year_month())))
			}	
		}
	unit_conv
})

###########################################################################
## Process the time series, keep date columns for later
###########################################################################
### Include a catch for before you select a site (blank plot)

paleo_ts_temp <- reactive({
	flow_db %>%
			filter(col_name == input$site_name & resolution == input$time_resolution) %>%
			as.data.frame()

})




### In the bear monthly, there are a bunch of NA dates with annual flow - check this
### The annual doesn't go for the full length and has a weird thing at the end where it jumps from annual to monthly - something with the read in


### For plotting, need to convert to an xts?
#xts(paleo_ts_temp, date_vec)



###########################################################################
## Extract dates from time series
###########################################################################
#paleo_ts_dates <- reactive({
#	paste0("", paleo_ts_temp()$Month, " / ", paleo_ts_temp()$Year, "")
#})

#year_month <- reactive({
#		as.Date(paste0(as.numeric(paleo_ts_temp()$Year),"-", as.numeric(paleo_ts_temp()$Month),"-15"))
#})




###########################################################################
## For troubleshooting
###########################################################################   

#output$text1 <- renderText({ 
 #     paste("list_id=",list_id(),"site_info=", site_info())
#    })

output$text1 <- renderText({ input$site_name })


output$testing_table <- renderDataTable(paleo_ts_temp())
  
  
 