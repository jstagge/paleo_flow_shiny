
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
