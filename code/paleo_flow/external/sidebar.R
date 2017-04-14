### Sidebar column 
column(3,
		conditionalPanel(
	condition = "input.nav_value == 'extremes' | input.nav_value == 'periods'",
		wellPanel(
		conditionalPanel(
			condition = "input.nav_value == 'periods'",
			h4("Period Comparison"),
		### Input for Period sliders
		uiOutput("period_slider_1")	,
		uiOutput("period_slider_2"),
		helpText("Choose two historical periods to compare. Period 2 will default to the same duration.")
			),
		h4("Extreme Threshold"),
		 # Less than or greater than input
  		selectInput("extreme_direction", "Threshold Direction",
    	choices = list("Less than (<)" = "lt", "Greater than (>)" = "gt"), 
    	selected = 1, width='60%'),		
		### Input for threshold is calculated from server using percentiles
		uiOutput("extreme_flow"),
		helpText("Choose a flow threshold and < or > to access Extremes tab.")
		)
	),
	wellPanel(
		fluidRow(column(12,
			### Input for time resolution
			selectizeInput('time_resolution', 'Time Resolution', 
			choices = c(`Monthly` = 'monthly', `Annual` = 'annual'),
			multiple = FALSE)
		)),
		fluidRow(column(12,
			### Input for site location
 			selectizeInput('site_name', 'Site Location', 
 			choices =  create_site_list(site_all, res = "monthly"),
    		selected = NULL,
    		multiple = FALSE,
    		options = list(
          		placeholder = 'Select site location'
        		)
        	)     
		)),
		fluidRow(column(12,
		### Input for units
		selectizeInput('flow_units', 'Flow Units', 
			choices = c(`Mean m3/s` = 'm3/s', `Mean ft3/s` = 'cfs', `Total acre-ft` = 'ac-ft'),
			multiple = FALSE)
		)),
		
		fluidRow(column(12,
		conditionalPanel(
		condition = "input.time_resolution == 'monthly'",
 		selectizeInput('time_subset', 'Date Subset', 
 			choices = c(`Full Timeseries` = '0', `January` = '1', `February` = '2', `March` = '3', `April` = '4', `May` = '5', `June` = '6', `July` = '7', `August` = '8', `September` = '9', `October` = '10', `November` = '11', `December` = '12' ),
 			multiple = FALSE))
		)),
	### Download Button
   		downloadButton('downloadData', 'Download Data', class="btn-primary")
		
	
	)
)
