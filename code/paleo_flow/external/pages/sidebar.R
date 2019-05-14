### Sidebar column 
	conditionalPanel(
	condition = "input.nav_value != 'submit' & input.nav_value != 'about' ",
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
			choices = c(`Annual` = 'annual', `Monthly` = 'monthly'),
			selected='annual',
			multiple = FALSE)
		)),
	
		fluidRow(column(12,
			### Input for site location
 			uiOutput("ui")
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
			uiOutput("time_subset")
		)
		)),
	### Download Button
   		downloadButton('downloadData', 'Download Data', class="btn-primary")
		
	
	),
	fluidRow(column(12,
		p("For monthly reconstructions, it is possible to focus on particular months. Selections on the left will populate all other tabs.")
	))
   )
)
