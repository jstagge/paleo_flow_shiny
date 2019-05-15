###########################################################################
## Goodness of Fit Tab
###########################################################################          

column(9,
	column(12,
	h2("Goodness of Fit"),
			### Add a loading notice
			#conditionalPanel(
			#	condition="$('html').hasClass('shiny-busy')",
			#	fluidRow(column(3, HTML('<div class="alert alert-info" role="alert">Data loading.</div>')))
			#),
   #       	h3("Observed vs. Reconstructed Flow"),
       		p("Reconstructed flows plotted against observed flows for the instrumental (calibration) period. A perfect calibration fit would be along the 1:1 line, shown in red."),
        	p("Hover over a point to see its date and values. Click and drag to zoom in on an area. Right click to zoom out."),
        	htmlOutput("gof_scatter"),

        	#tableOutput('gof_table'),
        	#DT::dataTableOutput("gof_table"),
        	
        				#dataTableOutput('table'),
   			#verbatimTextOutput('hoverIndex'),
			#dataTableOutput('table2'),

			fluidRow(
				column(6, 
					h3("Calibration"),
					tableOutput('cal_table')
				), 
				column(3, 
					br(),
					br(),
					bsCollapse(id = "collapse_cal", open = "Calibration Explained",
                 		bsCollapsePanel("Calibration Explained","Text goes here",  style = "info")
					)
				),

				column(3, 
					br(),
					br(),
					bsCollapse(id = "collapse_cal_metrics", open = NULL,
                 		bsCollapsePanel("RMSE (Root Mean Squared Error)", "In original units. The square root of the mean square of the calibration errors (estimated flow
minus observed flow). In other words, the measure of the central tendency of the calibration errors. ", style = "primary"), 
                		bsCollapsePanel("Mean Absolute Error", "In original units, similar to RMSE", style = "primary"),
                 		bsCollapsePanel("R^2 (Variance Explained)", "Percent variance explained. A measure of the proportion of total variation about the mean in the predictand
(observed flow or climate record) that is explained by the regression model.", style = "primary"),
                 		bsCollapsePanel("Mean Error", "Measures bias towards over-estimating or under-estimating", style = "primary")
					)
				)



			),

			fluidRow(
				column(6, 
					h3("Validation"),
					tableOutput('val_table')
				), 
				column(6, 
					br(),
					br(),
					bsCollapse(id = "collapse_val", open = "Validation Explained",
                 		bsCollapsePanel("Validation Explained", "Typically performed by cross-validation, e", style = "info"), 
                 		bsCollapsePanel("RMSE (Root Mean Squared Error)", "In original units. The square root of the mean square of the calibration errors (estimated flow
minus observed flow). In other words, the measure of the central tendency of the calibration errors. ", style = "primary"), 
                		bsCollapsePanel("NSE (Nash-Sutcliffe Efficiency", "Nash-Sutcliffe Efficiency, also called Reduction of Error. Above 0 means the model has some skill, does better than historical mean.", style = "primary")
					)
				)
			),

        	h3("Comparison of Flow Distributions"),
        	plotOutput("gof_distr")  
	
	)
)
