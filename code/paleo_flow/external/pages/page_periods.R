###########################################################################
## Period Comparison Tab
###########################################################################

column(9,
	column(12,
        	h2("Period Comparison"),
        	### Add a loading notice
			conditionalPanel(
				condition="$('html').hasClass('shiny-busy')",
				fluidRow(column(3, HTML('<div class="alert alert-info" role="alert">Data loading.</div>')))
			),
        	helpText("This tab allows a quick comparison between two user-selected historical periods. Select the period on the left. Extreme threshold can be adjusted.")  ,	
	      	#tableOutput("period_info_table"),
			fluidRow(column(6,
				DT::dataTableOutput("period_info_table")
			)),
			br(),
	      	fluidRow(column(6,
	      		h4("Extreme Flows"),
				DT::dataTableOutput("period_extreme_table")
	       		#tableOutput("period_extreme_table")
	       	),
	       	column(6,
	       		h4("Threshold Exceedances"),
				DT::dataTableOutput("period_threshold_table")
	       		#tableOutput("period_threshold_table")
	       	)),
	       	h3("Flow Distribution Comparison"),
	       	helpText("Flow distributions can be compared between the two periods. Plot will adjust automatically to changes in period."),			
			br(),			
			plotOutput("period_compar_hist"),
			plotOutput("period_compar_dist")
	)			
)
