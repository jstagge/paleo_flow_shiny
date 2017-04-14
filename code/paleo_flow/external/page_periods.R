###########################################################################
## Period Comparison Tab
###########################################################################

column(9,
	column(12,
        	h2("Period Comparison"),
        	helpText("This tab allows a quick comparison between two user-selected historical periods. Select the period on the left.")  ,	
	      	tableOutput("period_info_table"),
	      	fluidRow(column(6,
	      	h4("Extreme Flows"),
	       	tableOutput("period_extreme_table")
	       	),
	       	column(6,
	       	h4("Threshold Exceedances"),
	       	tableOutput("period_threshold_table")
	       	)),
	       	h3("Flow Distribution Comparison"),
	       	helpText("Flow distributions can be compared between the two periods. Plot will adjust automatically to changes in period. All flows shown on a log scale."),
	       	plotOutput("period_compar_dist")	
	)			
)
