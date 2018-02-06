###########################################################################
## Extremes Tab
###########################################################################           

column(9,
	column(12,
		fluidRow(
		h3("Extreme Flows")
		),
		### Add a loading notice
		conditionalPanel(
			condition="$('html').hasClass('shiny-busy')",
			fluidRow(column(3, HTML('<div class="alert alert-info" role="alert">Data loading.</div>')))
		),
	
		fluidRow(column(5,
			h5(htmlOutput("most_extreme_text")),
			h5(htmlOutput("date_most_extreme_text"))
			),
			column(7,
			h5(htmlOutput("threshold_text")),
			h5(htmlOutput("threshold_exceed_text")),
			h5(htmlOutput("freq_threshold_exceed_text")),
			h5(htmlOutput("return_per_text"))
			)
		),
 	    h3("Extreme Flow Distribution"),  
 	   # fluidRow(
 	   # 	column(5,     	
       # 	plotOutput("extreme_distr")
       # 	),
       # 	column(7,
       # 	DT::dataTableOutput("extreme_table")
       # 	)
       # )
        
        
        
 	    fluidRow(column(12,     	
        plotOutput("extreme_distr")
        )),
        h3("Extreme Flow Details"), 
        fluidRow(column(10, offset=1,
       	helpText("Flows are sorted by most extreme. You may re-sort by clicking column headers. You may also change the period of interest using the Date Subset drop-down."),
		DT::dataTableOutput("extreme_table")	
		))
	)			
)
