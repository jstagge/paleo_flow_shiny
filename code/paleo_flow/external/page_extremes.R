###########################################################################
## Extremes Tab
###########################################################################           

column(9,
	column(12,
		DT::dataTableOutput("extreme_table"),

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
		)	
	)			
)
