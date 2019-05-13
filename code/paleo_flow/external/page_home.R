column(9,
	column(12,
	### Uncomment for troubleshooting
    #textOutput("text1"),
    	fluidRow(h2("Reconstructed Time Series Overview")),
    	    textOutput("text1"),
			withSpinner(dygraphOutput("tsPlot")),
    	    dataTableOutput('testing_table')

    ) 	
)

