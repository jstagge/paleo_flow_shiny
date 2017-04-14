column(9,
	column(12,
	### Uncomment for troubleshooting
    #textOutput("text1"),
    	fluidRow(h2("Reconstructed Time Series Overview")),
		fluidRow(
			p("Please select a temporal resolution (Annual or Monthly) and a site on the left to view a reconstructed time series. For monthly reconstructions, it is possible to focus on particular months. These selections will populate the other tabs.")
	)),
	column(12,
		dygraphOutput("tsPlot"),
        br()),
    fluidRow(column(10, offset=1,    
        helpText("Plots are dynamic. Click and drag within the time series to zoom or use the scroll bar at the bottom. Double-click on the graph to zoom out.")
	)),
	fluidRow(
		column(6,
			h3("Reconstruction Source"),
            ### Text information          
			p(htmlOutput("recon_name_text")),
			p(htmlOutput("recon_author_text")),
			p(htmlOutput("recon_link_text")),
			p(tags$strong("Recommended Citation:"), textOutput("recon_citation_text"))
		),
		column(6,
			h3("Observation Source"),
            ### Text information   
            p(htmlOutput("base_name_text")),
			p(htmlOutput("base_author_text")),
			p(htmlOutput("base_link_text"))
		)
	) 	
)
