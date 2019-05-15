column(9,
	column(12,
		#textOutput("rwarn"),
		#textOutput("nsewarn"),
    	fluidRow(h2("Reconstructed Time Series Overview")),
    	conditionalPanel(
			condition = "input.site_name == ''",
			fluidRow(
				column(12, 
					HTML('<div class="alert alert-warning" role="alert">On the left, please select Annual/Monthly reconstruction and a Site to begin.</div>')
				)
			)
		),
    	conditionalPanel(
			condition = "output.rwarn == 'warn' | output.nsewarn == 'warn'",
			fluidRow(
				column(12, 
					HTML('<div class="alert alert-warning" role="alert">There may be fitting issues with this reconstuction, please check the Goodness of Fit tab for details.</div>')
				)
			)
		)
	),
	column(12,
		fluidRow(
			column(2,offset=10,
				dyDownload("tsPlot", "Download Plot", asbutton = TRUE, class="btn-primary btn-sm")
			)
		),
		
		dataTableOutput("testing_table"),
		dygraphOutput("tsPlot"),
        br()
	),
    fluidRow(
		column(10, offset=1,    
       		helpText("Plots are dynamic. Click and drag within the time series to zoom or use the scroll bar at the bottom. Double-click on the graph to zoom out.")
		)
	),
	fluidRow(
		column(12, 
			h3("Reconstruction Location")
		),
		column(8,offset=2,
			leafletOutput("mymap")
		)
	),
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


