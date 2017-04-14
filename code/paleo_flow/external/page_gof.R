###########################################################################
## Goodness of Fit Tab
###########################################################################          

column(9,
	column(12,
	h2("Goodness of Fit"),
			h3("Summary Statistics"),
        	p("Reconstructed flows are compared against observed flows at the site during the instrumental period"),
			tableOutput('gof_table_simple'),
        	helpText("Fit criteria are: ME (Mean error), RSME (Root Mean Square Error), NSE (Nash-Sutcliffe Efficiency), and R (Pearson Correlation Coefficient). "),        	        	
	       	h3("Observed vs. Reconstructed Flow"),
        	htmlOutput("gof_scatter"),
        	br(),
        	helpText("Hover over a point to see its date and values. Click and drag to zoom in on an area. Right click to zoom out."),

        	#tableOutput('gof_table'),
        	#DT::dataTableOutput("gof_table"),
        	
        	
        	h3("Comparison of Flow Distributions"),
        	plotOutput("gof_distr")  
	
	)
)
