
fluidPage(
  # Application title and theme
  theme = shinytheme("sandstone"),
  # Google Analytics script
  tags$head(includeScript("google-analytics.js")),
  # Title
  titlePanel("Reconstructed Streamflow Explorer"),
  # This line loads the Google Charts JS library
	googleChartsInit(),
###########################################################################
## Sidebar panel
###########################################################################  
  sidebarLayout(
  # Sidebar with a slider and selection inputs  
    sidebarPanel(
    	### Input for time resolution
		selectizeInput('time_resolution', 'Time Resolution', 
			choices = c(`Monthly` = 'monthly', `Annual` = 'annual'),
			multiple = FALSE), 

		### Input for site location
 		selectizeInput('site_name', 'Site Location', 
 			choices =  create_site_list(site_all, res = "monthly"),
    		selected = NULL,
    		multiple = FALSE,
    		options = list(
          		placeholder = 'Select site location.'
        	)
        	),     

		### Input for units
		selectizeInput('flow_units', 'Flow Units', 
			choices = c(`Mean m3/s` = 'm3/s', `Mean ft3/s` = 'cfs', `Total acre-ft` = 'ac-ft'),
			multiple = FALSE),
			

 
   		### Input for Date Subset appears if monthly is selected
		conditionalPanel(
		condition = "input.time_resolution == 'monthly'",
 		selectizeInput('time_subset', 'Date Subset', 
 			choices = c(`Full Timeseries` = '0', `January` = '1', `February` = '2', `March` = '3', `April` = '4', `May` = '5', `June` = '6', `July` = '7', `August` = '8', `September` = '9', `October` = '10', `November` = '11', `December` = '12' ),
 			multiple = FALSE)),  
             		

		### Input for Extreme Threshold appears once site name is selected
		conditionalPanel(
		condition = "input.site_name != ''",
 		
			hr(),
		h4("Extreme Threshold"),
		 # Less than or greater than input
  		selectInput("extreme_direction", "Threshold Direction",
    	choices = list("Less than (<)" = "lt", "Greater than (>)" = "gt"), 
    	selected = 1),
		
		### Input for threshold is calculated from server using percentiles
		uiOutput("extreme_flow")
		
)

    
    ),

###########################################################################
## Main panel
###########################################################################
    # Display results
    mainPanel(
     tabsetPanel(

###########################################################################
## Time Series Tab
###########################################################################     
        tabPanel("Overview", 
        	
        	h2("Reconstructed Time Series Overview"),
        	p("Please select a temporal resolution (Annual or Monthly) and a site on the left to view a reconstructed time series. For monthly reconstructions, it is possible to focus on particular months. These selections will populate the other tabs."),
        	br(),
        	dygraphOutput("tsPlot"),
        	br(),
        	helpText("Plots are dynamic. Click and drag within the time series to zoom or use the scroll bar at the bottom. Double-click on the graph to zoom out."),
        	hr(),
        	h2("Reconstruction Source"),
            ### Text information          
			h4(htmlOutput("recon_name_text")),
			h4(htmlOutput("recon_author_text")),
			h4(htmlOutput("recon_link_text")),
			h4(tags$strong("Recommended Citation:"), textOutput("recon_citation_text")),
			br(),
        	h2("Observation Source"),
            ### Text information   
            h4(htmlOutput("base_name_text")),
			h4(htmlOutput("base_author_text")),
			h4(htmlOutput("base_link_text"))		
        
        ),
        
###########################################################################
## Extremes Tab
###########################################################################           
        tabPanel("Extremes",  
        	DT::dataTableOutput("extreme_table"),
        	verbatimTextOutput('site_out')
        ), 

###########################################################################
## Period Comparison Tab
###########################################################################           
        tabPanel("Period Comparison",  
        	h2("Period Comparison"),
        	p("Coming Soon.")   
        ), 
                
###########################################################################
## Goodness of Fit Tab
###########################################################################          
        tabPanel("Goodness of Fit",       

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
        
        ),
        
###########################################################################
## About Tab
###########################################################################           
        tabPanel("About",  
        	h2("About"),
        	p("The Reconstructed Streamflow Explorer was developed by James Stagge, in conjunction with the Utah State University Water Lab. It was funded in part by Utah Mineral Lease funds"),
        	br(),
        	br(),
        	h3("Citation"),
        	p("When using the Reconstructed Streamflow Explorer for research or background, please cite as follows:"),
        	p("Stagge, J.H. (2017) Reconstructed Streamflow Explorer. www.com"),
        	br(),
            tags$p("All code for the Reconstructed Streamflow Explorer is available open source on GitHub. Feel free to contribute to this project there: ", tags$a(href="https://github.com/jstagge/paleo_flow_shiny", "https://github.com/jstagge/paleo_flow_shiny"), ".")	,
        	br(),
        	br(),
        	h3("Contact Information"),
        	tags$p("Please direct any questions to ", tags$a(href="mailto:james.stagge@usu.edu", "James Stagge"), ".")
        
        )         
      
      )
    )
  )
)

