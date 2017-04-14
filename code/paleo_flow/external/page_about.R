###########################################################################
## About Tab
###########################################################################           

column(9,
	column(12,
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
