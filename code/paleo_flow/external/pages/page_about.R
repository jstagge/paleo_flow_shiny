###########################################################################
## About Tab
###########################################################################           

column(12,
	column(12,offset=1, fluidRow(h2("About"))),
	
	column(5, offset=1,
		fluidRow(
			tags$p("The PaleoFlow website was developed and is maintained by ", tags$a(href="www.jstagge.com", "James Stagge"), "in conjunction with the ", tags$a(href="https://ceg.osu.edu/", "Ohio State University"), ", the ", tags$a(href="https://uwrl.usu.edu/", "Utah State University Water Research Lab"), " and the ", tags$a(href="https://wadr.usu.edu/", "Wasatch Dendroclimatology Research Group") ,". It was funded in part by Utah Mineral Lease funds.")
		),
       	fluidRow(
       		h3("Citation"),
       		p("When using the Reconstructed Streamflow Explorer for research or reference, please cite as follows:")
       	),
       	fluidRow(
       		strong("Stagge, J.H. (2017) PaleoFlow Reconstructed Streamflow Explorer Version 2.1.0. www.paleoflow.org  doi:10.5281/zenodo.583166"),
       		
			tags$div(tags$a(href="https://github.com/jstagge/paleo_flow_shiny",  img(src="https://zenodo.org/badge/DOI/10.5281/zenodo.583166.svg", alt="DOI"))),
		 	br()
		 ),
		 fluidRow(
		 br(),
		   tags$p("All code for this application is available as a", tags$a(href="https://github.com/jstagge/paleo_flow_shiny", "GitHub repository"), ", made available under the MIT license. The repository is actively maintained, so we accept feature requests and code contributions submitted as pull requests.")
		 ),
       	fluidRow(
       		h3("Contact Information"),
        	tags$p("Please direct any questions to ", tags$a(href="mailto:stagge.11@osu.edu", "James Stagge"), ".")
		)		
		
	), 
	column(3, offset=3,
			fluidRow(
			tags$div(img(src = "img/TheOhioStateUniversity-Stacked-RGBHEX.png", class="img-responsive", style="padding: 10px")),
			br()
			),
			fluidRow(
			tags$div(img(src = "img/Utah_State_University_Logo.png", class="img-responsive", style="padding: 10px")),
			br()
			),
			#fluidRow(
			#tags$div(img(src = "img/uwrllogo.gif", class="img-responsive")),
			#br()
			#), 
			fluidRow(
			tags$div(img(src = "img/wadr.jpg", class="img-responsive", style="padding: 10px")),
			br()
			)
	)
    	
)		

