

###########################################################################
## Submit Tab
###########################################################################           

column(9,
	column(12,fluidRow(h2("Submit"))),
	
	column(7,
		fluidRow(
			tags$p("To submit"), 
			textInput("name", label = "Name:", placeholder = "Name"), 
			 textInput("inst","Institution:"),
                textInput("notes","Notes:", placeholder = ""),
                fileInput("userdata", label = "Choose CSV File", 
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")),
                actionButton("submitdata",label = "Submit Data")
		),
       	fluidRow(
       		h3("Contact Information")
		)		
		
	), 
	column(3, offset=1,
			fluidRow(
			tags$div(img(src = "img/Utah_State_University_Logo.png", class="img-responsive")),
			br()
			),
			fluidRow(
			tags$div(img(src = "img/uwrllogo.gif", class="img-responsive")),
			br()
			), 
			fluidRow(
			tags$div(img(src = "img/wadr.jpg", class="img-responsive")),
			br()
			)
	)
    	
)		

