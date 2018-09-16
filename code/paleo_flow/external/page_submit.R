column(12,
fluidRow(
    column(12,
      h2("Submit a Reconstruction"),
      fluidRow(
        column(10,
          tags$p("To submit, please enter a name for the reconstruction and a valid email address so that we may contact in case of questions. Then complete the data upload process using the attached template."),
          fluidRow(
            column(7, 
              textAreaInput("recon_name", label = "Name for Reconstruction:", placeholder = "Name", width="600px")
            ),
            column(5,
              textInput("user_email","Email address:", placeholder = "Email")
            )
          ),
          fluidRow(
          	column(12,
          		textAreaInput("user_notes","Additional notes to PaleoFlow reviewers:", placeholder = "Optional", height="40px", width="800px")
          	),
          	HTML('<div class="col-xs-12" style="height:30px;"></div>')
                
          ),
          fluidRow(
          	column(4,
          		HTML(
          			'<label class="control-label">Step 1:Download the Template</label>'
          		),
          		HTML('<div class="button">
    					<a href="/docs/raw_data.csv" class="btn btn-primary active" download>
       						 <i class="glyphicon glyphicon-floppy-disk" aria-hidden="true"></i> Download Template
   						 </a>
					</div>'),
					br(),
					br(),
				helpText("Download a blank form and insert data. All submissions must use the template provided.")
             	
             ), 
             column(4,
             	
               	fileInput("userdata", label="Step 2: Upload CSV File", 
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")),
                 helpText("Upload the completed file. Header information is critical and no reconstructions will be posted without proper attribution (creator, citation, etc) or gauge information.")         	
              
              ),
              column(4,
          		HTML(
          			'<label class="control-label">Step 3: Submit Reconstruction</label>'
          		),
          		br(),
                actionButton("submitdata",label = "Submit", class="btn-success")
          	
            	)
          )         
          
        ),
        column(width = 2,
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
    )
  )
 )