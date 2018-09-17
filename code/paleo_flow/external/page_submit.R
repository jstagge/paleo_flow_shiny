column(12,
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
fluidRow(
    column(12,
      h2("Submit a Reconstruction"),
       fluidRow(
        column(10,     
        	shinyjs::hidden(
          		div(
            	id = "thankyou_msg",
            	h3("Thanks, your reconstruction was submitted successfully!"),
            	actionLink("submit_another", "Submit another reconstruction")
          		)
        	),	
      		div(
          id = "form",
          HTML('<p>To submit, please enter a name for the reconstruction and a valid email address so that we may contact in case of questions. Then complete the data upload process using the attached template. We strongly recommend that all files submitted here also be housed at NOAA <a href="https://www.ncdc.noaa.gov/data-access/paleoclimatology-data/datasets/tree-ring">International Tree-Ring Data Bank (ITRDB)</a>.</p>'),
          fluidRow(
            column(7, 
          		textAreaInput("recon_name", label = labelMandatory("Name for Reconstruction:"), placeholder = "Name", width="600px", height="50px")
          	),
            column(5,
              textInput("user_email", label=labelMandatory("Email"), value="", placeholder="placeholder" )
			)
          ),
          fluidRow(
          	column(12,
          		textAreaInput("user_notes","Additional notes to PaleoFlow reviewers:", placeholder = "Optional", height="40px", width="800px")
          	)
          	#HTML('<div class="col-xs-12" style="height:30px;"></div>')
                
          ),
          fluidRow(
          	column(4,
          		br(),
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
                 fileInput("upload", label = labelMandatory("Step 2: Upload CSV File"), 
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")),
             helpText("Upload the completed file. Header information is critical and no reconstructions will be posted without proper attribution (creator, citation, etc) or gauge information.")         	
              
              ),
              column(4,
              	br(),
              	HTML(
          			'<label class="control-label">Step 3: Submit Reconstruction</label>'
          		),
          		br(),
          		actionButton("submit", "Submit", class = "btn-primary"),
                shinyjs::hidden(
            		span(id = "submit_msg", "Submitting..."),
            		div(id = "error",
                	div(br(), tags$b("Error: "), span(id = "error_msg"))
            		)
          		)
        	  )
           )  ### Closing the fluid row
        	
          )  ### Closing the div for the form
        ),   ### Closing the 10 column
        column(2,
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
       ) ### Closing the fluid row
     ) ### Closing the 12 column
) ### Closing the fluid row
) ### Closing the 12 column
