column(12,
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
fluidRow(
    column(12,
      h2("Submit a Reconstruction"),
       fluidRow(
        column(12,     
        	shinyjs::hidden(
          		div(
            	id = "thankyou_msg",
            	h3("Thanks, your reconstruction was submitted successfully!"),
            	actionLink("submit_another", "Submit another reconstruction")
          		)
        	),	
      		div(
          id = "form",
          HTML('<p>To submit a new reconstruction, first download the template (Step 1). Then upload the completed template (Step 2). Please include as much metadata in the CSV file as possible, along with entering a name for the reconstruction and a valid email address so that we may contact in case of questions (Step 3). Finally, push Submit (Step 4). </p> <p>We strongly recommend that the original files submitted to PaleoFlow be housed in a standard repository, such as the <a href="https://www.ncdc.noaa.gov/data-access/paleoclimatology-data/datasets/tree-ring">International Tree-Ring Data Bank (ITRDB)</a>.</p>'),

          fluidRow(
          	column(3,
          		br(),
          		HTML(
          			'<label class="control-label">Step 1:Download the Template</label>'
          		),
          		HTML('<div class="button">
    					<a href="/docs/paleoflow_sub_blank.csv" class="btn btn-primary active" download>
       						 <i class="glyphicon glyphicon-floppy-disk" aria-hidden="true"></i> Download Blank Template
   						 </a>
					</div>'),
				br(),
				HTML('<div class="button">
    					<a href="/docs/paleoflow_sub_example.csv" class="btn btn-info btn-sm" download>
       						 <i class="glyphicon glyphicon-floppy-disk" aria-hidden="true"></i> Example
   						 </a>
					</div>'),
					br(),
					br(),
				helpText("Download a blank template and insert data. All submissions must use the template provided.")	,				
				helpText("A completed file is provided as an example.")	
             	
             ), 
             column(3,
                 fileInput("upload", label = labelMandatory("Step 2: Upload CSV File"), 
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")),
             helpText("Upload the completed file. Header information is critical and no reconstructions will be posted without proper attribution (creator, citation, etc) or gauge information.")         	
              
              ),
              column(3,
              		br(),
              		HTML('<span style="white-space:nowrap"><label class="control-label">Step 3: Add Metadata</label></span>'), 
              		HTML('<span style="white-space:nowrap">'),
          			textAreaInput("recon_name", label = labelMandatory("Name of Reconstruction :"), placeholder = "Name", width="300px", height="40px"),
          			HTML('</span>'),
          	   		textInput("user_email", label=labelMandatory("Email"), value="", placeholder="placeholder" ),
          	   		textAreaInput("user_notes","Additional notes to reviewers:", placeholder = "Optional", height="40px", width="250px")
          	   
          	   ), 
          	  column(2, 
          	 		br(),
          	 		HTML('<label class="control-label"; display:inline >Step 4: Submit</label>'),
          			br(),
          			actionButton("submit", "Submit", class = "btn-success"),
                	shinyjs::hidden(
            			span(id = "submit_msg", "Submitting..."),
            			div(id = "error", div(br(), tags$b("Error: "), span(id = "error_msg")))
          			),
					br(),
					br(),
				helpText("Push submit when finished. You will receive confirmation on the next screen.")
          	  
          	  )
             	
           )  ### Closing the fluid row
          )  ### Closing the div for the form
     ) ### Closing the 12 column
     ) ### Closing the fluid row
) ### Closing the 12 column
) ### Closing the fluid row
) ### Closing the 12 column
