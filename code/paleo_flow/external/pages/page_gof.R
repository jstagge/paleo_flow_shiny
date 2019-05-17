###########################################################################
## Goodness of Fit Tab
###########################################################################          

column(9,
	column(12,
	h2("Goodness of Fit"),

			### Add a goodness of fit warning
			fluidRow(
			conditionalPanel(
				condition="output.rwarn == 'warn'",
				column(6,HTML('<div class="alert alert-warning" role="alert"><h4>Caution: The reconstruction explains less than 50% of the variance in flow (R<sup>2</sup>).</h4><p> While this indicates a relatively poor calibration, it may be acceptable depending on your use case. Carefully consider how you will use the reconstruction and double-check the fit before proceeding.</p></div>')
			)),
			conditionalPanel(
				condition="output.nsewarn == 'warn'",
				column(6,HTML('<div class="alert alert-warning" role="alert"><h4>Caution: The Nash-Sutcliffe Efficiency (NSE) is less than 0. </h4><p>This means the reconstruction performs worse than if the model had assumed the historical average for all years. While this indicates a relatively poor calibration, it may be acceptable depending on your use case. Carefully consider how you will use the reconstruction and double-check the fit before proceeding.</p></div>'))
			)
			),

			### Add a loading notice
			#conditionalPanel(
			#	condition="$('html').hasClass('shiny-busy')",
			#	fluidRow(column(3, HTML('<div class="alert alert-info" role="alert">Data loading.</div>')))
			#),
   #       	h3("Observed vs. Reconstructed Flow"),
       		p("Reconstructed flows plotted against observed flows for the instrumental (calibration) period. A perfect calibration fit would be along the 1:1 line, shown in red."),
        	p("Hover over a point to see its date and values. Click and drag to zoom in on an area. Right click to zoom out."),
        	htmlOutput("gof_scatter"),

			fluidRow(
				column(5, 
					h3("Calibration"),
					tableOutput('cal_table'),
					bsCollapse(id = "collapse_cal", open = "Calibration Explained",
                 		bsCollapsePanel("Calibration Explained", "Calibration seeks to find the best predictor variables and calculates the model's ability to reproduce flows within calibration period, often the full observation record.",
br(),
br(),
"Calibration statistics indicate the best expected model performance and should be complemented with validation.
", style = "info")
					)
				), 
				column(4, 
					h3("Validation"),
					tableOutput('val_table'),
					bsCollapse(id = "collapse_val", open = "Validation Explained",
                 		bsCollapsePanel("Validation Explained", "Validation simulates the model's ability to predict flows outside the observed record. This is often done by fitting the model using part of the record and predicting the portion that was withheld.",br(),br(),"If validation is much worse than calibration, it indicates the model is too sensitive to new data, or 'overfit'", style = "info")
					)
				)	,
				column(3,
					br(),
					bsCollapse(id = "collapse_var", open = "RMSE (Root Mean Squared Error)",
                 		bsCollapsePanel("R^2 Variance Explained", "R-squared describes how much of the observed variance in reconstructed flow is accounted for by the predictors (tree-rings).", br(), br(), "It represents percent explained (%) and ranges from zero to one, with one being a perfect fit." , br(), br(), HTML('<a href="https://www.nap.edu/read/11676/chapter/12#92">Reconstruction-Statistical Background</a>'), style = "primary"), 
                 		bsCollapsePanel("RMSE (Root Mean Squared Error)", "RMSE provides the 'typical' error and is an absolute measure of fit, meaning it is displayed in flow units.", br(), br(), "RMSE is calculated as the square root of mean square of errors (predicted minus observed flow). A perfect model would have no errors, and therefore RMSE = 0.", br(), br(), HTML('<a href="https://www.nap.edu/read/11676/chapter/12#92">Reconstruction-Statistical Background</a>'), style = "primary"), 
                		bsCollapsePanel("NSE (Nash-Sutcliffe Efficiency)", "Nash-Sutcliffe Efficiency measures a model's predictive ability against the long term mean. NSE ranges from 1 (perfect fit) to -∞. Values greater than zero indicate that the model has some skill (performs better than assuming the mean).", br(), br(), "Within the reconstruction community, this metric is typically referred to as the Coefficient of Efficiency (CE) when applied to calibration and Reduction of Error (RE) when applied to validation.", br(), br(), HTML('<a href="https://www.adv-geosci.net/5/89/2005/adgeo-5-89-2005.pdf">Efficiency Criteria</a>'), style = "primary"),
                 		bsCollapsePanel("Mean Absolute Error", "Mean Absolute Error (MAE) calculates the average absolute (no sign) difference between predicted and observed flows. It is displayed in original flow units and a perfect fit have MAE = 0.", br(), br(), "MAE is similar in interpretation to RMSE and is the more intuitive of the two. Where RMSE handles negative values by squaring them, MAE simply applies the absolute value ||.", style = "primary"), 
                 		bsCollapsePanel("Mean Error", "Mean Error (ME) calculates the average error (predicted - observed). It is a measure of bias, if the model consistently over-predicts or under-predicts. Unbiased models have a ME near zero.", br(), br(), "Mean Error should always be considered with other metrics because poor predictive models can have ME near zero if large positive and negative errors can cancel each other out.", style = "primary"), 
                 		bsCollapsePanel("Validation Methods", "Validation is typically performed by splitting the observed record into a 'training' set and a 'validation' set. A model fit using the training set is then used to predict values in the witheld valdation set to simulate prediction errors outside the observed record.", br(), br(), "'Split Sample' validation defines these training and validation sets explicitly when fitting the model.", br(), br(), " 'K-fold' cross-validation predicts part of the sample (typically 10%, referred to as 'k') and predicts using the remaining 90%. This is repeated k times until each point is predicted without being used in training.", br(), br(), "Leave-One-Out is a commonly used method and is an extreme version of K-fold cross-validation, where each point is predicted using a model fit with all other data.", style = "primary")					)
					
				)
			),


        	h3("Comparison of Flow Distributions"),
        	plotOutput("gof_distr")  
	
	)
)
