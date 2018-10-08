shinyUI(navbarPageWithText(
  # Title
 #HTML('<div class="hidden-md hidden-sm hidden-xs"><h3 style="display:inline"><strong>PaleoFlow:</strong> Reconstructed Streamflow Explorer</h3></div><div class="hidden-xl hidden-lg"><h3 style="display:inline"><strong>PaleoFlow</strong></h3><p>Reconstructed Streamflow Explorer</p></div>'),
#HTML('<h3 style="display:inline"><strong>PaleoFlow</strong></h3><p >Reconstructed Streamflow Explorer</p>'),

  # Application theme
  theme = shinytheme("sandstone"),
  HTML('<a " href="#"><img src="./img/logo.png" width="100%" ></a>'),
	#position=c("fixed-top"),

	### Header
  	tags$head(
  	# Google Analytics script
  	includeScript("google-analytics.js"),
  	# This line loads the Google Charts JS library
  	#googleChartsInit(),
  	includeScript("www/dygraph-extra.js"),	
    tags$style(HTML("   "))
  ),
  ### Prevent error messages
  tags$style(type="text/css",
  ".shiny-output-error { visibility: hidden; }",
  ".shiny-output-error:before { visibility: hidden; }"
	),

  ### Longer drawdown menus
  tags$style(type="text/css",
   ".selectize-dropdown-content {max-height: 400px;}"
	),


  	selected="home",
  	tabPanel("Home", value="home"),
  	tabPanel("Extremes", value="extremes"),
  	tabPanel("Period Comparison", value="periods"),
  	tabPanel("Goodness of Fit", value="gof"),
  	tabPanel("Submit", value="submit"),
  	tabPanel("Background", value="background"),
	tabPanel("About", value="about"),
  	text = HTML('<div class="hidden-md hidden-sm hidden-xs"><div class="pull-right" style="padding-right:40px; padding-top:20px"><img src="./img/usu_horizontal_white.png" width="100%" ></div></div>'),
	windowTitle="Paleoflow",
	#collapsible=TRUE,
	id="nav_value",
	fluidRow(
		source("external/sidebar.R",local=T)$value,
		conditionalPanel(
			condition = "input.nav_value == 'home'",
			source("external/page_home.R",local=T)$value
		),
		conditionalPanel(
			condition = "input.nav_value == 'extremes'",
			source("external/page_extremes.R",local=T)$value
		),
		conditionalPanel(
			condition = "input.nav_value == 'periods'",
			source("external/page_periods.R",local=T)$value
		),		
		conditionalPanel(
			condition = "input.nav_value == 'gof'",
			source("external/page_gof.R",local=T)$value
		),
		conditionalPanel(
			condition = "input.nav_value == 'background'",
			source("external/page_background.R",local=T)$value
		),		
		conditionalPanel(
			condition = "input.nav_value == 'submit'",
			source("external/page_submit.R",local=T)$value
		),		
		conditionalPanel(
			condition = "input.nav_value == 'about'",
			source("external/page_about.R",local=T)$value
		)			
	)
	#footer=HTML("<div class = 'navbar navbar-fixed-bottom' style='line-height:30px; height:30px;'><div class = 'navbar-inner'><div class = 'container footer-margin-top'><p class = 'muted pull-left'>Created by hwhd</p> <p class = 'muted pull-right'>MyCompany  2014</p> </div></div> </div> ")
))