shinyUI(navbarPageWithText(theme=shinytheme("sandstone"),
  	"Paleostreamflow Reconstruction",
  	tabPanel("Community Charts", value="commChart"),
  	tabPanel("About", value="about"),
  	text = HTML('<br><div class="pull-right" style="padding-right:40px; padding-top:20px"><img src="./img/usu_horizontal_white.png" width="100%" ></div>'),
	windowTitle="Paleoflow",
	collapsible=FALSE,
	fluidRow(
		source("external/sidebar.R",local=T)$value,
		source("external/main.R",local=T)$value
	)
))