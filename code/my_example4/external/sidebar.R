column(4,
	wellPanel(
		fluidRow(
			column(12, sliderInput("yrs", "Decades:", min=1900, max=2000, value=1990, step=10, sep="", post="s", width="100%"))
		),
#		fluidRow(
#			column(6, selectInput("mo", "Month:", choices=month.abb, selected=month.abb[1], width="100%")),
#			column(6, h1("a test"))
#		),
	br(), br()
)
)
