function(input, output, session) {

	output$myresults <- renderText({
		mythingy <- a_test(a=3, b=6)
  		print(mythingy)
	})
	
}