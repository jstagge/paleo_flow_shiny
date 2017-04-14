output$content2 <- renderUI({
  "Tab 2 content"
})


  output$plot <- renderPlot(function() {
    input$button1
    hist(rnorm(20))
  })