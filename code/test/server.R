function(input, output, session) {
#  observeEvent(input$add, {
   
  datasetInput <- reactive({
   switch(input$dataset,
          "rock" = rock,
          "pressure" = pressure,
          "cars" = cars)
})

output$nrows <- reactive({
  nrow(datasetInput())
})
}