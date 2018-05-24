


# observeEvent(input$userPosition,{
#   
#   a <- input$userPosition$latitude
#   b <- input$userPosition$longitude
#   
#   print(paste(a,", ",b))
#   
# })


output$lat <- renderPrint({
  input$lat
})

output$long <- renderPrint({
  input$long
})

output$geolocation <- renderPrint({
  input$geolocation
})

output$accuracy <- renderPrint({
  input$accuracy
})

output$time <- renderPrint({
  input$time
})
