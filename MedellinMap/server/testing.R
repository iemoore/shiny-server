


# observeEvent(input$userPosition,{
#   
#   a <- input$userPosition$latitude
#   b <- input$userPosition$longitude
#   
#   print(paste(a,", ",b))
#   
# })'

onclick(".gps-button",print("Onclick gps detected"))
onclick("a.gps-button",print("Onclick gps 2 detected"))
onclick("#gps-button active",print("Onclick gps 3 detected"))
onclick(".gps-button active",print("Onclick gps 4 detected"))
onclick(".leaflet-control-gps",print("Onclick gps 2 detected"))

onclick(".leaflet-control-gps .gps-button.active",
        print(".leaflet-control-gps .gps-button.active"))
onclick(".leaflet-control-gps .gps-button",
        print(".leaflet-control-gps .gps-button"))

onclick(".search-button", print(".search-button clicked"))

observeEvent(input$lat1,{
  
  print(input$lat1)
  
})

observeEvent(input$gpscontrol,{
  
  print(input$gpscontrol)
  
})


# observeEvent(input$lat,{
#   
#   print(input$lat)
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
