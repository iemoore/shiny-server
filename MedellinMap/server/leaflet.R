



output$map <- renderLeaflet({
  leaflet() %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%
    setView(lng = -75.583515, lat = 6.205090, zoom = 10) %>% 
    addMarkers(lng = -75.583515, lat = 6.205090) %>% 
    addControlFullScreen() %>% 
    addControlGPS()
  
}) 


# 
# observeEvent(input$geoloc, {
#   js$geoloc()
# })
















