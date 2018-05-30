


output$map <- renderLeaflet({
  leaflet() %>%
    # addTiles(
    #   urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    #   attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    # ) %>%
    addTiles(
      urlTemplate = paste0('http://server.arcgisonline.com/ArcGIS/rest/services/',
                           'World_Imagery/MapServer/tile/{z}/{y}/{x}'),
      group = "Sat",options =tileOptions(minZoom=8)) %>%
    addProviderTiles("OpenStreetMap.DE",
                     options =providerTileOptions(#opacity = 0.8,
                       noWrap = TRUE,reuseTiles = TRUE),
                     group = "Roads") %>%
    addTiles(urlTemplate = paste0('http://server.arcgisonline.com/ArcGIS/rest/',
                            'services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}'),
             group = "Topo",options =tileOptions(minZoom=10))  %>%
    addLayersControl(baseGroups = c("Roads","Topo","Sat"),
                     position = "topright") %>% 
    setView(lng = -75.583515, lat = 6.205090, zoom = 14) %>% 
    addMarkers(lng = -75.583515, lat = 6.205090) %>% 
    addFullscreenControl() %>%
    # addSearchOSM() %>%
    addControlGPS() #%>% 
    # addMarkers(lng = d0$lng, lat = d0$lat)
    
}) 













































