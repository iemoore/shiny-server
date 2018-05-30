


dataNow <- eventReactive(c(rv$lngRng,rv$latRng),{
  
  
  # pripas("DataNow activated at ",Sys.time())
  # pripas("rv$lngRng <- ",paste(rv$lngRng,collapse = ", "))
  # pripas("rv$latRng <- ",paste(rv$latRng,collapse = ", "))
  
  
  d1 <- filter(domic, lng<as.numeric(rv$lngRng[2]) & lng>as.numeric(rv$lngRng[1]) &
                 lat<as.numeric(rv$latRng[2]) & lat>as.numeric(rv$latRng[1]))
  
  
  # pripas("Dim dataNow() <- ",dim(d1)[1])
  
  if(length(d1[[1]])>1000){
    
    pripas("d1 >1000rows")
    d1 <- d1[1:1000,]
  }
  
  return(d1)
  
  
}) 


# observe({
#   data1 <- dataNow()
# 
#   leafletProxy("map", data = data1) %>%
#     clearMarkers() %>%
#     addMarkers(lng = data1$lng, lat=data1$lat)
# 
# 
# })


output$slidersUI <- renderUI({
  
  div(
    sliderInput("rangeLat", "Lat Range", round(range(domic$lat)[1],3),
                round(range(domic$lat)[2],3), value = range(rv$latRng), step = 0.02),
    sliderInput("rangeLng", "Lng Range", round(range(domic$lng)[1],3),
                round(range(domic$lng)[2],3), value = range(rv$lngRng), step = 0.02)
  )
  
})

observeEvent(input$rangeLat,{
  
  # pripas("input$rangeLat <- ",paste(input$rangeLat,collapse = ", "),
  #        " at ",Sys.time())
  rv$latRng <- input$rangeLat
  
})

observeEvent(input$rangeLng,{
  
  # pripas("input$rangeLng <- ",paste(input$rangeLng,collapse = ", "),
  #        " at ",Sys.time())
  rv$lngRng <- input$rangeLng
  
})