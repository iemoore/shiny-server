



observeEvent(input$shift_key,{ 
  
  data1 <- domic
  b <- input$map_bounds
  z <- input$map_zoom
  
  pripas("Map bounds <- ",paste(b,collapse = ", ")," and zoom <- ",z)
  
  data1 <- filter(domic, lat<b[1] & lng<b[2] & lat>b[3] & lng>b[4])
  
  pripas("dim(data1)[1] <- ",dim(data1)[1])
 
  leafletProxy("map", data = data1) %>%
    clearMarkers() %>%
    addMarkers(lng = data1$lng, lat=data1$lat, popup = ~paste(VIA,", ",PLACA))


})



observeEvent(input$numKeys,{
  
  a <- input$numKeys[1]
  
  if(a==1){
    
    b <- filter(barrios, name=="Cristo Rey")
    
    c1 <- str_split_fixed(un_sp(b$poly," "),",",2)
    
    leafletProxy("map") %>%
      addPolylines(lng=as.numeric(c1[,1]),lat=as.numeric(c1[,2]),
                   group = "line",color = "black")
    
  }

  if(a==2){
    
    b <- barrios#[200:320,]
    
    df1 <- dftoSPDF(b)
    
    leafletProxy("map",data = df1) %>%
      addPolygons(group = "line",color = "black")
      
    
  }
  
  
  if(a==3){
    
    b <- c(6.21125279766647, -75.5956792831421, 6.19700302276826, -75.6224155426026)
    b <- input$map_bounds
    
    z <- input$map_zoom
    
    pripas("bounds <- ",paste(b,collapse = ", "))
    
    data1 <- filter(usos, latmean<b[1] & lngmean<b[2] & latmean>b[3] & lngmean>b[4])
    
    pripas("dim(data1)[1] <- ",dim(data1)[1])
    
    df1 <- dftoSPDF(data1)
    
    leafletProxy("map",data = df1) %>%
      addPolygons(group = "line",color = "black")
    
    
  }
  
  if(a==4){
    
    
    leafletProxy("map") %>%
      addRectangles(
        lng1=min(usos$lngmean), lat1=max(usos$latmean),
        lng2=max(usos$lngmean), lat2=min(usos$latmean),
        fillColor = "transparent"
      )
  
  }
  
  
})



















