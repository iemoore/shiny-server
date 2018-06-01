

server <- function(input, output, session) {
  
  
  
  source("server/keyResponse.R",local = T)
  source("server/leaflet.R",local = T)
  source("server/mapInfo.R",local = T)
  source("server/sliders.R",local = T)
  source("server/testing.R",local = T)
  source("server/functions.R",local = T)
  source("server/loadMap.R",local = T)  
  
  session$onSessionEnded(function() {
    
    stopApp()
    
    
  })

}