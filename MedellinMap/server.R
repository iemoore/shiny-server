

server <- function(input, output, session) {
  
  
  
  source("server/keyResponse.R",local = T)
  source("server/leaflet.R",local = T)
  source("server/testing.R",local = T)
  
  
  session$onSessionEnded(function() {
    
    stopApp()
    
    
  })

}