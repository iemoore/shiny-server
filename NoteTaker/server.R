

server <- function(input, output, session) {
  
  session$allowReconnect(TRUE)
  
  rv$noteData <- readRDS("solid/rds/noteMaster.rds")
  # a <- readRDS("solid/rds/noteMaster1.rds")
  # notedf <<- rv$noteData
  rv$typeNow <- as.character(unique(readRDS(paste0("solid/rds/",dfNow,".rds"))$type))
  rv$typePicked <- as.character(unique(readRDS(paste0("solid/rds/",dfNow,".rds"))$type))
  rv$pickedNow <- "Spanish"
  
  source("server/keyResponse.R",local = T) 
  source("server/noteSave.R",local = T) 
  source("server/noteView.R",local = T) 
  source("server/noteTests.R",local = T) 
  
  session$onSessionEnded(function() {
    
    stopApp()
    
    Encoding(notedf$body) <-"UTF-8"
    saveRDS(notedf,paste0("solid/rds/",dfNow,".rds"))
    
    
  })
  
  



}