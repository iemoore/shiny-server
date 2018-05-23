

server <- function(input, output, session) {
  
  session$allowReconnect(TRUE)
  
  
  source("server/keyResponse.R",local = T) 
  source("server/noteSave.R",local = T) 
  source("server/noteView.R",local = T) 
  source("server/noteTests.R",local = T) 
  
  session$onSessionEnded(function() {
    
    stopApp()
    
    # writeLines(paste(vls_cache,collapse = ","),
    #            con = paste0("userData/",global_user,"/savedVerbs.txt"))
    
    saveRDS(notedf,"solid/rds/noteMaster1.rds")
    
    
  })
  
  
  rv$noteData <- readRDS("solid/rds/noteMaster1.rds")
  
  # a <- readRDS("solid/rds/noteMaster1.rds")
  rv$typeNow <- unique(readRDS("solid/rds/noteMaster1.rds")$type)
  rv$typePicked <- unique(readRDS("solid/rds/noteMaster1.rds")$type)


}