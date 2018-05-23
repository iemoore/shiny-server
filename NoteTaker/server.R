

server <- function(input, output, session) {
  
  
  
  source("server/keyResponse.R",local = T) 
  source("server/noteSave.R",local = T) 
  source("server/noteView.R",local = T) 
  
  session$onSessionEnded(function() {
    
    stopApp()
    
    # writeLines(paste(vls_cache,collapse = ","),
    #            con = paste0("userData/",global_user,"/savedVerbs.txt"))
    
    saveRDS(notedf,"solid/rds/noteMaster.rds")
    
    
  })


}