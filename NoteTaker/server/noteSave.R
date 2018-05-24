



observeEvent(input$saveNote,{
  pripas("input$noteArea <- ",input$noteArea)
  
  a <- as.character(input$noteArea)
  b <- as.character(input$typeInput)
  

  if(!(b %in% rv$typeNow)){
    
    pripas("PickerInput changing")
    
    
    rv$typeNow <- c(as.character(rv$typeNow),b)
    rv$typePicked <- c(as.character(rv$typePicked),b)
    # pripas("rv$typeNow <- ",paste(as.character(rv$typeNow),collapse = ", "))
    # pripas("rv$typePicked <- ",paste(as.character(rv$typePicked),collapse = ", "))
    # 
    updatePickerInput(session, "typePicker", label = NULL, selected = rv$typePicked,
                      choices = rv$typeNow, 
                      choicesOpt = NULL)    
    
  }

  
  
  rv$noteData <- rbind(data.frame(time = Sys.time(), body = a, type= b),
                       rv$noteData)
  notedf <<- rv$noteData
  saveRDS(rv$noteData,paste0("solid/rds/",dfNow,".rds"))
  

   
  updateTextAreaInput(session, 'noteArea', label = NULL, value = "")
  
})

















