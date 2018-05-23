


observeEvent(input$saveNote,{
  
  
  a <- input$noteArea
  
  pripas("input$noteArea <- ",input$noteArea)
  
  
  rv$noteData <- rbind(data.frame(time = Sys.time(), body = a),rv$noteData)
  notedf <<- rv$noteData
  
  pripas("length(rv$noteData) <- ",dim(rv$noteData)[1])
  pripas("length(notedf) <- ",dim(notedf)[1])
   
  updateTextAreaInput(session, 'noteArea', label = NULL, value = "")
  
})

















