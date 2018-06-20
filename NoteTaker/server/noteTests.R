

output$selectInput1 <- renderUI({
  
  selectizeInput(
    'typeInput', label = NULL, choices = isolate(rv$typeNow),
    # selected = isolate(rv$pickedNow),
    options = list(create = TRUE)
  )
    
})


output$pickerOut <- renderUI({
  
  pripas("output$pickerOut called at ",Sys.time())
  shinyjs::logjs(paste0("output$pickerOut called at ",Sys.time()))
  
  pickerInput(
    inputId = "typePicker", 
    label = "Choose desired types", 
    choices = as.character(rv$typeNow), 
    options = list(
      `actions-box` = TRUE, 
      size = 10,
      `selected-text-format` = "count > 3"
    ), 
    multiple = TRUE,
    selected = as.character(isolate(rv$typePicked))
  )
  
})


observeEvent(input$typePicker,{
  
  pripas("input$typePicker called at ",Sys.time())
  pripas("input$typePicker <- ",paste(input$typePicker,collapse = ", "))
  
  rv$typePicked <- input$typePicker
  
  
})



observeEvent(input$note_tbl_cell_edit, {


  info = input$note_tbl_cell_edit
  # pripas("input$note_tbl_cell_edit <- ",info$value)
  # str(info)
  i = info$row
  j = info$col
  v = as.character(info$value)
  
  pripas("Cell edit initiated v <-",v," i=",i," j=",j)
  
  rv$noteData[i, j] <- v
  notedf[i, j] <<- v
  saveRDS(rv$noteData,paste0("solid/rds/",dfNow,".rds"))
  # notedf[i, j] <<- DT::coerceValue(v, rv$noteData[i, j])
  replaceData(proxy1, rv$noteData, resetPaging = FALSE)  # important
})



observeEvent(input$note_tbl_row_click,{
  
  a <- input$note_tbl_row_click
  pripas("Row click <- ",a)
  
})























