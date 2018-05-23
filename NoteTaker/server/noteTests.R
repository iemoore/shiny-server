

output$selectInput1 <- renderUI({
  
  selectizeInput(
    'typeInput', label = NULL, choices = isolate(rv$typeNow),
    # selected = isolate(rv$pickedNow),
    options = list(create = TRUE)
  )
    
})

# observeEvent(input$typeInput,{
#   
#   
#   rv$pickedNow <- input$typeInput
#   
# })



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
    selected = as.character(rv$typePicked)
  )
  
})



observeEvent(input$typePicker,{
  
  pripas("input$typePicker called at ",Sys.time())
  pripas("input$typePicker <- ",paste(input$typePicker,collapse = ", "))
  
  rv$typePicked <- input$typePicker
  
  
})















