



observeEvent(input$typePicker,{
  
  pripas("input$typePicker called at ",Sys.time())
  pripas("input$typePicker <- ",paste(input$typePicker,collapse = ", "))
  
  rv$typePicked <- input$typePicker
  
  
})















