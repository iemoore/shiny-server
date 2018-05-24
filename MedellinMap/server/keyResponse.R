


observeEvent(input$dirKeys, {
  
  a <- input$dirKeys[1]
  
  #...Left
  if(a==37){b <- "Left"}
  
  #...Up
  if(a==38){b <- "Up"}
  
  #...Right
  if(a==39){b <- "Right"}
  
  #...Down
  if(a==40){b <- "Down"}
  
  pripas("input$dirKeys <- ",b)
  
})



observeEvent(input$numKeys,{
  
  a <- input$numKeys[1]
  
  pripas("input$numKeys <- ",a)
  
  
  
})