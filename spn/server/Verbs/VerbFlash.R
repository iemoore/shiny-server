
rv$nextCt3 <- 0
rv$exclude3 <- 0
t4 <- c("Essential 501","Saved","All")
rv$start3 <- 1
rv$verbData <- data.frame(NULL)
rv$choose3 <- ""


#...SelectInput, Buttons
#####------------------------------------------------------------------

output$verbType <- renderUI({
  if (USER$Logged == TRUE) {
  
  selectInput('chooseType', 'Choose Verb Type...',
              choices = c('', t4),selected = "Essential 501",
              selectize=TRUE,multiple = F)
  
  }
})


output$flashNext3 <- renderUI({
  if (USER$Logged == TRUE) {
  
  a <- rv$choose3 
  
  if(nchar(a)>0) {
    
    isolate(actionButton("nextF3","Next")) 
    
  }
  
  }
})

output$flashShow3 <- renderUI({
  if (USER$Logged == TRUE) {
  
  a <- rv$choose3 
  
  if(nchar(a)>0) {
    
    isolate(actionButton("showF3","Show")) 
    
  }
  
  }
})

#####



#...observeEvents
#####------------------------------------------------------------------

observeEvent(input$hideSide3,{
  
  #print("input$hideSide3 clicked")
  shinyjs::toggle("Sidebar3")
  
})


observeEvent(input$chooseType, {
  
  #print(paste0("rv$exclude1 reset",paste(rv$exclude1,collapse = ",")))
  rv$choose3 <- input$chooseType
  
})


observeEvent(input$chooseType,{
  
  a <- as.character(input$chooseType)
  t4 <- c("Essential 501","Saved","All")
  
  # if(rv$start3>0){
    
    if(a==t4[1]) {
      
      rv$verbData <- verbdf

    } 
    
    if(a==t4[2]){
       
      rv$verbData <- rv$vls_data
      
    }
    
    if(a==t4[3]){
      
      b <- filter(vocabdf, type=="verb")[,c(5,4)]
      b2 <- verbdf
      b2$verb <- tolower(b2$verb)
      colnames(b) <- colnames(b2)
      b3 <- rbind(b,b2)
      b3$verb <- as.character(b3$verb)
      b4 <- b3[!duplicated(b3$verb),]
      b5 <- b4[order(b4[,1]),]
      
      rv$verbData <- b5
      
    }
    
  # }
  
  rv$start3 <- rv$start3 + 1
})


observeEvent(input$nextF3,{
  
  pripas("Next clicked")
  rv$nextCt3 <- rv$nextCt3 + 1
  shinyjs::hide(id= "hideF3")
  
})


observeEvent(rv$start3,{
  
  rv$exclude3 <- 0
})


observeEvent(input$showF3, {
  
  #print(paste0("Trying to toggle hideF"))
  shinyjs::toggle(id= "hideF3")
  
})


#####



#...Dynamic UI
#####------------------------------------------------------------------

observeEvent(c(rv$start3,rv$nextCt3), {
  
  if(rv$start3>1){
    
    removeUI("#flashUI1")
    removeUI("#flashUI2")  
    removeUI("#flashUI3")
    removeUI("#flashUI4")
    a3 <- input[["chooseType"]]
    
    pripas("a3: chooseType <- ",a3," rv$start3 <- ",rv$start3)
    # a3 <- c("Abatir","Tener","Zumbar","Dar","Fabricar")
    
    
    if(!is.null(a3)){
      
      #pripas("a3 = ",paste(a3,collapse = ", "))
      
      
      b <- rv$verbData
      
      
      #pripas("length b = ",dim(b)[1])
      
      if(length(rv$exclude3)<dim(b)[1]){
        
        #pripas("length(rv$exclude3)<dim(b)[1]")
        
        rNum <- sample(rownames(b)[!(rownames(b) %in% rv$exclude3)],1)
        
        rv$exclude3 <- c(rv$exclude3,rNum)
        #print(paste0("rv$exclude3 = ",paste(rv$exclude3,collapse = ", ")))
        
        f <- b[rNum,]  
        
        # print(paste0("f has ",dim(f)[1]," item"))
        
        ui_build1 <- div(id= "flashUI1",h3(f[1,1]))
        
        ui_build2 <- div(id= "flashUI2",
                         shinyjs::hidden(div(id = "hideF3",h3(f[1,2])))
                        )
        
        
      }
      else {
        
        ui_build1 <- tags$div(id= "flashUI3", 
                              h2("Has agotado esta Lista de verbos")
        )
        
        ui_build2 <- tags$div(id= "flashUI4", 
                              h2("You have exhausted this Verb List")
        )
        
        
      }
      

      
      insertUI(
        selector = "#hereVe1",
        where = "afterEnd",
        ui = ui_build1
      )
      
      insertUI(
        selector = "#hereVe2",
        where = "afterEnd",
        ui = ui_build2
      )
      
    } 
    
  }
})

#####