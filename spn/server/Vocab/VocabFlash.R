
rv$choose1 <- ""
rv$choose2 <- ""
rv$nextCt <- 0
rv$exclude <- 0


#...SelectInput, Buttons
#####------------------------------------------------------------------

output$theme1Select <- renderUI({
  if (USER$Logged == TRUE) {
  
  selectInput('choosetheme1', 'Choose Theme...',
              # c(Choose='', Abatir="1",Abrasar="2"),
              c(Choose='', unique(vocabdf$theme1)),
              selectize=TRUE)
  } 
})

output$theme2Select <- renderUI({
  if (USER$Logged == TRUE) {
  
    a <- as.character(rv$choose1)
    
    if(nchar(a)>1) { 
      
      b <- unique(filter(vocabdf,theme1==rv$choose1)$theme2)
      
      selectInput('choosetheme2', 'Choose Sub-Theme...',
                  c(Choose='', unique(b)),
                  selectize=TRUE)
    }else{br()}

  }  
})

output$flashNext <- renderUI({
  if (USER$Logged == TRUE) {
  
  a <- as.character(rv$choose2)
  
  if(nchar(a)>1) {
    
    isolate(actionButton("nextF","Next")) 
    
  }
 
  } 
})

output$flashShow <- renderUI({
  if (USER$Logged == TRUE) {
  
  a <- as.character(rv$choose2)
  
  if(nchar(a)>1) {
    
    isolate(actionButton("showF","Show")) 
    
  }
 
  } 
})

#####



#...observeEvents
#####------------------------------------------------------------------

observeEvent(input$hideSide2,{
  
  #print("input$hideSide clicked")
  shinyjs::toggle("Sidebar2")
  
})


observeEvent(input$choosetheme1, {
  
  rv$choose1 <- input$choosetheme1
  
})


observeEvent(input$choosetheme2, {
  
  #print(paste0("rv$exclude1 reset",paste(rv$exclude1,collapse = ",")))
  rv$choose2 <- input$choosetheme2
  
})


observeEvent(input$nextF,{
  
  rv$nextCt <- rv$nextCt + 1
  shinyjs::hide(id= "hideF")
  
})


observeEvent(rv$choose2,{
  
  rv$exclude <- 0
})


observeEvent(input$showF, {
  
  #print(paste0("Trying to toggle hideF"))
  shinyjs::toggle(id= "hideF")
  
})

#####



#...Dynamic UI 
#####------------------------------------------------------------------

observeEvent(c(rv$choose2,rv$nextCt), {
  
  #print("Flashcard UI called")
  removeUI("#flashUI")
  a1 <- rv$choose1
  a2 <- rv$choose2
  #a1<-"MiniList";a2<-"Colors"
  
  #print(paste0("rv$choose2 = ",rv$choose2))
  
  if(nchar(a2)>1){
    
    #print(nchar(a2))
    
    b <- filter(vocabdf,theme1==a1,theme2==a2)[,c("eng","spn")]
    
      if(length(rv$exclude)<dim(b)[1]){
        
        rNum <- sample(rownames(b)[!(rownames(b) %in% rv$exclude)],1)
        
        rv$exclude <- c(rv$exclude,rNum)
        #print(paste0("rv$exclude = ",paste(rv$exclude,collapse = ", ")))
        
        f <- b[rNum,]  
        
        # print(paste0("f has ",dim(f)[1]," item"))
        
        # ui_build <- div(
        #   id= "flashUI",
        #   wellPanel(id="flashPanel",
        #             
        #             h3(f[1,2])
        #   ),
        #   wellPanel(id="flashPanel2",
        #   shinyjs::hidden(div(id = "hideF",
        #            
        #           h3(f[1,1])
        #     ))
        #   )
        # )      
        ui_build1 <- div(id= "flashUI",h3(f[1,2]))
        
        ui_build2 <- div(id= "flashUI",
                         shinyjs::hidden(div(id = "hideF",h3(f[1,1])))
        )
        
      }
      else {
        
        ui_build1 <- tags$div(id= "flashUI", 
                              h2("Has agotado este conjunto de vocabulario")
        )
        
        ui_build2 <- tags$div(id= "flashUI", 
                              h2("You have exhausted this Vocabulary Set")
        )
        
        
      }
      
      # insertUI(
      #   selector = "#here4",
      #   where = "afterEnd",
      #   ui = ui_build
      # )
    
      insertUI(
        selector = "#hereVo1",
        where = "afterEnd",
        ui = ui_build1
      )
      
      insertUI(
        selector = "#hereVo2",
        where = "afterEnd",
        ui = ui_build2
      )
    
  }  
  
  
})



#####











