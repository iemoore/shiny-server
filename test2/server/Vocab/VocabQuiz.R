

#...Basic Reactive Values 
#####------------------------------------------------------------------

rv$choose1_Vo <- ""

uiTrigger_Vo <- makeReactiveTrigger()
rv$showQuiz_Vo <- ""
rv$excludeQ_Vo <- 0
rv$nextCt4_Vo <- 0
rv$addCt_Vo <- 0
rv$answerNow_Vo <- ""
rv$answerCheck_Vo <- ""
rv$data_Vo <- data.frame(NULL)


rv$checkNowNum_Vo <- 0
rv$numWrong_Vo <- 0
rv$numRight_Vo <- 0

rv$showTheme1_Vo <- ""
rv$showTheme2_Vo <- ""

#####


#...Render UI needed for sentence quiz
#####------------------------------------------------------------------

output$theme1Select_Vo <- renderUI({
  if (USER$Logged == TRUE) {
  
  selectInput('choosetheme1_Vo', 'Choose Theme...',
              # c(Choose='', Abatir="1",Abrasar="2"),
              c(Choose='', unique(vocabdf$theme1)),
              selectize=TRUE)
  }
})


output$theme2Select_Vo <- renderUI({
  if (USER$Logged == TRUE) {
  
    a <- as.character(rv$showTheme1_Vo)
    
    if(nchar(a)>1) { 
      
      # b <- unique(filter(vocabdf,theme1==rv$choose1V)$theme2)
      
      # selectInput('choosetheme2V', 'Choose Sub-Theme...',
      #             c(Choose='', unique(b)),
      #             selectize=TRUE)
      
      a <- count(filter(vocabdf,theme1==rv$showTheme1_Vo),theme2)  
      
      selectInput('choosetheme2_Vo', label = NULL,
                  c(Choose='', paste0(a$theme2,"   [",a$n,"]")),
                  selectize=TRUE)
      
    }
    else{br()}

  }  
})


output$buttons_Vo <- renderUI({
  if (USER$Logged == TRUE) {
    div(
      actionButton(inputId = "add_VoQ", label = "Start"),
      actionButton(inputId = "rem_VoQ", label = "Reset")
    )
  }
})


output$quizShow_Vo <- renderUI({
  if (USER$Logged == TRUE) {
  
  a <- rv$addCt_Vo
  
  if(a>0) {isolate(actionButton("check_Vo","Check"))}
 
  } 
})


output$quizNext_Vo <- renderUI({
  if (USER$Logged == TRUE) {
  
  a <- rv$addCt_Vo
  
  if(a>0) {isolate(actionButton("next_Vo","Next"))}
  
  }
})


output$sentScore_Vo <- renderUI({
  if (USER$Logged == TRUE) {
    
    div(
      inlineDing(wellPanel(id="flashPanel",column(h2("Correct"),
                                                  h3(rv$numRight_Vo),width = 12))),
      inlineDing(wellPanel(id="flashPanel",column(h2("Incorrect"),
                                                  h3(rv$numWrong_Vo),width = 12)))
    )
    
    # div(
    # wellPanel(h2("Correct")),
    # wellPanel(h2("Incorrect"))
    # )
  }
})
#####


#...Observe inputs for sentence quiz: chooseVerb, rem, add
#####------------------------------------------------------------------

observeEvent(input$choosetheme1_Vo, {
  
  pripas("input$theme1Select_Vo <- ",input$choosetheme1_Vo)
  
  rv$showTheme1_Vo <- as.character(input$choosetheme1_Vo)
  
})


observeEvent(input$choosetheme2_Vo, {
  
  pripas("input$theme2Select_Vo <- ",input$choosetheme2_Vo)
  
  
  rv$showTheme2_Vo <- as.character(input$choosetheme2_Vo)
  
  
  uiTrigger_Vo$trigger()
  
})


observeEvent(input$add_VoQ,{
  
  rv$addCt_Vo <- rv$addCt_Vo + 1 
  uiTrigger_Vo$trigger()
  
  pripas("input$add_Vo clicked at ",Sys.time())
})


observeEvent(input$check_Vo,{
  
  rv$answerCheck_Vo <-  input$quizAnswer_Vo
  #pripas("input$quizAnswer <- ",input$quizAnswer)
  
  
})


observeEvent(input$next_Vo,{
  
  rv$nextCt4_Vo <- rv$nextCt4_Vo + 1
  #print(rv$nextCt4)
})


observeEvent(input$rem_Vo, {
  
  rv$reset_Vo <- rv$reset_Vo + 1
  rv$addCt_Vo <- 0
  rv$numWrong_Vo <- 0
  rv$numRight_Vo <- 0
  removeUI("#quizUI1_Vo")
  removeUI("#quizUI2_Vo")
  removeUI("#quizUI3_Vo")
  removeUI("#quizUI4_Vo")
})



#####


#...Dynamic UI rendering of sentences when input$add is clicked
#####------------------------------------------------------------------

observeEvent(c(rv$addCt_Vo,rv$nextCt4_Vo) ,{ 
  
  pripas("rv$nextCt4_Vo <- ",rv$nextCt4_Vo)  
  
  
  pripas("Rebuilding UI at ",Sys.time())
  uiTrigger_Vo$depend()
  # b <- isolate(rv$data_Vo)
  
  c <- un_sp(rv$showTheme2_Vo,"   \\[")[1]
  pripas("rv$showTheme1_Vo <- ",rv$showTheme1_Vo,
         " ; rv$showTheme2_Vo <- ",c)
  
  
  b <- filter(vocabdf, theme1==rv$showTheme1_Vo,
                             theme2==c)
  
  # b$eng <- as.character(b$eng)  
  # b$spn <- as.character(b$spn)
  pripas("dim(mydata)[1] <- ",dim(b)[1])
  
  
  
  removeUI("#quizUI1_Vo")
  removeUI("#quizUI2_Vo")
  removeUI("#quizUI3_Vo")
  removeUI("#quizUI4_Vo")
  
  
  if(dim(b)[1]>0){
    
    if(length(rv$excludeQ_Vo)<dim(b)[1]){
      
      
      rNum <- sample(rownames(b)[!(rownames(b) %in% rv$excludeQ_Vo)],1)
      rv$excludeQ_Vo <- c(rv$excludeQ_Vo,rNum)
      print(paste0("rv$excludeQ_Vo = ",paste(rv$excludeQ_Vo,collapse = ", ")))
      
      
      f <- b[rNum,]
      
      # f <- b[rNum,]
      # a2 <- conjAct[as.numeric(rNum)]
      # rv$answerNow <- a2
      # f[1,2] <- sub(a2,"______",f[1,2],ignore.case = T)
      # lab1 <- paste0(f[1,2]," - ",f[1,3])
      
      rv$answerNow_Vo <- un_sp(f[,"eng"],",")
      
      ui_build1 <- div(id= "quizUI1_Vo",h3(f[,"spn"]))
      
      ui_build2 <- div(id= "quizUI2_Vo",textInput("quizAnswer_Vo",label = NULL))
      
      
    } 
    else {
      
      ui_build1 <- tags$div(id= "quizUI3_Vo", 
                            h2("Has agotado este conjunto de frases verbales")
      )
      
      ui_build2 <- tags$div(id= "quizUI4_Vo", 
                            h2("You have exhausted this Verb Sentence Set")
      )
      
    }
    
    insertUI(
      selector = "#here1_VoQ",
      where = "afterEnd",
      ui = ui_build1
    )
    
    insertUI(
      selector = "#here2_VoQ",
      where = "afterEnd",
      ui = ui_build2
    )
    
  }     
  
}) 
  
#####



#...Check our sentence input answers and give feedback to user
#####------------------------------------------------------------------

observeEvent(input$check_Vo,{
  
  a1 <- trimws(rv$answerCheck_Vo)
  pripas("rv$answerCheck_Vo <- ",rv$answerCheck_Vo)
  
  a2 <- trimws(rv$answerNow_Vo)
  a2 <- trimws(un_spf(a2,"\\(",2)[,1])
  pripas("rv$answerNow_Vo <- ",paste(a2,collapse = ","))
  
  
  # if(identical(a1,a2)){
  if(a1 %in% a2){
    
    print("We have a match")
    rv$nextCt4_Vo <- rv$nextCt4_Vo + 1
    rv$numRight_Vo <- rv$numRight_Vo + 1
    # shinyjs::alert(paste0("We have a match! This increases yout score ",
    #                       "to Correct: ",rv$numRight_Vo,
    #                       " / Incorrect: ",rv$numWrong_Vo))
    shinyalert(
      title = "Ohhhh Wow",
      text = paste0("\'",a1,"\' is the Correct Answer! This increases your score ",
                    "to Correct: ",rv$numRight_Vo,
                    " / Incorrect: ",rv$numWrong_Vo),
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "success",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#11D621",
      timer = 10000,
      imageUrl = "",
      animation = TRUE
    )
    
    
  } else {
    
    print("We do not have a match")
    rv$nextCt4_Vo <- rv$nextCt4_Vo + 1
    rv$numWrong_Vo <- rv$numWrong_Vo + 1
    # shinyjs::alert(HTML(paste0("You're going to need to study harder, ",
    #                            "the correct answer is \\'",
    #                            paste(a2,collapse = ", "),
    #                            "'. This increases yout score ",
    #                            "to Correct: ",rv$numRight_Vo,
    #                            " / Incorrect: ",rv$numWrong_Vo)))
    
    shinyalert::shinyalert(
      title = "Nooooooooo!!!!",
      text = paste0("You're going to need to study harder, ",a1," is not the answer.",
                    " The correct answer is \'",paste(a2,collapse = ", "),
                     "'. This increases yout score to Correct: ",rv$numRight_Vo,
                     " / Incorrect: ",rv$numWrong_Vo),
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "error",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#D61B11",
      timer = 10000,
      imageUrl = "",
      animation = TRUE
    )
    
  }
  
})


#####



