

#...Basic Reactive Values 
#####------------------------------------------------------------------



uiTrigger_Ve <- makeReactiveTrigger()
fileTrigger_Ve <- makeReactiveTrigger()
rvs <- reactiveValues(data = NULL)
rv$sentlen_Ve <- 0
rv$checkNow_Ve <- ""

#...New dings
rv$showQuiz_Ve <- ""
rv$excludeQ_Ve <- 0
rv$nextCt4_Ve <- 0
rv$addCt_Ve <- 0
rv$answerNow_Ve <- ""
rv$answerCheck_Ve <- ""
rv$data_Ve <- data.frame(NULL)


rv$checkNowNum_Ve <- 0
rv$numWrong_Ve <- 0
rv$numRight_Ve <- 0

#####



#...Render UI 
#####------------------------------------------------------------------

output$verbType_Ve <- renderUI({
  if (USER$Logged == TRUE) {
    
    t4 <- c("Essential 500","Action","Home","Emotions")
    
    selectInput('chooseType_Ve', label = NULL,#'Choose Verb Type...',
                choices = c('', t4),selectize=TRUE,multiple = F)
    
  }
})


output$buttons_Ve <- renderUI({
  if (USER$Logged == TRUE) {
    div(
      actionButton(inputId = "add_Ve", label = "Start"),
      actionButton(inputId = "rem_Ve", label = "Reset")
    )
  }
})


output$quizShow_Ve <- renderUI({
  if (USER$Logged == TRUE) {
    
    a <- rv$addCt_Ve
    
    if(a>0) {isolate(actionButton("checkQ_Ve","Check"))}
    
  } 
})


output$quizNext_Ve <- renderUI({
  if (USER$Logged == TRUE) {
    
    a <- rv$addCt_Ve
    
    if(a>0) {isolate(actionButton("nextQ_Ve","Next"))}
    
  }
})


output$sentScore_Ve <- renderUI({
  if (USER$Logged == TRUE) {
    
    div(
      inlineDing(wellPanel(id="flashPanel",column(h2("Correct"),
                                                  h3(rv$numRight_Ve),width = 12))),
      inlineDing(wellPanel(id="flashPanel",column(h2("Incorrect"),
                                                  h3(rv$numWrong_Ve),width = 12)))
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

observeEvent(input$chooseType_Ve, {
  
  pripas("input$chooseType_Ve <- ",input$chooseType_Ve)
  
  a <- input$chooseType_Ve
  rv$showQuiz_Ve <- a
  
  rv$data_Ve <- verbdf
  
  uiTrigger_Ve$trigger()
  
})


observeEvent(input$add_Ve,{
  
  rv$addCt_Ve <- rv$addCt_Ve + 1 
  
  print("input$add_Ve")
})


observeEvent(input$checkQ_Ve,{
  
  rv$answerCheck_Ve <-  input$quizAnswer_Ve
  #pripas("input$quizAnswer <- ",input$quizAnswer)
  
  
})


observeEvent(input$nextQ_Ve,{
  
  rv$nextCt4_Ve <- rv$nextCt4_Ve + 1
  #print(rv$nextCt4)
})



observeEvent(input$rem_Ve, {
  
  rv$resetQ_Ve <- rv$resetQ_Ve + 1
  rv$addCt_Ve <- 0
  rv$numWrong_Ve <- 0
  rv$numRight_Ve <- 0
  removeUI("#quizUI1_Ve")
  removeUI("#quizUI2_Ve")
  removeUI("#quizUI3_Ve")
  removeUI("#quizUI4_Ve")
})


observeEvent(input$add_Ve, {
  
  # lapply(1:rv$sentlen, function(j) {
  #   
  #   removeUI(paste0("#sent",j))    
  #   
  # })
  
  uiTrigger_Ve$trigger()
  
})

# #...Choose Random (Might use later)
# observeEvent(input$rdm_Ve, {
#   
#   # lapply(1:rv$sentlen, function(j) {
#   #   
#   #   removeUI(paste0("#sent",j))    
#   #   
#   # })
#   
#   updateSelectInput(session,'chooseVerb', 'Choose Verb...',
#                     c(Choose='', as.character(verbdf$verb)),
#                     selected = verbdf$verb[floor(runif(1, min=5, max=501))])
#   
#   shinyjs::click("button#add")
#   
# })

#####


#...Dynamic UI rendering of sentences when input$add is clicked
#####------------------------------------------------------------------


observeEvent(c(input$add_Ve,rv$nextCt4_Ve) ,{ 
  
  pripas("rv$nextCt4_Ve <- ",rv$nextCt4_Ve)  
  
  
  cat("i rebuild the UI\n")
  uiTrigger_Ve$depend()
  b <- isolate(rv$data_Ve)
  b$verb <- as.character(b$verb)  
  pripas("dim(mydata)[1] <- ",dim(b)[1])
  
  
  
  removeUI("#quizUI1_Ve")
  removeUI("#quizUI2_Ve")
  removeUI("#quizUI3_Ve")
  removeUI("#quizUI4_Ve")
  
  
  if(dim(b)[1]>0){
    
    if(length(rv$excludeQ_Ve)<dim(b)[1]){
      
      
      rNum <- sample(rownames(b)[!(rownames(b) %in% rv$excludeQ_Ve)],1)
      rv$excludeQ_Ve <- c(rv$excludeQ_Ve,rNum)
      print(paste0("rv$excludeQ_Ve = ",paste(rv$excludeQ_Ve,collapse = ", ")))
      
      
      f <- b[rNum,]
      
      # f <- b[rNum,]
      # a2 <- conjAct[as.numeric(rNum)]
      # rv$answerNow <- a2
      # f[1,2] <- sub(a2,"______",f[1,2],ignore.case = T)
      # lab1 <- paste0(f[1,2]," - ",f[1,3])
      
      rv$answerNow_Ve <- un_sp(f[,2],",")
      
      ui_build1 <- div(id= "quizUI1_Ve",h3(f[,1]))
      
      ui_build2 <- div(id= "quizUI2_Ve",textInput("quizAnswer_Ve",label = NULL))
      
      
    } 
    else {
      
      ui_build1 <- tags$div(id= "quizUI3_Ve", 
                            h2("Has agotado este conjunto de frases verbales")
                            )
      
      ui_build2 <- tags$div(id= "quizUI4_Ve", 
                            h2("You have exhausted this Verb Sentence Set")
                            )
      
    }
    
    insertUI(
      selector = "#here_VeQ1",
      where = "afterEnd",
      ui = ui_build1
    )
    
    insertUI(
      selector = "#here_VeQ2",
      where = "afterEnd",
      ui = ui_build2
    )
    
  }     
  
}) 
#####



#...Check our sentence input answers and give feedback to user
#####------------------------------------------------------------------


observeEvent(input$checkQ_Ve,{
  
  a1 <- trimws(rv$answerCheck_Ve)
  pripas("rv$answerCheck_Ve <- ",rv$answerCheck_Ve)
  
  a2 <- trimws(rv$answerNow_Ve)
  a2 <- trimws(un_spf(a2,"\\(",2)[,1])
  pripas("rv$answerNow_Ve <- ",paste(a2,collapse = ","))
  
  
  # if(identical(a1,a2)){
  if(a1 %in% a2){
    
    print("We have a match")
    rv$nextCt4_Ve <- rv$nextCt4_Ve + 1
    rv$numRight_Ve <- rv$numRight_Ve + 1
    # shinyjs::alert(paste0("We have a match! This increases yout score ",
    #                       "to Correct: ",rv$numRight_Ve,
    #                       " / Incorrect: ",rv$numWrong_Ve))
    shinyalert(
      title = "Ohhhh Wow",
      text = paste0("\'",a1,"\' is the Correct Answer! This increases your score ",
                    "to Correct: ",rv$numRight_Ve,
                    " / Incorrect: ",rv$numWrong_Ve),
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
    rv$nextCt4_Ve <- rv$nextCt4_Ve + 1
    rv$numWrong_Ve <- rv$numWrong_Ve + 1
    # shinyjs::alert(HTML(paste0("You're going to need to study harder, ",
    #                            "the correct answer is ",paste(a2,collapse = ", "),
    #                            ". This increases yout score ",
    #                            "to Correct: ",rv$numRight_Ve,
    #                            " / Incorrect: ",rv$numWrong_Ve)))
    shinyalert::shinyalert(
      title = "Nooooooooo!!!!",
      text = paste0("You're going to need to study harder, ",a1," is not the answer.",
                    " The correct answer is \'",paste(a2,collapse = ", "),
                    "'. This increases yout score to Correct: ",rv$numRight_Ve,
                    " / Incorrect: ",rv$numWrong_Ve),
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




















