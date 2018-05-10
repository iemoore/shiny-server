

#...Basic Reactive Values 
#####------------------------------------------------------------------


makeReactiveTrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}
uiTrigger <- makeReactiveTrigger()
fileTrigger <- makeReactiveTrigger()
rvs <- reactiveValues(data = NULL)
rv$sentlen <- 0
rv$checkNow <- ""

#...New dings
rv$showQuiz <- ""
rv$excludeQ <- 0
rv$nextCt4 <- 0
rv$addCt <- 0
rv$answerNow <- ""
rv$answerCheck <- ""
rv$data <- data.frame(NULL)
#####


#...Render UI 
#####------------------------------------------------------------------

output$verbSelect <- renderUI({
  if (USER$Logged == TRUE) {

                      rv$resetQ

                      # search_selection_choices("chooseVerb",
                      #             choices = verbdf$verb, value = NULL,
                      #             multiple = TRUE)
                      
                      selectInput("chooseVerb", label = NULL,
                                  choices = verbdf$verb, #value = NULL,
                                  multiple = TRUE)
  }
})


output$buttons <- renderUI({
  if (USER$Logged == TRUE) {
    div(
      actionButton(inputId = "add", label = "Start"),
      actionButton(inputId = "rem", label = "Reset"),
      actionButton(inputId = "rdm", label = "Random")
    )
  }
})


output$sentScore <- renderUI({
  if (USER$Logged == TRUE) {
   
    div(
    inlineDing(wellPanel(id="flashPanel",column(h2("Correct"),
                                         h3(rv$numRight),width = 12))),
    inlineDing(wellPanel(id="flashPanel",column(h2("Incorrect"),
                                         h3(rv$numWrong),width = 12)))
    )
    
    # div(
    # wellPanel(h2("Correct")),
    # wellPanel(h2("Incorrect"))
    # )
  }
})


output$quizShow <- renderUI({
  if (USER$Logged == TRUE) {
  
    a <- rv$addCt
    
    if(a>0) {isolate(actionButton("checkQ","Check"))}
    
  }
})


output$quizNext <- renderUI({
  if (USER$Logged == TRUE) {
  
  a <- rv$addCt
  
  if(a>0) {isolate(actionButton("nextQ","Next"))}
  
  
  }
})


#####


#...Observe inputs for sentence quiz: chooseVerb, rem, add
#####------------------------------------------------------------------

observeEvent(input$chooseVerb, {
  
  # a <- un_sp(input$chooseVerb,",")
  rv$showQuiz <- input$chooseVerb
  
  rv$data <- sentdf[which(sentdf$verb %in% a),]
  #print(paste0("rsv$data=",rvs$data[1,1]," has ",length(rvs$data)," items"))
  uiTrigger$trigger()
  
})


observeEvent(input$add,{
  
  rv$addCt <- rv$addCt + 1 
  
  print("input$add")
})


observeEvent(input$checkQ,{
  
  rv$answerCheck <-  input$quizAnswer
  #pripas("input$quizAnswer <- ",input$quizAnswer)
  
  
})


observeEvent(input$nextQ,{
  
  rv$nextCt4 <- rv$nextCt4 + 1
  #print(rv$nextCt4)
})


observeEvent(input$rem, {
  
    rv$resetQ <- rv$resetQ + 1
    rv$addCt <- 0
    removeUI("#quizUI")
    rv$numWrong <- 0
    rv$numRight <- 0
    removeUI("#quizUI1")
    removeUI("#quizUI2")
    removeUI("#quizUI3")
    removeUI("#quizUI4")
})


observeEvent(input$add, {
  
  lapply(1:rv$sentlen, function(j) {
    
    removeUI(paste0("#sent",j))    
    
  })
  
  uiTrigger$trigger()
  
})


observeEvent(input$rdm, {
  
  lapply(1:rv$sentlen, function(j) {
    
    removeUI(paste0("#sent",j))    
    
  })
  
  updateSelectInput(session,'chooseVerb', 'Choose Verb...',
                          c(Choose='', as.character(verbdf$verb)),
                          selected = verbdf$verb[floor(runif(1, min=5, max=501))])
  
  shinyjs::click("button#add")
  
})

#####


#...Dynamic UI rendering of sentences when input$add is clicked
#####------------------------------------------------------------------


  observeEvent(c(input$add,rv$nextCt4) ,{ 
  
  pripas("rv$nextCt4 <- ",rv$nextCt4)  
    
    
    cat("i rebuild the UI\n")
    uiTrigger$depend()
    b <- isolate(rv$data)
    b$spanish <- as.character(b$spanish)  
    pripas("dim(mydata)[1] <- ",dim(b)[1])
    removeUI("#quizUI1")
    removeUI("#quizUI2")
    removeUI("#quizUI3")
    removeUI("#quizUI4")
    
    
    if(dim(b)[1]>0){
      
      if(length(rv$excludeQ)<dim(b)[1]){
        
        
        rNum <- sample(rownames(b)[!(rownames(b) %in% rv$excludeQ)],1)
        
        rv$excludeQ <- c(rv$excludeQ,rNum)
        print(paste0("rv$excludeQ = ",paste(rv$excludeQ,collapse = ", ")))
        
        
        f <- b[rNum,]
        a2 <- conjAct[as.numeric(rNum)]
        rv$answerNow <- a2
        f[1,2] <- sub(a2,"______",f[1,2],ignore.case = T)
        lab1 <- paste0(f[1,2]," - ",f[1,3])
        
      
        ui_build1 <- div(id= "quizUI1",h3(lab1))
          
        ui_build2 <- div(id= "quizUI2",textInput("quizAnswer",label = NULL))
          
          
      } 
      else {
        
        ui_build1 <- tags$div(id= "quizUI3", 
                              h2("Has agotado este conjunto de frases verbales")
        )
        
        ui_build2 <- tags$div(id= "quizUI4", 
                              h2("You have exhausted this Verb Sentence Set")
        )
        
      }
      
      insertUI(
        selector = "#hereSeQ1",
        where = "afterEnd",
        ui = ui_build1
      )
      
      insertUI(
        selector = "#hereSeQ2",
        where = "afterEnd",
        ui = ui_build2
      )
      
    }     
    
}) 
#####



#...Check our sentence input answers and give feedback to user
#####------------------------------------------------------------------


rv$checkNowNum <- 0
rv$numWrong <- 0
rv$numRight <- 0

observeEvent(input$checkQ,{
  
  a1 <- rv$answerCheck
  pripas("rv$answerCheck <- ",rv$answerCheck)
  
  a2 <- rv$answerNow
  pripas("rv$answerNow <- ",rv$answerNow)
  

    if(identical(a1,a2)){

      print("We have a match")
      rv$nextCt4 <- rv$nextCt4 + 1
      rv$numRight <- rv$numRight + 1
      # shinyjs::alert(paste0("We have a match! This increases yout score ",
      #                       "to Correct: ",rv$numRight,
      #                       " / Incorrect: ",rv$numWrong))
      shinyalert(
        title = "Ohhhh Wow",
        text = paste0("\'",a1,"\' is the Correct Answer! This increases your score ",
                      "to Correct: ",rv$numRight,
                      " / Incorrect: ",rv$numWrong),
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
      rv$nextCt4 <- rv$nextCt4 + 1
      rv$numWrong <- rv$numWrong + 1
      # shinyjs::alert(HTML(paste0("You're going to need to study harder, ",
      #                            "the correct answer is ",a2,
      #                            ". This increases yout score ",
      #                            "to Correct: ",rv$numRight,
      #                            " / Incorrect: ",rv$numWrong)))
      shinyalert::shinyalert(
        title = "Nooooooooo!!!!",
        text = paste0("You're going to need to study harder, ",a1," is not the answer.",
                      " The correct answer is \'",paste(a2,collapse = ", "),
                      "'. This increases yout score to Correct: ",rv$numRight,
                      " / Incorrect: ",rv$numWrong),
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#D61B11",
        timer = 30000,
        imageUrl = "",
        animation = TRUE
      )

    }

})

#####




