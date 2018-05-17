#### Log in module ###

USER <- reactiveValues()
USER$Logged <- FALSE
USER$pass <- ""
USER$name <- "ian"

PASSWORD <- data.frame(
  User = c("ian","cat"), 
  Pswd = c("ipass","cpass")
  )

output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    wellPanel(
      textInput("userName", "User Name:"),
      passwordInput("passwd", "Password:"),
      br(),
      actionButton("Login", "Log in")
    )
  }
})

output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    USER$pass
  }  
})

# Login info during session ----
output$userPanel <- renderUI({
  if (USER$Logged == TRUE) {

                span("User: ",actionLink("logout", USER$name) )

  }  
})

# control login
observeEvent(input$Login,{
  Username <- tolower(gsub(" ","",isolate(input$userName)))
  Password <- isolate(input$passwd)
  Id.username <- which(PASSWORD$User == Username)
  Id.password <- which(PASSWORD$Pswd    == Password)
  if (length(Id.username) > 0 & length(Id.password) > 0) {
    if (Id.username == Id.password) {
      
      USER$Logged <- TRUE
      USER$name <- Username  
      global_user <<- USER$name
      print("Login Successful")
      
      pripas("User: ",USER$name)
      
      a11 <- readLines(paste0("userData/",USER$name,"/savedVerbs.txt"))
      if(nchar(a11)>1){
        pripas("Login: savedVerbs <- ",a11)
        rv$savedVerbs <- as.numeric(unlist(str_split(a11,",")))  
        vls_cache <<- rv$savedVerbs
      } else { vls_cache <<- "" }


      a12 <- readLines(paste0("userData/", USER$name,"/remVerbs.txt"))
      if(nchar(a12)>1){
        pripas("Login: remVerbs <- ",a12)
        rv$remVerbs <- as.numeric(unlist(str_split(a12,",")))
        vlr_cache <<- rv$remVerbs
      } else { vls_cache <<- ""}
      
      if(nchar(a12)<2 & nchar(a12)<2){
        
        pripas("rv$initTrig bc of empty rem/save called")
        rv$initTrig <- rv$initTrig*(-1)
      }
      
      
      rv$vls_length <- ifelse(length(rv$savedVerbs)<10,length(rv$savedVerbs),10)
      rv$vlr_length <- ifelse(length(rv$remVerbs)<10,length(rv$remVerbs),10)
      
      
      rv$excludeE <- as.numeric(un_sp(readLines(paste0("userData/",USER$name,
                                  "/excludeE.txt")),","))
      rv$excludeW <- as.numeric(un_sp(readLines(paste0("userData/",USER$name,
                                  "/excludeW.txt")),","))
      rv$excludeA <- as.numeric(un_sp(readLines(paste0("userData/",USER$name,
                                  "/excludeA.txt")),","))
      exE_data <<- rv$excludeE
      exW_data <<- rv$excludeW
      exA_data <<- rv$excludeA
      
      rv$excludeE_ct <- length(rv$excludeE)
      rv$excludeW_ct <- length(rv$excludeW)
      rv$excludeA_ct <- length(rv$excludeA)
      
    } 
  } else {
    USER$pass <- "User name or password failed!"
  }
})

# control logout
observeEvent(input$logout , {
  USER$Logged <- FALSE
  USER$pass <- ""
  
  writeLines(paste(vls_cache,collapse = ","),
             con = paste0("userData/",global_user,"/savedVerbs.txt"))
  
  writeLines(paste(vlr_cache,collapse = ","),
             con = paste0("userData/",global_user,"/remVerbs.txt"))
  
  writeLines(paste(exE_data,collapse = ","),
             paste0("userData/",global_user,"/excludeE.txt"))
  
  writeLines(paste(exW_data,collapse = ","),
             paste0("userData/",global_user,"/excludeW.txt"))
  
  writeLines(paste(exA_data,collapse = ","),
             paste0("userData/",global_user,"/excludeA.txt"))
  
  exE_data <<- ""
  exW_data <<- ""
  exA_data <<- ""
  global_user <<- ""
  vls_cache <<- ""
  vlr_cache <<- ""
  
  
})

