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
      # global_user <<- as.character(USER$name)
      pripas("Login Successful: ",USER$name)
      
      pripas("User: ",USER$name)
      
      a11 <- readLines(paste0("userdata/",USER$name,"/savedVerbs.txt"))
      if(nchar(a11)>1){
        pripas("Login: savedVerbs <- ",a11)
        rv$savedVerbs <- as.numeric(unlist(str_split(a11,",")))  
        vls_cache <<- rv$savedVerbs
      } else { vls_cache <<- "" }


      a12 <- readLines(paste0("userdata/", USER$name,"/remVerbs.txt"))
      if(nchar(a12)>1){
        pripas("Login: remVerbs <- ",a12)
        rv$remVerbs <- as.numeric(unlist(str_split(a12,",")))
        vlr_cache <<- rv$remVerbs
      } else { vls_cache <<- ""}
      
      if(nchar(a12)<2 & nchar(a12)<2){
        
        pripas("rv$initTrig bc of empty rem/save called")
        logjs("rv$initTrig bc of empty rem/save called")
        rv$initTrig <- rv$initTrig*(-1)
      }
      
      
      rv$vls_length <- ifelse(length(rv$savedVerbs)<10,length(rv$savedVerbs),10)
      rv$vlr_length <- ifelse(length(rv$remVerbs)<10,length(rv$remVerbs),10)
      
      rv$exData <- readRDS(paste0("userdata/Data.rds"))
      rv$dfNow <- audioWeb
      rv$exData$row <- as.character(rv$exData$row)
      rv$exData <- rv$exData[which(rv$exData$user==USER$name),]
      
      rv$modal_data <- readRDS("userData/modal_data.rds") 
      rv$modal_data <- rv$modal_data[which(rv$modal_data$user==USER$name),]
      
      rv$typeLock <- 0
      rv$audioNow_sf <- 0
      
      print(paste(tail(rv$exData$row,n=5)))
      print(paste(tail(rv$exData$type,n=5)))
      
      # exDataG <<- rv$exData
      
      
      
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
             con = paste0("userdata/",global_user,"/savedVerbs.txt"))
  
  writeLines(paste(vlr_cache,collapse = ","),
             con = paste0("userdata/",global_user,"/remVerbs.txt"))
  
  
  saveRDS(rv$exData,paste0("userdata/Data.rds"))
  saveRDS(rv$modal_data,paste0("userData/modal_data.rds"))
  
  rv$typeLock <- 0
  rv$dfNow <- data.frame(var="1",bar="1")
  rv$exData <- NULL
  rv$exNow <- NULL
  rv$exNow_ct <- 0
  rv$tNow <- NULL
  rv$audioNow_sf <- 0
  rv$modal_data <- NULL
  rv$sf_word_now <- NULL
  
  
})

