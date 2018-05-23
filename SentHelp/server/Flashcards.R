




#...Outputs
#####

output$flashBack <- renderUI({if (USER$Logged == TRUE) {
  

    isolate(actionButton("back1","Back"))
 
  
}})


output$flashShow <- renderUI({if (USER$Logged == TRUE) {
  
  isolate(actionButton("show1","Show"))
  
}
})


output$flashNext <- renderUI({if (USER$Logged == TRUE) {
  
  isolate(actionButton("next1","Next"))
  
}
})


output$flashAudioWeb <- renderUI({if (USER$Logged == TRUE) {
  
  a <- 1  
  
  if(!(a == 1)) {
    
    HTML(paste0("<audio class=\"aud\" id=\"player", rv$audioNow,
                "\" src=\"audio",ifelse(a==2,"2",""),"/",
                rv$audioNow,".mp3\" ",ifelse(rv$autoplay==T,"autoplay",""),
                " controls></audio>"))      
    
  }
}
})

#####


#...ObserveEvent
#####

#...Print
observeEvent(input$myPicker,{
  
  pripas("input$myPicker <- ",paste(input$myPicker,collapse = ", "))
})
observeEvent(input$which_df,{
  
  pripas("input$which_df <- ",paste(input$which_df,collapse = ", "))
})


#...Counts
observeEvent(input$back1,{
  
  rv$backCt <- rv$backCt + 1
  shinyjs::hide(id= "hideF2")
  
})
observeEvent(input$next1,{
  
  # print("Next Clicked")
  rv$nextCt <- rv$nextCt + 1
  shinyjs::hide(id= "hideF2")
  
})
observeEvent(input$show1, {
  
  #print(paste0("Trying to toggle hideF2"))
  shinyjs::toggle(id= "hideF2")
  
})


#...Flashcards Back,Next
observeEvent(rv$nextCt,{
  
  removeUI("#flashUI1");removeUI("#flashUI2");
  removeUI("#flashUI3");removeUI("#flashUI4")
  
  
  # d1 <- sentdf[,c("spn","eng","conj","num1","x")]
  d1 <- dataNow()
  
  # pripas("Next called, rv$exclude_ct <- ",rv$exclude_ct,
         # " / rv$exclude <- ",paste(rv$exclude,collapse = ", "))
  
  rv$exclude_ct <- rv$exclude_ct + 1
  
  if(rv$exclude_ct==(length(rv$exclude)+1)){
    
    # pripas("rv$exclude_ct==length(rv$exclude)")
    
    rNum <- sample(rownames(d1)[!(rownames(d1) %in% rv$exclude)],1)
    f <- d1[rNum,]
    
    rv$audioNow <- f$num1
    rv$exclude <- c(rv$exclude, f$x)
    ex_data <<- rv$exclude
    
    # pripas("rv$exclude <- ",paste(rv$exclude,collapse = ", "))

    sent2 <- sent2function(f)
    
    ui_tar1 <- HTML(sent2)
    ui_tar2 <- f[1,"eng"]
    
    ui_build1 <- div(id= "flashUI1",h3(ui_tar1))
    
    ui_build2 <- div(id= "flashUI2",
                     shinyjs::hidden(div(id = "hideF2",h3(ui_tar2))))
    
    insertUI(
      selector = "#here1",
      where = "afterEnd",
      ui = ui_build1
    )
    
    insertUI(
      selector = "#here2",
      where = "afterEnd",
      ui = ui_build2
    )
    
  } else {
    
    # pripas("rv$exclude_ct<length(rv$exclude)")
    
    rNum <- rv$exclude[rv$exclude_ct]
    f <- d1[rNum,]
    rv$audioNow <- f$num1
    
    # pripas("rv$exclude <- ",paste(rv$exclude,collapse = ", "))
    
    
    sent2 <- sent2function(f)
    
    ui_tar1 <- HTML(sent2)
    ui_tar2 <- f[1,"eng"]
    
    ui_build1 <- div(id= "flashUI1",h3(ui_tar1))
    
    ui_build2 <- div(id= "flashUI2",
                     shinyjs::hidden(div(id = "hideF2",h3(ui_tar2))))
    
    insertUI(
      selector = "#here1",
      where = "afterEnd",
      ui = ui_build1
    )
    
    insertUI(
      selector = "#here2",
      where = "afterEnd",
      ui = ui_build2
    )
  }
  
  # pripas("Next finished , rv$exclude_ct <- ",rv$exclude_ct)
  
})
observeEvent(rv$backCt,{if(rv$backCt>0){
  
  removeUI("#flashUI1");removeUI("#flashUI2");
  removeUI("#flashUI3");removeUI("#flashUI4")
  
  rv$exclude_ct <- rv$exclude_ct - 1
  # pripas("Back called , rv$exclude_ct <- ",rv$exclude_ct)
  rNum <- rv$exclude[rv$exclude_ct]
  f <- d1[rNum,]
  rv$audioNow <- f$num1

  
  sent2 <- sent2function(f)
  
  ui_tar1 <- HTML(sent2)
  ui_tar2 <- f[1,"eng"]
  
  ui_build1 <- div(id= "flashUI1",h3(ui_tar1))
  ui_build2 <- div(id= "flashUI2",
                   shinyjs::hidden(div(id = "hideF2",h3(ui_tar2))))
  
  insertUI(
    selector = "#here1",
    where = "afterEnd",
    ui = ui_build1
  )
  
  insertUI(
    selector = "#here2",
    where = "afterEnd",
    ui = ui_build2
  )
  
  
}})


#...Conj click
observeEvent(input$flashClick_web,{
  
  a <- input$flashClick_web[1]
  
  pripas("input$flashClick_web[1] <- ",rv$conj1[as.numeric(a)])
  
  b0 <- rv$conj1[as.numeric(a)]
  
  b <- as.character(unlist(str_split_fixed(b0,"-",3)))
  
  word1 <- b[1]
  verb1 <- b[2]
  tense1 <- unlist(str_split(b[3],"/"))
  
  c <- filter(conjdf, verb==verb1, tenseNum %in% tense1)
  
  pripas(b0," <- ",paste(c$conj," = ",c$popup,collapse = ", "))
  
  
  toggleModal(session, "sf_Modal", toggle = "toggle")
  
})

#####


#...Reactives
#####

dataNow <- reactive({
  
  c1 <- c(1, 4, 6)
  c1 <- input$myPicker
  
  m <- sentdf
  m0 <- input$which_df
  m <- m[which(m$cType %in% m0),]
  
  m1 <- NULL
  
  
  if(length(c1)>0){
    
    if(1 %in% c1){m1 <- c(m1,grep("1",m$Infin))}
    if(2 %in% c1){m1 <- c(m1,grep("1",m$PrePar))}
    if(3 %in% c1){m1 <- c(m1,grep("1",m$PasPar))}
    if(4 %in% c1){m1 <- c(m1,grep("1",m$Indic))}
    if(5 %in% c1){m1 <- c(m1,grep("1",m$Subjun))}
    if(6 %in% c1){m1 <- c(m1,grep("1",m$Imper))}
    if(7 %in% c1){m1 <- c(m1,grep("1",m$Contin))}
    if(8 %in% c1){m1 <- c(m1,grep("1",m$Perfec))}
    if(9 %in% c1){m1 <- c(m1,grep("1",m$PerSub))}
    
    m <- m[sort(unique(m1)),]
    
  }
  
  pripas("dataNow() called dim()[1] = ",dim(m)[1])
  
  return(m[,c("spn","eng","conj","num1","x")])
  
})


#####


#...Datatables
#####

output$dataNowTable <- DT::renderDataTable({ if (USER$Logged == TRUE) { 
    
    s1 <- dataNow()
    
    # s1$Audio <- paste0("<audio src=\"audio/", audioDf$num1,
    #                    ".mp3\" type=\"audio/mp3\" controls></audio>")
    
    s1 <- s1[,c("spn","eng","conj","x")]
    
    action <- DT::dataTableAjax(session, s1)
    
    pripas("dataTableNow called dim()[1] = ",dim(s1)[1])
    
    DT::datatable(s1, 
                  selection = list(mode = "none", target = "row"),
                  options = list(ajax = list(url = action), 
                                 # displayStart = isolate(rv$sent_start),
                                 # pageLength = isolate(rv$sent_length),
                                 #stateSave = TRUE,
                                 # stateSave = ifelse(!is.null(rv$sent_length),
                                 # TRUE,FALSE),
                                 
                                 lengthMenu = c(5, 20, 50, 100),
                                 language = list(search = 'Filter:')),
                  escape = FALSE, 
                  editable = T)
    
  }})


output$excludeTable <- DT::renderDataTable({ if (USER$Logged == TRUE) {

  s1 <- sentdf

  s1 <- s1[rv$exclude,c("spn","eng","conj","x")]

  # s1$Audio <- paste0("<audio src=\"audio/", audioDf$num1,
  #                    ".mp3\" type=\"audio/mp3\" controls></audio>")


  action <- DT::dataTableAjax(session, s1)

  DT::datatable(s1,
                selection = list(mode = "none", target = "row"),
                options = list(ajax = list(url = action),
                               # displayStart = isolate(rv$sent_start),
                               # pageLength = isolate(rv$sent_length),
                               #stateSave = TRUE,
                               # stateSave = ifelse(!is.null(rv$sent_length),
                               # TRUE,FALSE),

                               lengthMenu = c(5, 20, 50, 100),
                               language = list(search = 'Filter:')),
                escape = FALSE,
                editable = T)

}})

#####
