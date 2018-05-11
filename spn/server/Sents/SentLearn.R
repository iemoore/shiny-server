


sent_now <- eventReactive(input$search_result2,{
  
  if("All" %in% input$search_result2){
    
    return(sentdf)
  }
  else {
    
    return(filter(sentdf, verb %in% input$search_result2))  
  }  
  
})


output$sentVerbSelect <- renderUI({
  if (USER$Logged == TRUE) {
    
    # search_selection_choices("search_result2",
    #                          choices = verbdf$verb, 
    #                          default_text = "Please Select Verbs",
    #                          value = NULL, 
    #                          multiple = TRUE)
    
    selectInput("search_result2", label = NULL,
                choices = c("All",as.character(verbdf$verb)),
                selected = "All",
                multiple = TRUE)
  }    
})


output$tabSentLearn <- renderUI({
  
  #column(
  tabsetPanel(id = "subTabPanel3",
              # navlistPanel(id = "subTabPanel1",
              
              tabPanel("Anki 7627 w/A", value = "panel7_a",
                       
                       DTOutput('audio_tbl')
              ),
              
              tabPanel("Websters 575 w/A", value = "panel7_b",
                       
                       DTOutput('audio_tbl_web')
              ),
              
              tabPanel("Essential 501", value = "panel7_c",
                       
                      tags$br(),
                      uiOutput("sentVerbSelect"),
                      DTOutput('sent_tbl'),
                      shinyjs::hidden(wellPanel(id = "options_panel",
                            source("ui/checkboxSentences.R",local=T)$value
                      )),
                      actionButton("show_options", "Settings")
              )
  )#,
  #offset = 1,width = 10) 
})



#...Datatables
#####--------------------------------------------------

output$audio_tbl <- DT::renderDataTable({ 
  if (USER$Logged == TRUE) { 
    
    s1 <- audioDf[,c(1,2)]
    
    # s1$Audio <- paste0("<audio src=\"audio/", audioDf$num1,
    #                    ".mp3\" type=\"audio/mp3\" controls></audio>")
    
    s1$Audio <- paste0("<audio id=\"player", audioDf$num1,"\" src=\"audio/",
                  audioDf$num1,".mp3\"></audio><div> <button onclick=\"document",
                  ".getElementById('player", 
                  audioDf$num1,"').play()\">Play</button></div>")
    
    
    action <- DT::dataTableAjax(session, s1)
    
    DT::datatable(s1, 
            selection = list(mode = "none", target = "row"),
            options = list(ajax = list(url = action), 
                   # displayStart = isolate(rv$sent_start),
                   # pageLength = isolate(rv$sent_length),
                   #stateSave = TRUE,
                   # stateSave = ifelse(!is.null(rv$sent_length),
                                      # TRUE,FALSE),
                   
                   lengthMenu = c(10, 20, 50, 100),
                   language = list(search = 'Filter:')),
            escape = FALSE, 
            editable = T)
    
}})


output$audio_tbl_web <- DT::renderDataTable({ 
  if (USER$Logged == TRUE) { 
    
    s1 <- audioWeb[,c(1,2)]
    
    # s1$Audio <- paste0("<audio src=\"audio/", audioDf$num1,
    #                    ".mp3\" type=\"audio/mp3\" controls></audio>")
    
    s1$Audio <- paste0("<audio id=\"player", audioWeb$num1,"\" src=\"audio2/",
                       audioWeb$num1,".mp3\"></audio><div> <button onclick=\"document",
                       ".getElementById('player", 
                       audioWeb$num1,"').play()\">Play</button></div>")
    
    
    action <- DT::dataTableAjax(session, s1)
    
    DT::datatable(s1, 
                  selection = list(mode = "none", target = "row"),
                  options = list(ajax = list(url = action), 
                                 # displayStart = isolate(rv$sent_start),
                                 # pageLength = isolate(rv$sent_length),
                                 #stateSave = TRUE,
                                 # stateSave = ifelse(!is.null(rv$sent_length),
                                 # TRUE,FALSE),
                                 
                                 lengthMenu = c(10, 20, 50, 100),
                                 language = list(search = 'Filter:')),
                  escape = FALSE, 
                  editable = T)
    
  }})


output$sent_tbl <- DT::renderDataTable({
  if (USER$Logged == TRUE) {
    
    #input$reset
    
    s1 <- sent_now()
    if(length(s1[,1])>0){
    
      c1 <- input$checkGroup
      
      # print("Datatable render initiated")
      # print(paste0("DT: rv$sent_start <- ",isolate(rv$sent_start)))
      # print(paste0("DT: rv$sent_length <- ",isolate(rv$sent_length)))
      
      if(!(1%in%c1)) {s1$verb<-NULL}  
      if(!(2%in%c1)) {s1$spanish<-NULL} 
      if(!(3%in%c1)) {s1$english<-NULL} 
      if(!(4%in%c1)) {s1$tense<-NULL}   
      
      if(5 %in% c1) {
        s1$Like <- paste0('<a class="go-map-like" href="#" onClick="return',
                        ' false"><i class="glyphicon glyphicon-thumbs-up"></i></a>')  
        s1$Dislike <- paste0('<a class="go-map-dislike" href="#" onClick="return',
                        ' false"><i class="glyphicon glyphicon-thumbs-down"></i></a>')
      }
      if(6 %in% c1) {
        s1$Save <- paste0('<a class="go-map-save" href="#" onClick="return fals',
                        'e"><i class="glyphicon glyphicon-star-empty"></i></a>')
      }  
      
      
      action <- DT::dataTableAjax(session, s1)
      
      DT::datatable(s1, selection = list(mode = "single", target = "row"#,
                                         #selected = isolate(rv$sent_now)
      ),
      options = list(ajax = list(url = action), 
                     displayStart = isolate(rv$sent_start),
                     pageLength = isolate(rv$sent_length),
                     #stateSave = TRUE,
                     stateSave = ifelse(!is.null(rv$sent_length),
                                        TRUE,FALSE),
                     
                     lengthMenu = c(10, 20, 50, 100),
                     language = list(search = 'Filter:')
      ),
      escape = FALSE, 
      editable = T
      )
    }  
  }
})


#####
 














