


#... rv init
#####------------------------------------------------------------------


# rv$savedVerbs <- sort(as.numeric(unlist(str_split(readLines(paste0("solid/data/",
#                                             USER$name,"/savedVerbs.txt")),","))))
# len_s1 <- length(sort(as.numeric(unlist(str_split(readLines(paste0("solid/data/",
#                                             USER$name,"/savedVerbs.txt")),",")))))
rv$savedVerbs <- NULL


# rv$remVerbs <- sort(as.numeric(unlist(str_split(readLines(paste0("solid/data/",
#                                                 USER$name,"/remVerbs.txt")),","))))
# len_r1 <- length(sort(as.numeric(unlist(str_split(readLines(paste0("solid/data/",
#                                                 USER$name,"/remVerbs.txt")),",")))))
rv$remVerbs <- NULL


rv$vl_start <- 0
rv$vl_length <- 10
rv$vl_count <- 1

rv$vls_start <- 0
rv$vls_length <- 10
rv$vls_count <- 1

rv$vlr_start <- 0
rv$vlr_length <- 10
rv$vlr_count <- 1 

rv$vl_data <- data.frame()
rv$vls_data <- data.frame(verb="",meaning="",rem="")
rv$vlr_data <- data.frame(verb="",meaning="",rem="")

rv$initTrig <- 1

#####

#...Data Observe
#####------------------------------------------------------------------

observeEvent(c(rv$initTrig,rv$savedVerbs,rv$remVerbs),{
  
    pripas("c(rv$initTrig,rv$savedVerbs,rv$remVerbs) called")
  
    s1 <- verbdf
    
    
    s1$Save <- ifelse(seq(1:501) %in% rv$savedVerbs,
                      
                paste0('<a data-row="',seq(1:length(s1[,1])),
                       '" class="go-map-save-ve" href="#" onClick="return fals',
                       'e"><i class="glyphicon glyphicon-star"></i></a>'),
                
                paste0('<a data-row="',seq(1:length(s1[,1])),
                       '" class="go-map-save-ve" href="#" onClick="return fals',
                       'e"><i class="glyphicon glyphicon-star-empty"></i></a>') 
                )    
      
    s1$Rem <- paste0('<a data-row="',seq(1:length(s1[,1])),
                      '" class="go-map-rem-ve" href="#" onClick="return fals',
                      'e"><i class="glyphicon glyphicon-remove-sign"></i></a>') 
    
    # s1$Audio <- paste0("<audio src=\"audio/",seq(1:length(s1[,1])),
    #                    ".mp3\" type=\"audio/mp3\" controls></audio>")
    
    
    # pripas("length(rv$remVerbs) <- ",length(rv$remVerbs))
    
    if(length(rv$remVerbs)>0){
      
      rv$vl_data <- s1[-as.numeric(rv$remVerbs),]
      # s2 <- s1[-n,]
      
    } 
    else{
      
      rv$vl_data <- s1
      # s2 <- s1
    }  
  
})   


observeEvent(rv$savedVerbs,{
  
  pripas("observeEvent(rv$savedVerbs,{}) called")
  
  s1 <- rv$savedVerbs
 
  if(length(s1)>0){ 
    
    pripas("ob: rv$savedVerbs <- ",paste(rv$savedVerbs,collapse = ", "))
    
    s1 <- verbdf[(rownames(verbdf) %in% rv$savedVerbs),]
    
    s1$Rem <- paste0('<a data-row="',rownames(s1),
                   '" class="go-map-rem-ves" href="#" onClick="return fals',
                   'e"><i class="glyphicon glyphicon-remove-sign"></i></a>')
     
    rv$vls_data <- s1 
    pripas("dim(rv$vls_data)[1] = ",dim(rv$vls_data)[1])  

  } else {
    
    rv$vls_data <-data.frame(verb="",meaning="",rem="")
    
  }
  
  
  
})


observeEvent(rv$remVerbs,{

  s1 <- rv$remVerbs

  if(length(s1)>0){  
    
    pripas("rv$remVerbs <- ",paste(rv$remVerbs,collapse = ", "))
    
    s1 <- verbdf[(rownames(verbdf) %in% rv$remVerbs),]
    
    s1$Rem <- paste0('<a data-row="',rownames(s1),
                   '" class="go-map-rem-ver" href="#" onClick="return fals',
                   'e"><i class="glyphicon glyphicon-remove-sign"></i></a>') 
  
    pripas("dim(rv$vlr_data)[1] <- ",dim(rv$vlr_data)[1])  
    rv$vlr_data <- s1  
  }
  

  
})

#####


#... observeEvents
#####------------------------------------------------------------------


observeEvent(input$verb_tbl_rows_current,{

  a <- input$verb_tbl_rows_current
  rv$vl_start <- a[1]-1
  rv$vl_length <- length(a)
  
})


observeEvent(input$verb_tbl_saved_rows_current,{

  a <- input$verb_tbl_saved_rows_current
  rv$vls_start <- a[1]-1
  rv$vls_length <- length(a)
  
})


observeEvent(input$verb_tbl_rem_rows_current,{

  a <- input$verb_tbl_rem_rows_current
  rv$vlr_start <- a[1]-1
  rv$vlr_length <- length(a)
  
})


#...Saved verb from main tab
observeEvent(input$verbSave,{
  
 
  a <- input$verbSave$row
  
  pripas("verbSave called on row ",a)

  
  if(a %in% rv$savedVerbs){
    
    pripas("verbSave: Removing '",a, "' from Saved list")
    rv$savedVerbs <- rv$savedVerbs[-grep(a,rv$savedVerbs)]
    
    vls_cache <<- rv$savedVerbs
    
  }
  else{
    
    pripas("verbSave: Adding '",a, "' to Saved list")
    rv$savedVerbs <- sort(c(rv$savedVerbs,a))
    
    pripas("From verbSave procedure: rv$savedVerbs <- ",
           paste(rv$savedVerbs,collapse = ", "))
    
    vls_cache <<- rv$savedVerbs

  }
  
})


#...Remove verb from Saved Tab
observeEvent(input$verbRemSaved,{
  
  pripas("verbRemSaved <- row:",input$verbRemSaved$row)
  
  a <- input$verbRemSaved$row
  a2 <- grep(paste0("\\b",a,"\\b"),rv$savedVerbs)
  
  
  if(length(a2)>0){
    
    pripas("verbRemSaved: Removing '",a, "' from Saved list")
    pripas("rv$savedVerbs <- ",paste(rv$savedVerbs,collapse = ", "))
    
    b <- grep(paste0("\\b",a,"\\b"),rv$savedVerbs)
    
    rv$savedVerbs <- rv$savedVerbs[-b]
    
    # if(length(rv$savedVerbs)==0){
    #   
    #   rv$vls_data <- data.frame(verb="",meaning="",rem="")
    # }    
    
    vls_cache <<- rv$savedVerbs
    
    # writeLines(paste(rv$savedVerbs,collapse = ","),
    #            con = paste0("userData/",USER$name,"/savedVerbs.txt"))
    
  }
  
})


#...Remove verb from main tab
observeEvent(input$verbRem,{
  
  pripas("verbRem <- ",input$verbRem$row)
  
  a <- input$verbRem$row
  
  pripas("Adding '",a, "' to Removed list")
  
  rv$remVerbs <- sort(c(rv$remVerbs,a))
  
  vlr_cache <<- rv$remVerbs
  
  # writeLines(paste(rv$remVerbs,collapse = ","),
  #            con = paste0("userData/",USER$name,"/remVerbs.txt"))

})


#...Re-add verb from removed tab
observeEvent(input$verbSaveRemoved,{
  
  pripas("verbSaveRemoved <- ",input$verbSaveRemoved$row)
  
  a <- input$verbSaveRemoved$row
  a2 <- grep(paste0("\\b",a,"\\b"),rv$remVerbs)
  
  if(length(a2)>0){
    
    pripas("Removing '",a, "' from Removed list")
    rv$remVerbs <- rv$remVerbs[-grep(paste0("\\b",a,"\\b"),rv$remVerbs)]
    
    if(length(rv$remVerbs)==0){
      
      rv$vlr_data <- data.frame(verb="",meaning="",rem="")
    }
    
    vlr_cache <<- rv$remVerbs
    
    # writeLines(paste(rv$remVerbs,collapse = ","),
    #            con = paste0("userData/",USER$name,"/remVerbs.txt"))
  }
  
})
 





#####


#... renderUI
#####------------------------------------------------------------------

output$tabVerbLearn <- renderUI({
  
  tabsetPanel(id = "subTabPanel1",
              # navlistPanel(id = "subTabPanel1",
              
        tabPanel("Ess. 501", value = "panel1_a",
                 
                br(),DTOutput('verb_tbl')
        ),
      
        tabPanel("Saved", value = "panel1_b",
                   
                   br(),DTOutput('verb_tbl_saved')
                   
        ),                
        tabPanel("Removed", value = "panel1_c",
                 
                br(),DTOutput('verb_tbl_rem')
                 
        )
  )
}) 


output$verb_all_tbl <- DT::renderDataTable({
  if (USER$Logged == TRUE) {
    
    input$reset  
    
    a <- filter(vocabdf, type=="verb")[,c("spn","eng")]
    colnames(a) <- c("verb","meaning")
    a$verb <- as.character(a$verb)
    a <- a[order(a$verb),]
    rownames(a) <- NULL
    
    
    datatable(a)
    
    
  }
})


output$verb_tbl <- DT::renderDataTable({
  if (USER$Logged == TRUE) {
    
    pripas("render verb_tbl called")
    
    # s2 <- vl_data()
    s2 <- isolate(rv$vl_data)
    
    
    # action <- DT::dataTableAjax(session, s2)
    
    DT::datatable(s2, selection = list(mode = "none", target = "row"#,
                                       #selected = isolate(rv$sent_now)
    ),
    options = list(#ajax = list(url = action), 
                  displayStart = isolate(rv$vl_start),
                  pageLength = isolate(rv$vl_length),
                  processing = FALSE,
                  deferRender = TRUE,
                  # scrollCollapse=TRUE,
                  scrollX=TRUE,
                  # autoWidth = TRUE,
                  # columnDefs = list(list(width = c("25%"),
                  #                        targets = c(1,2,3,4))),
                  
                  # columnDefs = list(list(width = c("40%","40%","10%","10%"),
                  #                        targets = 4)),
                  # columnDefs = list(list(targets = 4, visible = FALSE)),
                  
                  columnDefs = list(list(width = '35%', targets = list(1,2)),
                                    list(className = 'dt-center', targets = c(0,1,3,4))),

                  lengthMenu = c(10, 20, 50, 100),
                  language = list(search = 'Filter:')
    ), 
    # rownames=FALSE,
    escape = FALSE,  
    editable = T
    ) 
  
  }
})


output$verb_tbl_saved <- DT::renderDataTable({
  if (USER$Logged == TRUE) { 
    
    # s1 <- verbdf[(rownames(verbdf) %in% isolate(rv$savedVerbs)),] 
    s1 <- isolate(rv$vls_data)
    # s1 <- rv$vls_data
    
    #if(!is_empty(s1)){
    
      pripas("rv$vls_data not empty")
      pripas("rv$vls_start <- ",isolate(rv$vls_start))
      pripas("rv$vls_length <- ",isolate(rv$vls_length))
      
      DT::datatable(s1, selection = list(mode = "none", target = "row"),
      
      options = list(#ajax = list(url = action), 
                    displayStart = isolate(rv$vls_start),
                    pageLength = isolate(rv$vls_length),
                    processing = FALSE,
                    lengthMenu = c(10, 20, 50, 100),
                    language = list(search = 'Filter:')
      ),
      escape = FALSE, 
      editable = T
      )
      
    #}
  }
})


output$verb_tbl_rem <- DT::renderDataTable({
  if (USER$Logged == TRUE) { 
    
    # s1 <- verbdf[(rownames(verbdf) %in% isolate(rv$remVerbs)),] 
    s1 <- isolate(rv$vlr_data)
    # s1 <- rv$vlr_data
    

      pripas("rv$vlr_data not empty")
      
      DT::datatable(s1, selection = list(mode = "none", target = "row"#,
                                         #selected = isolate(rv$sent_now)
      ),
      options = list(#ajax = list(url = action), 
                    displayStart = isolate(rv$vlr_start),
                    pageLength = isolate(rv$vlr_length),
                    processing = FALSE,
                    lengthMenu = c(10, 20, 50, 100),
                    language = list(search = 'Filter:')
      ),
      escape = FALSE, 
      editable = T
      )     
      
      
    #}
  }
})

#####


#... dataTable proxy
####-------------------------------------------------------------------

  proxy1 = dataTableProxy('verb_tbl')
  proxy2 = dataTableProxy('verb_tbl_saved')
  proxy3 = dataTableProxy('verb_tbl_rem')

  # Main
  observe({
    
    dataTableAjax(session, rv$vl_data, outputId = 'verb_tbl')
    reloadData(proxy1, resetPaging = FALSE)
    
  }) 
  
  # Saved
  observe({

    print("rv$vls_data table data replaced")

    dataTableAjax(session, rv$vls_data, outputId = 'verb_tbl_saved')
    reloadData(proxy2, resetPaging = FALSE)



  })

  # Removed
  observe({

    print("rv$vlr_data table data replaced")
    
    dataTableAjax(session, rv$vlr_data, outputId = 'verb_tbl_rem')
    reloadData(proxy3, resetPaging = FALSE)

  })


####




