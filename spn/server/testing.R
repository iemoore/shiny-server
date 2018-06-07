

####---------------------------------------------------------------------

rv$sf_word_now <- NULL


observeEvent(input$flashClick_web,{
  
  a <- input$flashClick_web[1]
  
  # pripas("input$flashClick_web[1] <- ",rv$conj1[as.numeric(a)])
  
  b0 <- rv$conj1[as.numeric(a)]
  
  b <- as.character(unlist(str_split_fixed(b0,"-",3)))
  
  word1 <- b[1]
  verb1 <- b[2]
  tense1 <- unlist(str_split(b[3],"/"))
  
  c <- filter(conjdf, verb==verb1, tenseNum %in% tense1)
  
  pripas(b0," <- ",paste(c$conj," = ",c$popup,collapse = ", "))
  
  rv$sf_word_now <- b0
  
  toggleModal(session, "sf_Modal", toggle = "toggle")
  
})


output$sf_Modal_fill <- renderUI({
  
  # a <- "1/2-faltar-73"
  # a <- "1-convocar-73"
  # a <- "1/2-alquilar-112"
  a <- "2-obsequiar-6/16"
  
  a <- rv$sf_word_now

  
  word1 <- str_split_fixed(a,"-",3)[,1]
  verb1 <- str_split_fixed(a,"-",3)[,2]
  tense1 <- str_split_fixed(a,"-",3)[,3] 
  tense1 <-  un_sp(tense1,"/") %>% as.numeric()
  
  n <- filter(verbMaster,spn==verb1)
  head <- h3(tags$b(paste0(toupper(n$spn)," - ",n$eng))) 
  
  
  b <- filter(dictdf,verb==verb1)
  dim(b)
  
  if(length(b[[1]])>0){
    
    b1 <- unique(b$ctx)
    
    h1 <- paste0("The verb ",b$verb[1]," has ",length(b1),
                 " unique context",ifelse(length(b1)==1,"","s"))
    
    table1 <- ifelse(as.numeric(tense1[1])==0,"",paste0(verbToTable(a),"<br>"))
    # table1 <- verbToTable(a)
    
    list3 <- NULL
    for(i in 1:length(b1)){
      
      b2 <- filter(b,ctx==b1[i])
      b3 <- paste0("<b>(",b2$mean,")</b> ",b2$spn," ... ",b2$eng)
      
      b4 <- paste0("<dt><u>",toupper(b1[i]),"</u>...</dt><br><dd>",paste(b3,
                  collapse = "<br><br>"),"</dd>")
      
      list3 <- paste0(list3,"<dl>",b4,"</dl><br>")
    }
    # <center>
    
    return(HTML(paste0("<center>",tags$u(head),"</center>",table1,
                       h3(tags$b(h1)),list3)))
    
  }
  
  
})



output$sf_dataModal_fill <- DT::renderDataTable({
  if (USER$Logged == TRUE) {
    
    pripas("Data modal activated at ",Sys.time())
    
    input$actDataModal
    
    s2 <- isolate(rv$exData)
    d2 <- isolate(rv$dfNow)
    d2$row2 <- rownames(d2)
    
    s3 <- merge(s2, d2, by.x = "row", by.y = "row2")
    
    s2 <- s3[,c("row","time","spn","eng")]
    s2 <- arrange(s2, desc(time))
    
    pripas("dim(s2)[1] <- ",dim(s2)[1])
    
     # action <- DT::dataTableAjax(session, s2)
    
    DT::datatable(s2, selection = list(mode = "none", target = "row"),
    options = list(#ajax = list(url = action), 
      processing = FALSE,
      deferRender = TRUE,
      scrollX=TRUE,
      # columnDefs = list(list(width = '15%', targets = list(1,2)),
      #                   list(className = 'dt-center', targets = c(0,1,3,4))),
      
      lengthMenu = c(5,10, 20, 50, 100),
      language = list(search = 'Filter:')
    ), 
    # rownames=FALSE,
    escape = FALSE,  
    editable = T
    ) 
    
  }
})






























