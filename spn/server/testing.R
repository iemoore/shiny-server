

####---------------------------------------------------------------------





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



output$sf_popHistModal_fill <- DT::renderDataTable({
  if (USER$Logged == TRUE) {
    
    pripas("Modal History modal activated at ",Sys.time())
    
    input$actPopHistModal
    
    s2 <- isolate(rv$modal_data)
    
    if(!is.null(s2)){
      
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
                    editable = T)       
    }
    

    
  }
})


observeEvent(rv$sf_word_now,{
  
  a <- "2-obsequiar-6/16"
  
  a <- rv$sf_word_now[1]
  word1 <- str_split_fixed(a,"-",3)[,1]
  verb1 <- str_split_fixed(a,"-",3)[,2]
  tense1 <- str_split_fixed(a,"-",3)[,3] 
  # tense1 <-  un_sp(tense1,"/") %>% as.numeric()
  
  conj1 <- rv$sf_word_now[2]
  
  lildf <- data.frame(user=USER$name,time=Sys.time(),word=word1,verb=verb1,
                      conj=conj1,tense=tense1,type=rv$tNow,row=tail(rv$exNow,n=1))
  
  
  rv$modal_data <- rbind(lildf,rv$modal_data)
  saveRDS(rv$modal_data,paste0("userdata/modal_data.rds"))
  
  
})





















