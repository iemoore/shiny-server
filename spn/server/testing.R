

observeEvent(rv$sf_word_now,{
  
  a <- "2-obsequiar-6/16"
  
  a <- rv$sf_word_now[1]
  
  if(rv$sf_word_key1[1]==0){
    
    pripas("rv$sf_word_key1[1]==0")
    
    word1 <- str_split_fixed(a,"-",3)[,1]
    verb1 <- str_split_fixed(a,"-",3)[,2]
    tense1 <- str_split_fixed(a,"-",3)[,3] 
    # tense1 <-  un_sp(tense1,"/") %>% as.numeric()
    
    conj1 <- rv$sf_word_now[2]
    
    lildf <- data.frame(user=USER$name,time=Sys.time(),word=word1,verb=verb1,
                        conj=conj1,tense=tense1,type=rv$tNow,row=tail(rv$exNow,n=1))
    
    
    rv$modal_data <- rbind(lildf,rv$modal_data)
    saveRDS(rv$modal_data,paste0("userdata/modal_data.rds"))
    
  } 
  
  
})

####---------------------------------------------------------------------


output$sf_dataModal_DF <- DT::renderDataTable({
  if (USER$Logged == TRUE) {
    
    pripas("Data modal activated at ",Sys.time())
    
    # input$actDataModal
    
    s2 <- isolate(rv$exData)
    d2 <- isolate(rv$dfNow)
    d2$row2 <- rownames(d2)
    
    s3 <- merge(s2, d2, by.x = "row", by.y = "row2")
    
    s2 <- s3[,c("row","time","spn","eng")]
    s2 <- arrange(s2, desc(time))
    
    pripas("dim(s2)[1] <- ",dim(s2)[1])
    
     # action <- DT::dataTableAjax(session, s2)
    
    DT::datatable(s2, selection = list(mode = "single", target = "row"),
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
proxy2 = dataTableProxy('sf_dataModal_DF')
observe({
  
  input$actDataModal
  s2 <- isolate(rv$exData)
  d2 <- isolate(rv$dfNow)
  d2$row2 <- rownames(d2)
  
  if(!is.null(s2)){
    
    s3 <- merge(s2, d2, by.x = "row", by.y = "row2")
    s2 <- s3[,c("row","time","spn","eng")]
    s2 <- arrange(s2, desc(time))
    
    dataTableAjax(session, s2, outputId = 'sf_dataModal_DF')
    reloadData(proxy2, resetPaging = FALSE)    
  }
  
})
observeEvent(input$sf_dataModal_DF_rows_selected,{
  
  a <- input$sf_dataModal_DF_rows_selected
  
  toggleModal(session, "sf_dataModal", toggle = "toggle")
  
  # rv$sf_word_now <- c(b0,c2)
  
  
})



output$sf_popHistModal_DF <- DT::renderDataTable({
  if (USER$Logged == TRUE) {
    
    pripas("Modal History modal activated at ",Sys.time())
    
    # input$actPopHistModal
    
    s2 <- isolate(rv$modal_data)
    
    if(!is.null(s2)){
      
      DT::datatable(s2, selection = list(mode = "single", target = "row"),
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
proxy1 = dataTableProxy('sf_popHistModal_DF')
observe({
  
  input$actPopHistModal
  
  dataTableAjax(session, isolate(rv$modal_data), outputId = 'sf_popHistModal_DF')
  reloadData(proxy1, resetPaging = FALSE)
  
})
observeEvent(input$sf_popHistModal_DF_rows_selected,{
  
  a <- input$sf_popHistModal_DF_rows_selected
  
  pripas("popHistModal row <- ",a)
    
  b <-  rv$modal_data[a,]
  
  toggleModal(session, "sf_popHistModal", toggle = "toggle")
  
  
  rv$sf_word_now <- c(paste0(b$word,"-",b$verb,"-",b$tense),Sys.time())
  rv$sf_word_key1 <- c(1,Sys.time())
  
  # shinyjs:click('sfm_trigger')
  
  pripas("rv$sf_word_now <- ",rv$sf_word_now[1])
  
  # Sys.sleep(1)
  
  toggleModal(session, "sf_Modal", toggle = "toggle")
  
})



observeEvent(rv$sf_word_key2,{

  if(rv$sf_word_key2==1){
    
    pripas("opening modal at ",Sys.time())
    toggleModal(session, "sf_Modal",toggle = "open")
    rv$sf_word_key2 <- 0
    
  }
  
})




















