
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
  # a <- length(rv$exData[[1]])-a+1
  # b <- rv$exData
  
  s2 <- arrange(rv$exData, desc(time))
  a <- s2[a,"row"]
  
  pripas("rows_selected <- ",a)
  
  toggleModal(session, "sf_dataModal", toggle = "toggle")
  
  b <- grep(paste0("\\b",a,"\\b"),rv$exNow)
  pripas("b <- ",b)
  
  rv$exNow_ct <- b-1
  
  rv$nextCt2 <- rv$nextCt2 + 1
  
})









































