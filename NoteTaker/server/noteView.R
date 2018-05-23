

#...DataTable
#####----------------------------------------------------------

output$note_tbl <- DT::renderDataTable({
    
    s2 <- notedf
    pripas("RenderDataTable called at ",Sys.time())
    
    
    # action <- DT::dataTableAjax(session, s2)
    
    DT::datatable(s2, selection = list(mode = "single", target = "row"),
    options = list(#ajax = list(url = action),
      displayStart = isolate(rv$nt_start),
      pageLength = isolate(rv$nt_length),
      processing = FALSE,
      deferRender = TRUE,
      # scrollCollapse=TRUE,
      scrollX=TRUE,
      lengthMenu = c(10, 20, 50, 100),
      language = list(search = 'Filter:')
    ),
    # rownames=FALSE,
    escape = FALSE,
    editable = T
    )
    
    
})

proxy1 = dataTableProxy('note_tbl')


observe({

  pripas("dataTable proxy called at ",Sys.time())
  # pripas("dim(rv$noteData)[1] <- ",dim(rv$noteData)[1])
  
  
  s1 <- rv$noteData[which(rv$noteData$type %in% rv$typePicked),]
  
  dataTableAjax(session, s1, outputId = 'note_tbl')
  reloadData(proxy1, resetPaging = FALSE)

})

#####


















