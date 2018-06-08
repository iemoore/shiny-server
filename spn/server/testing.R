
####---------------------------------------------------------------------




observeEvent(input$modalNext,{
  
  pripas("input$modalNext clicked at ",Sys.time())
  
  a <- rv$sf_word_now 
  b0 <- rv$modal_data_ct
  
  rv$modal_data_ct <- b0 - 1
  pripas("rv$modal_data_ct <- ",rv$modal_data_ct)
  
  b <- rv$modal_data[rv$modal_data_ct,]
  
  rv$sf_word_now <- c(paste0(b$word,"-",b$verb,"-",b$tense),Sys.time())
  rv$sf_word_key1 <- c(1,Sys.time())
  
})

observeEvent(input$modalBack,{
  
  pripas("input$modalBack clicked at ",Sys.time())
  
  a <- rv$sf_word_now 
  b0 <- rv$modal_data_ct
  
  rv$modal_data_ct <- b0 + 1
  pripas("rv$modal_data_ct <- ",rv$modal_data_ct)
  
  b <- rv$modal_data[rv$modal_data_ct,]
  
  rv$sf_word_now <- c(paste0(b$word,"-",b$verb,"-",b$tense),Sys.time())
  rv$sf_word_key1 <- c(1,Sys.time())
  
})


































