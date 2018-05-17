


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
  
  pripas("input$flashClick_web[1] <- ",paste(c$conj," = ",c$popup,collapse = ", "))
  
  rv$sf_word_now <- b0
  
  toggleModal(session, "sf_Modal", toggle = "toggle")
  
})


output$sf_Modal_fill <- renderUI({
  
  
  h3(as.character(rv$sf_word_now))
  
  
})





sent2function <- function(f) {
  
  if(nchar(f[1,4])>0){
    
    conj1 <- unlist(str_split(f[1,4],", "))
    rv$conj1 <- conj1
    
    sent1 <- unlist(str_split(f[1,1]," "))
    
    for(h in 1:length(conj1)){
      
      a0 <- str_split_fixed(conj1[h],"-",3)
      a <- as.numeric(unlist(str_split(a0[1],"/")))
      cnum <- as.numeric(unlist(str_split(a0[3],"/")))
      
      if(cnum[1] == 0){
        
        c <- filter(verbMaster, spn==a0[2])
        pop <- c$eng
        
      } else {
        
        c <- filter(conjdf, verb==a0[2], tenseNum %in% cnum) 
        
        if(c$popup[1]=="Translation not available"){
          
          c2 <- filter(verbMaster, spn==a0[2])
          pop <- paste0(c$verb[1]," (",c2$eng,") <- ",paste(unique(c$tense),
                                                            collapse = "/"))
          
        } else{
          
          pop <- paste(c$popup,collapse = ", ")
        }
        
        
      }
      
      if(length(a)==1){
        
        sent1[a] <- paste0('<a id="gfw',h,'" data-row="',h,
                           '" class="go-flash-web" href="#" onClick="return false" data-',
                           'toggle="tooltip" title="',pop,'" data-placement="top">',
                           sent1[a],'</a>')
        
      } 
      
      if(length(a)>1 && (a[2]-a[1])==1) {
        
        sent1[a[1]] <- paste0('<a id="gfw',h,'" data-row="',h,'" class="go-',
                              'flash-web" href="#" onClick="return fals',
                              'e" data-toggle="tooltip" title="',pop,
                              '" data-placement="top" >',
                              paste(sent1[a],collapse = " "),'</a>')
        
        sent1[(a[1]+1):a[length(a)]] <- ""
        
        
      } else {
        
        sent1[a[1]] <- paste0('<a data-row="',h,
                              '" class="go-flash-web" href="#" onClick="return fals',
                              'e">',paste(sent1[a[1]],collapse = " "),'</a>')
        
        sent1[a[2]] <- paste0('<a data-row="',h,
                              '" class="go-flash-web" href="#" onClick="return fals',
                              'e">',paste(sent1[a[2]],collapse = " "),'</a>') 
        
        
      }
      
      
    }
    
    sent2 <- paste(sent1[which(nchar(sent1)>0)],collapse = " ") 
    return(sent2)
    
  } else {
    
    sent2 <- f[1,1]
    return(sent2)
    
  }
}


































