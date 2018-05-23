
sent2function <- function(f) {
  
  if(nchar(f[["conj"]])>0){
    
    conj1 <- unlist(str_split(f[["conj"]],", "))
    rv$conj1 <- conj1
    
    sent1 <- unlist(str_split(f[["spn"]]," "))
    
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
  
  a <- rv$sf_word_now
  # a <- "1/2-faltar-73"
  # a <- "1-convocar-73"
  
  
  # <dl>
  #   <dt>Coffee</dt><dd>- black hot drink</dd>
  #   <dt>Milk</dt><dd>- white cold drink</dd>
  # </dl>
  

  b <- filter(dictdf,verb==split_between(a,"-","-"))
  dim(b)
  
  if(length(b[[1]])>0){
    
    b1 <- unique(b$ctx)
    
    h1 <- paste0("The verb ",b$verb[1]," has ",length(b1),
                 " unique context",ifelse(length(b1)==1,"","s"))
    
    list3 <- NULL
    for(i in 1:length(b1)){
      
      b2 <- filter(b,ctx==b1[i])
      b3 <- paste0("<b>(",b2$mean,")</b> ",b2$spn," ... ",b2$eng)
      
      b4 <- paste0("<dt><u>",b1[i],"</u>...</dt><dd>",paste(b3,
                  collapse = "<br><br>"),"</dd>")
      
      list3 <- paste0(list3,"<dl>",b4,"</dl><br>")
    }
    
    
    
    
    # list2 <- paste0("<dl>",paste("<dt>",b$ctx,"  (",b$mean,")</dt><dd>",b$spn," - ",
    #                              b$eng,"</dd>",collapse = "<br>"),"</dl>")
    # 
    
    return(HTML(paste0(h3(h1),list3)))
    
  }
  
  
  
  
})


observeEvent(rv$sf_word_now, {
  
  #print("Verb tbl row selected")
  # removeUI("#conjUI2")
  # 
  # r <- "1/2-faltar-73"
  # r <- rv$sf_word_now
  # 
  #   if(length(r)>0){
  #     
  #     source("server/Fun/verbTable.R",local = T)
  #     verbTable_fun(r)
  #     
  #     n <- dynamicTabs
  #     h1<-n[1];h4<-n[4];b1<-n[6];b4<-n[9]
  #     h2 <- "";h3<-"";b2<-"";b3<-""
  #     h2 <- sub("Home",verbdf$verb[r[1]],n[2])
  #     if(length(r)>1){
  #       for(i in 2:length(r)){
  #         
  #         h <- gsub("Menu 1",verbdf$verb[r[i]],n[3])
  #         h <- sub("menu1",paste0("menu",i),h)
  #         h3 <- c(h3,h)
  #         
  #       }
  #     }
  #     
  #     b2 <- sub("home content",a[1],n[7])  
  #     if(length(r)>1){
  #       for(i in 2:length(r)){
  #         
  #         b <- sub("menu1",paste0("menu",i),n[8])
  #         b <- sub("tab content",a[i],b)
  #         b3 <- c(b3,b)
  #         
  #       }
  #     }
  #     
  #     f <- c(h1,h2,h3,h4,b1,b2,b3,b4)
  #     
  #     ui_build <- tags$div(
  #       id= "conjUI",
  #       HTML(f)
  #     )  
  #     
  #     insertUI(
  #       selector = "#hereModal1",
  #       where = "afterEnd",
  #       ui = ui_build
  #     )
  #   }
  #   
  #   
})






























