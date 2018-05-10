

a1 <- conjdf$conj[r]
a <- NULL
t1 <- paste(verbTable[1:2],collapse = "")
t2 <- paste(verbTable[4],collapse = "")
t3 <- paste(verbTable[8],collapse = "")

# istop <- length(a1)
# jstop <- length(tenses)
# i=1;j=1;k=1;l=1;m=1


#...Verb, 
for(i in 1:length(a1)){ 
  
  v1 <- verbdf$verb[r[i]]
  vm <- verbdf$meaning[r[i]]
  
  
  b <- unlist(str_split(a1[i],","))#[3:144]
  b2 <- as.numeric(unlist(str_split(conjdf$uniqConj[r[i]],",")))
  tz <- NULL

  #...Tense
  for(j in 1:length(tenses)) { if(j %in% input$checkTense){
    
    
    t <- tenses[i] 
    
    #...Build Header
    h1 <- paste0("<th></th>")    
    for(k in 1:length(tenseC[[j]])){ 
      h1 <- paste0(h1,"<th>",tenses2[tenseC[[j]][k]],"</th>")
    }
    
    #...Build Rows
    rc <- NULL
    lstart <- ifelse(j==3,2,1)
    for(l in lstart:6){
      
      r1 <- paste0("<td>",cj[l],"</td>")
      
      ifelse(j %in% c(1,4,5),
             rowlist <- c(1,3,2,5,4),
             rowlist <- c(1:length(tenseC[[j]])))
      
      # for(m in 1:length(tenseC[[j]])){
      for(m in rowlist){
        
        num1 <- (chainC[[j]][1] + (m-1) + 
                   (l-2)*length(tenseC[[j]]))
        num2 <- (chainC[[j]][1] + (m-1) + 
                   (l-1)*length(tenseC[[j]]))
        numF <- ifelse(j==3,num1,num2)
        
        #...Decide whether to make span or link
        vt <- ifelse(!(numF %in% b2),verbTable[12],verbTable[6])
        
        verbrow <- ifelse(j==3,
                      sub("ID1",paste0("mydiv",letters[i],num1),vt),
                      sub("ID1",paste0("mydiv",letters[i],num2),vt))

        r1 <- ifelse(j==3,
                paste(r1,sub("Cell",enc2native(b[num1]),verbrow),collapse = ""), 
                paste(r1,sub("Cell",enc2native(b[num2]),verbrow),collapse = ""))
        
        } 
      
     rc <- paste(rc,"<tr>",r1,"</tr>",collapse = "") 
      
    }
    
    #...Combine pieces
    tz2 <- paste(t1,h1,t2,rc,t3)
    tz <- paste(tz,tags$h3(tenses[j]),tz2,collapse = "")

  
  }}
  
  
  #...Add Past/Present Participles
  if(7 %in% input$checkTense){
    
    if(!(1 %in% b2)){
      p1 <- sub("ID1",paste0("mydiv",letters[i],"1"),sub("Cell",enc2native(b[1]),
                  paste0("<br/><span \"id=\"ID1\">Present Participle : ",
                         "Cell</span><br/>")))
      } else{
        p1 <- sub("ID1",paste0("mydiv",letters[i],"1"),sub("Cell",enc2native(b[1]),
                  paste0("<br/><span >Present Participle : <a href=\"#\" ",
                  "id=\"ID1\" >Cell</a></span><br/>")))
        }


    if(!(2 %in% b2)){
      p2 <- sub("ID1",paste0("mydiv",letters[i],"2"),sub("Cell",enc2native(b[2]),
                  paste0("<br/><span \"id=\"ID1\">Past Participle : ",
                  "Cell</span><br/>")))
      } else{
      p2 <- sub("ID1",paste0("mydiv",letters[i],"2"),sub("Cell",enc2native(b[2]),
                  paste0("<br/><span >Past Participle : <a href=\"#\" ",
                  "id=\"ID1\" >Cell</a></span><br/>")))
      }
    
    
    a[i] <- paste(p1,p2,tz,collapse = "")
  
  } 
  else {
    
    a[i] <-tz
  }
  
  
  #...Add verb meaning as header
  a[i] <- paste0(tags$br(),h1(paste0(v1," - ",vm)),a[i])

}




















