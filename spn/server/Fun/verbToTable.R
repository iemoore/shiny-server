
# <table style="width:100%">
#   <tr>
#   <th>Firstname</th>
#   <th>Lastname</th> 
#   <th>Age</th>
#   </tr>
#   <tr>
#   <td>Jill</td>
#   <td>Smith</td> 
#   <td>50</td>
#   </tr>
#   <tr>
#   <td>Eve</td>
#   <td>Jackson</td> 
#   <td>94</td>
#   </tr>
# </table>

# a <- "1/2-faltar-73"
# a <- "1-convocar-73"
# a <- "1/2-alquilar-112"
# a <- "5-ayudar-29"
# a <- "2/3-agrandar-114"
# a <- "2-querer-13/57"
a <- "2/3-rescatar-143"
a <- "6-viajar-33/41/59"
a <- "3/4-fijarse-13"
a <- "2-prohibir-13/57"
a <- "4-promover-13/57"
a <- "2-obsequiar-6/16"
a <- "4-sonar-16"

verbToTable <- function(a){
  
  word1 <- str_split_fixed(a,"-",3)[,1]
  verb1 <- str_split_fixed(a,"-",3)[,2]
  tense1 <- str_split_fixed(a,"-",3)[,3]
  tense1 <-  un_sp(tense1,"/") %>% as.numeric()
  
  d <- filter(conjdf, verb==verb1)
  table1 <- NULL
  
  
  tmdf <- data.frame(ten1 = tense1, tm = character(length=length(tense1)),
                    stringsAsFactors = F)
  
  for(f in 1:length(tense1)){
  
    for(k in 1:length(chainC)){
      
      a1 <- chainC[[k]]
      if(tense1[f] %in% seq(a1[1],a1[2],1)){ tm <- k }
      
    }
    
    tmdf$tm[f] <- tm
  }
  
  unqt <- unique(tmdf$tm)
  
  for(g in 1:length(unqt)){
    
    # dm <-  filter(d, tenseNum==tense1[g])
    
    dm <- d[which(d$tenseNum %in% tmdf[which(tmdf$tm %in% unqt[g]),"ten1"]),]
    
    tm <- unqt[g] %>% as.numeric()
    
    if(dm$tenseNum[1] %in% c(1,2)) {
      
      table1[g] <- h2(paste0(d[tense1[g],"tense"]," - ",d[tense1[g],"conj"]))
      pripas(table1[g])
      
      next()
    }
    

    b <- chainC[[tm]]
    
    d2 <- d[which(d$tenseNum %in% seq(b[1],b[2],1)),] 
    #-------------------------------------------------
    
    d3 <- d2[which(nchar(d2$irreg)>0),]
    rownames(d3) <- NULL
    
    d3l <- length(d3[[1]])
    if(d3l>0){
      for(w in 1:length(d3[[1]])){
        
        # d2$conj[d3$tenseNum[i]] <- d3$irreg[i]
        
        a1 <- which(d2$tenseNum==d3$tenseNum[w])
        
        res1 <- d3$irreg[w]
        res1 <- sub("\'","<font color=\"blue\">",res1)
        res1 <- sub("\'","</font>",res1)
        
        
        d2[a1,"conj"] <- res1
        
      }
    }
    
    
    
    
    
    #-------------------------------------------------
    
    mn <- NULL
    for(y in 1:length(dm[[1]])){
      mn[y] <- grep(paste0("\\b",dm$tenseNum[y],"\\b"),d2$tenseNum)
    }
    # mn <- grep(dm$tenseNum,d2$tenseNum)
    
    d2$conj[mn] <- paste0("<font color=\"red\"><b>",d2$conj[mn],"<b></font>")
    dim(d2)
    
  
    #...Build Header
    h1 <- paste0("<th></th>")    
    for(k in 1:length(tenseC[[tm]])){ 
      h1 <- paste0(h1,"<th>",tenses2[tenseC[[tm]][k]],"</th>")
    }
    h1 <- paste0("<tr>",h1,"</tr>")
    
    
    #...Build Rows
    len1 <- dim(d2)[1]/ifelse(tm==3,5,6)
    r <- NULL
    for(n in ifelse(tm==3,2,1):length(cj)){
      
      ifelse(tm %in% c(1,4),
             rowlist <- c(1,3,2,5,4),
             rowlist <- c(1:length(tenseC[[tm]])))
      
      b1 <- ifelse(tm==3,2,1)
      w1 <- d2$conj[(1+((n-b1)*len1)):(len1+((n-b1)*len1))]
      w1 <- w1[rowlist]
      
      
      r0 <- paste0("<td>",c(cj[n],w1),"</td>")
      
      r <- paste0(r,"<tr>",paste(r0,collapse = ""),"</tr>")
    }
    
    # p(paste0(tenses[tm]," Tense:"))
    
    tran1 <- ifelse(dm$popup[1]=="Translation not available","",
                    paste0(" - ",paste(dm$popup,collapse = ", ")))
    
    
    title0 <- h3(HTML(paste0("<b><center>",dm$conj[1],tran1,"</b></center>")))
    title1 <- paste0(tenses[tm]," Tense ")
    
    table1[g] <- paste(title0,"<br>",h4(tags$b(title1)),
                       "<table style=\"width:100%\">",h1,r,"</table>")
  
  }
  
  
  return(paste(table1,collapse = "<br>"))
  
}



