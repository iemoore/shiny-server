
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

verbToTable <- function(a){
  
  word1 <- str_split_fixed(a,"-",3)[,1]
  verb1 <- str_split_fixed(a,"-",3)[,2]
  tense1 <- str_split_fixed(a,"-",3)[,3] 
  tense1 <-  un_sp(tense1,"/") %>% as.numeric()
  
  d <- filter(conjdf, verb==verb1)
  # dim(d)
  
  table1 <- NULL
  
  for(g in 1:length(tense1)){
    
    dm <-  filter(d, tenseNum==tense1[g])
    
    tm <- 0
    
    if(tense1[g] %in% c(1,2)) {
      
      table1[g] <- h2(paste0(d[tense1[g],"tense"]," - ",d[tense1[g],"conj"]))
      pripas(table1[g])
      
      next()
    }
    
    for(k in 1:length(chainC)){
      
      a1 <- chainC[[k]]
      
      if(tense1[g] %in% seq(a1[1],a1[2],1)){
        
        pripas("Match at ",tenses[k])
        
        tm <- k
        
      }
    }
    
    b <- chainC[[tm]]
    
    d2 <- d[which(d$tenseNum %in% seq(b[1],b[2],1)),] 
    mn <- grep(dm$tenseNum,d2$tenseNum)
    d2$conj[mn] <- paste0("<font color=\"red\">",d2$conj[mn],"</font>")
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
    
    table1[g] <- paste(h4(tags$b(paste0(tenses[tm]," Tense:"))),
                       "<table style=\"width:100%\">",h1,r,"</table>")
  
  }
  
  
  return(paste(table1,collapse = "<br>"))
  
}



