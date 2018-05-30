


# lildf <- barrios[1:100,]


dftoSPDF <- function(x){
  
  lildf <- x
  rownames(lildf) <- NULL
  
  boundlist <- NULL
  for(x in 1:length(lildf[,1]))
  {#x
    
    polyA <- lildf$poly[x]  
    bound0 <- unlist(str_split(polyA,"<D>")) 
    #bound0 <- bound0[-grep("NA",bound0,ignore.case = T)]
    
    for(i in 1:length(bound0))
    {#i
      # ###
      # parList[cnt] <- lildf$par[x]
      # cnt <- cnt + 1
      # ###
      
      boundA <- unlist(str_split(bound0[i]," ",n=Inf))
      boundB <- str_split_fixed(boundA,",",2)
      boundC <- as.matrix(cbind(as.numeric(boundB[,1]),as.numeric(boundB[,2])))
      boundC <- boundC[!rowSums(!is.finite(boundC)),]
      e <- sp::Polygons(list(sp::Polygon(boundC)),paste0(x))
      # e <- sp::Polygons(list(sp::Polygon(boundC)),paste0(rownames(lildf)[x]))

      
      boundlist <- c(boundlist,e)
    }#i
    
    
  }#x
  
  SPnewA <- SpatialPolygons(Srl = boundlist, pO = 1:length(boundlist))
  # SPnewA <- SpatialPolygons(Srl = boundlist, pO = as.numeric(rownames(lildf)))
  
  p <- SPnewA
  lildf$ID <- 1:length(p)
  
  p2 <- SpatialPolygonsDataFrame(p, lildf)
  
  return(p2)
  
}




















