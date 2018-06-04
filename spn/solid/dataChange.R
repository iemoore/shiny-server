

# 6-4-18 Txt to RDS for exclude values
#####-------------------------------------------------------------------------


de <- un_sp(readLines(paste0("userdata/ian/excludeE.txt")),",")
da <- un_sp(readLines(paste0("userdata/ian/excludeA.txt")),",")
dw <- un_sp(readLines(paste0("userdata/ian/excludeW.txt")),",")

de <- de[2:length(de)] %>% as.numeric()
da <- da[2:length(da)] %>% as.numeric()
dw <- dw[2:length(dw)] %>% as.numeric()


dedf <- data.frame(user="ian",type="E",time=Sys.time(),row=de)
dadf <- data.frame(user="ian",type="A",time=Sys.time(),row=da)
dwdf <- data.frame(user="ian",type="W",time=Sys.time(),row=dw)

dcdf <- rbind(dedf,dadf,dwdf)

saveRDS(dcdf,"userdata/Data.rds")

#####


# 6-4-18 Check sentence dfs for irreg conjugations
#####-------------------------------------------------------------------------

audioDf <- readRDS("solid/data/rds/audioAnkiSent.rds") 
audioDf$irregTF <- FALSE
audioDf$irregDet <- ""
for(i in 1:length(audioDf[[1]])){
  
  a <- un_sp(audioDf$conj[i],",")
  uniqDet <- NULL
  
  for(j in 1:length(a)){
    
    b0 <- un_spf(a[j],"-",3)
    b1 <- un_sp(b0[3],"/") %>% as.numeric()
    
    for(k in 1:length(b1)){
      
      c <- filter(conjdf,verb==b0[2],tenseNum==b1[k])
      
      if(length(c[[1]])==1){
        
        if(nchar(c$irreg)>0){
          
          audioDf$irregTF[i] <- TRUE
          # audioDf$irregDet[i] <- ifelse(nchar(audioDf$irregDet[i])==0,b0[1],
          #                               paste0(audioDf$irregDet[i],",",b0[1]))
          uniqDet <- c(uniqDet,b0[1])
          
        }
        
      }
      
    }
    
  }
  
  audioDf$irregDet[i] <- paste(unique(uniqDet),collapse = ",")
  
}

audioWeb <- readRDS("solid/data/rds/audioWebSent.rds")
audioWeb <- audioWeb[grepl(" ",audioWeb$spn),]
audioWeb$irregTF <- FALSE
audioWeb$irregDet <- ""
for(i in 1:length(audioWeb[[1]])){
  
  a <- un_sp(audioWeb$conj[i],",")
  uniqDet <- NULL
  
  for(j in 1:length(a)){
    
    b0 <- un_spf(a[j],"-",3)
    b1 <- un_sp(b0[3],"/") %>% as.numeric()
    
    for(k in 1:length(b1)){
      
      c <- filter(conjdf,verb==b0[2],tenseNum==b1[k])
      
      if(length(c[[1]])==1){
        
        if(nchar(c$irreg)>0){
          
          audioWeb$irregTF[i] <- TRUE
          # audioWeb$irregDet[i] <- ifelse(nchar(audioWeb$irregDet[i])==0,b0[1],
          #                               paste0(audioWeb$irregDet[i],",",b0[1]))
          uniqDet <- c(uniqDet,b0[1])
          
        }
        
      }
      
    }
    
  }
  
  audioWeb$irregDet[i] <- paste(unique(uniqDet),collapse = ",")
  
}

library(beepr)
beep(7)

saveRDS(audioDf,"solid/data/rds/audioAnkiSent6-4.rds")
saveRDS(audioWeb,"solid/data/rds/audioWebSent6-4.rds")
#####






















