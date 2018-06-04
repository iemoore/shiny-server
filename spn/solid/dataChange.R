

# Txt to RDS for exclude values
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