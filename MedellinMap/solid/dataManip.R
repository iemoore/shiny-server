



barrios <- readRDS(paste0("solid/rds/Barrios.rds"))

###

espac <- readRDS(paste0("solid/rds/EspacioPublico.rds"))
colnames(espac)[1] <- "ID"

###

domic <- readRDS(paste0("solid/rds/NomenDomic.rds"))
colnames(domic)[c(1,2)] <- c("lng","lat")
domic <- domic[complete.cases(domic$lat), ]
domic <- domic[complete.cases(domic$lng), ]

###

usos <- readRDS(paste0("solid/rds/UsosUrbano.rds"))
for(i in 1:length(usos[[1]])){
  
  a <- un_sp(usos$poly[i]," ") %>% 
        str_split_fixed(pattern = ",",n=2)
  
  usos$lngmean[i] <- mean(as.numeric(range(a[,1])))
  usos$latmean[i] <- mean(as.numeric(range(a[,2])))
  
}
saveRDS(usos,"solid/rds/UsosUrbano5-29.rds")

###

vias <- readRDS(paste0("solid/rds/ViasUrbano.rds"))





























