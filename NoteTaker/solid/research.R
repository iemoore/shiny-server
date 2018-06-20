
#6-20-18 10:43am
a <- readRDS("noteMaster.rds") # taken from server
glimpse(a)
# a$body <- as.character(a$body)
a$time <- as.POSIXct(a$time,tz="US/Central")

b <- a[!validUTF8(a$body),]
dim(b)
c <- b$body[1]
Encoding(c)
# Encoding(c) <- "UTF-8"
# c
# Encoding(c) <- "latin1"
# c
# Encoding(a$body) <- "UTF-8"\

d <- iconv(c,"WINDOWS-1252","UTF-8")

a$body <- iconv(a$body,"WINDOWS-1252","UTF-8")

saveRDS(a,"solid/rds/noteMaster.rds")
