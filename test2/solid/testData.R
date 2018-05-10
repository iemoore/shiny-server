




a <- c(0,1,2,3,4,11, 12, 13, 14, 15, 16, 17, 18, 19, 20)


user <- "ian"

writeLines(paste(a,collapse = ","),
           con = paste0("solid/data/",user,"/savedVerbs.txt"))



rv$savedVerbs <- as.numeric(unlist(str_split(readLines(paste0("solid/data/",
                                              user,"/savedVerbs.txt")),",")))



rv <- list()
rv$remVerbs <- c(0,2,4,11,13,14,18)

writeLines(paste(rv$remVerbs,collapse = ","),
           con = paste0("solid/data/",user,"/remVerbs.txt"))

rv$remVerbs <- as.numeric(unlist(str_split(readLines(paste0("solid/data/",
                                                user,"/remVerbs.txt")),",")))
