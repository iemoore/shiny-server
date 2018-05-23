
# setwd("C:/Users/moore/Dropbox/R3/Shiny/shiny-server/SentHelp")


library(shiny)
library(shinyBS)
library(shinyjs, quietly = T)
library(shinydashboard)
library(shinyalert)
library(shinyWidgets)
library(DT)
library(dplyr)
library(stringr)
library(purrr)

options(warn=1)

#...Data Load
#####
which_data <- list("Websters 2440"="W", "Anki 7627"="A", "Dictionary 10275"="D",
                   "123 TeachMe"="T")


tense_Names <- c("Infinitive","Present Participle","Past Participle","Indicative",
                 "Subjunctive","Imperative","Continuous","Perfect",
                 "Perfect Subj")
tense_Names2 <- list("Infinitive"=1,"Present Participle"=2,"Past Participle"=3,
                     "Indicative"=4,"Subjunctive"=5,"Imperative"=6,
                     "Continuous"=7,"Perfect"=8,"Perfect Subj"=9)

tense_Nums <- list(0,1,2,c(3,32),c(33,56),c(57,66),c(67,96),c(97,126),c(127,144))


# audioDf <- readRDS(paste0("C:/Users/moore/Dropbox/R3/Shiny/shiny-server/spn/",
#                           "solid/data/rds/audioAnkiSent.rds")) 
#   audioDf$cType <- "A"
# audioWeb <- readRDS(paste0("C:/Users/moore/Dropbox/R3/Shiny/shiny-server/spn/",
#                            "solid/data/rds/audioWebSent.rds"))
#   audioWeb$cType <- "W"
# dictdf <- readRDS(paste0("C:/Users/moore/Dropbox/R3/Shiny/shiny-server/spn/",
#                          "solid/data/rds/dictMaster5-19.rds"))[,c("spn","eng","conj")]
#   dictdf$num1 <- numeric(length = length(dictdf[[1]]))
#   dictdf$cType <- "D"
# sentdf <- rbind(audioWeb,audioDf,dictdf)
# sentdf$conjUniq <- character(length = length(sentdf[[1]]))
# sentdf$Infin <- 0
# sentdf$PrePar <- 0
# sentdf$PasPar <- 0
# sentdf$Indic <- 0
# sentdf$Subjun <- 0
# sentdf$Imper <- 0
# sentdf$Contin <- 0
# sentdf$Perfec <- 0
# sentdf$PerSub <- 0
# for(i in 1:length(sentdf[[1]])){
# 
#   a <- sentdf[i,"conj"] %>%
#         str_split(pattern = ", ") %>%
#         unlist() %>%
#         str_split_fixed(pattern = "-",3)
# 
#   a1 <- a[,3]
# 
#   b <- unique(unlist(str_split(a1,"/"))) %>%
#         as.numeric()
#   b1 <- b[order(b)]
#   t1 <- tense_Nums
# 
#   if(nchar(a1[1])>0){
# 
#     for(j in b1){
# 
#     if(j==0){ sentdf$Infin[i] <- 1}
#     if(j==1){ sentdf$PrePar[i] <- 1}
#     if(j==2){ sentdf$PasPar[i] <- 1}
#     if(between(j,t1[[4]][1],t1[[4]][2])){ sentdf$Indic[i] <- 1}
#     if(between(j,t1[[5]][1],t1[[5]][2])){ sentdf$Subjun[i] <- 1}
#     if(between(j,t1[[6]][1],t1[[6]][2])){ sentdf$Imper[i] <- 1}
#     if(between(j,t1[[7]][1],t1[[7]][2])){ sentdf$Contin[i] <- 1}
#     if(between(j,t1[[8]][1],t1[[8]][2])){ sentdf$Perfec[i] <- 1}
#     if(between(j,t1[[9]][1],t1[[9]][2])){ sentdf$PerSub[i] <- 1}
# 
#     }
# 
#   }
# 
# }
# saveRDS(sentdf,paste0("C:/Users/moore/Dropbox/R3/Shiny/shiny-server/",
#                       "SentHelp/solid/sentdf.rds"))


sentdf <- readRDS(paste0("solid/data/sentdf.rds"))
rownames(sentdf) <- NULL
sentdf$x <- rownames(sentdf)

conjdf <- readRDS(paste0("solid/data/conjMaster5-15.rds"))

verbMaster <- readRDS(paste0("solid/data/verbMaster5-16.rds"))
#####

#...Functions
#####

pripas <- function(...){
  
  
  print(paste0(...))
  
}

inlineDing <- function(x){
  
  div(style="display: inline-block;vertical-align:top;",list(x))
  
}

un_sp <- function(x,by){
  
  
  unlist(str_split(x,by))
}

un_spf <- function(x,by,num){
  
  
  unlist(str_split_fixed(x,by,num))
}

split_between <- function(x,pat1,pat2){
  
  require(stringr)
  a <- str_split_fixed(x,pat1,2)[2]
  b <- str_split_fixed(a,pat2,2)[1]
  
  return(b)
} 

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

#####

#...RV's
#####

USER <- reactiveValues(Logged=T)

rv <- reactiveValues()
rv$audioNow <- NULL
rv$autoplay <- T

rv$backCt <- 0
rv$nextCt <- 0
rv$exclude <- c(1,2,3,4,5)
rv$exclude_ct <- length(c(1,2,3,4,5))
# rv$now <- NULL
# rv$now_ct <- 0

#####























