
# setwd("C:/Users/moore/Dropbox/R3/Shiny/shiny-server/spn")

library(shiny)
library(shinyBS)
library(shinyjs, quietly = T)
library(shinyalert)
library(shinyWidgets)
library(DT)
library(dplyr)
library(stringr)
library(purrr)
library(shinymaterial)

options(warn=1)



#...Data Load
verbdf <- readRDS("solid/data/rds/verbs501.rds")

verbMaster <- readRDS("solid/data/rds/verbMaster5-16.rds")

sentdf <- readRDS("solid/data/rds/sent501.4.30.rds")
  conjAct <- sentdf[,6]
  sentdf <- sentdf[,-c(4,6)]
  colnames(sentdf)[4] <- "tense"

# conjdf <- readRDS("solid/data/rds/conj501.4.25.rds")
conjdf <- readRDS("solid/data/rds/conjMaster5-15.rds")


vocabdf <- readRDS("solid/data/rds/vocab.4.30.rds")
  vocabdf <- vocabdf[,c(1,2,3,5,4)]
  vocabdf[which(vocabdf$theme1=="MiniList"),"theme1"] <-  "Category"

audioDf <- readRDS("solid/data/rds/audioDf.rds") 
  

audioWeb <- readRDS("solid/data/rds/audioWebSent.rds")
  audioWeb <- audioWeb[grepl(" ",audioWeb$spn),]
  verbWeb <- audioWeb[!grepl(" ",audioWeb$spn),]

  
dynamicTabs <- readLines(paste0("solid/html/tabs/dynamicTabs.txt"),warn = F)
verbTable <- readLines(paste0("solid/html/verbTable2.txt"),warn = F)    






#Change to reactive!!!!
tenseNow <- c("Indicative","Subjunctive","Imperative","Continuous","Perfect",
              "Perfect Subj")





#...Data Maniplulation
dim(verbdf);dim(sentdf);dim(conjdf)
conjdf$conj <- as.character(conjdf$conj)


sent_now <- data.frame(verb=NULL,spanish=NULL,english=NULL)

rv <- reactiveValues()
rv$icon_click <- F
rv$tab_click <- F
rv$clickNow <- ""

rv$sent_state <- NULL
rv$sent_start <- NULL
rv$sent_length <- NULL
count1 <- 1



#...Verb Cnjugation Tenses______________________________________________________
cj <- c("Yo","Tu","EL/Ella/Ud,","Nosotros","Vosotros","Ellos/Ellas/Uds.")
tenses <- c("Indicative","Subjunctive","Imperative","Continuous","Perfect",
            "Perfect Subj")
tenses2 <- c("Present","Preterite","Imperfect","Imperfect2","Conditional",
             "Future","Past","Affirmative","Negative")

tenseC <- list(c(1,2,3,5,6),c(1,3,4,6),c(8,9),c(1,2,3,5,6),c(1,2,3,5,6),c(1,7,6))
tenseC <- list(c(1,3,2,6,5),c(1,3,4,6),c(8,9),c(1,3,2,6,5),c(1,3,2,6,5),c(1,7,6))
#chainC <- list(c(1,30),c(31,54),c(55,64),c(65,94),c(95,124),c(125,142))
chainC <- list(c(3,32),c(33,56),c(57,66),c(67,96),c(97,126),c(127,144))



#Functions 

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
  
  
} 



























