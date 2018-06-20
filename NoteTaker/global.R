
# setwd("C:/Users/moore/Dropbox/R3/Shiny/shiny-server/NoteTaker")


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

options(warn=2)
options(shiny.error=recover)
options(shiny.sanitize.errors=F)
options(shiny.reactlog=TRUE)

dfNow <- "noteMaster"

#...Functions
#####-------------------------------------------------------------------

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


#####

#...Data Load
#####-------------------------------------------------------------------


notedf <- readRDS(paste0("solid/rds/",dfNow,".rds"))
notedf$time <- as.POSIXct(notedf$time,tz="US/Central")
notedf$time <- paste(as.character(notedf$time), "CTZ")
# Encoding(notedf$body) <- "UTF-8"
# notedf$body <- enc2utf8(notedf$body)
notedf$body <- iconv(notedf$body,"WINDOWS-1252","UTF-8")

#####

#...RV's
#####-------------------------------------------------------------------

rv <- reactiveValues()
rv$nt_start <- 0
rv$nt_length <- 10

# rv$typeNow <- unique(notedf$type)
# rv$typePicked <- unique(notedf$type)
rv$typeNow <- as.character(unique(readRDS(paste0("solid/rds/",dfNow,".rds"))$type))
rv$typePicked <- as.character(unique(readRDS(paste0("solid/rds/",dfNow,".rds"))$type))
rv$pickedNow <- "Spanish"

#####





















