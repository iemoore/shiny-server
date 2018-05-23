
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

options(warn=1)



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

# notedf <- readRDS("solid/rds/noteMaster5-23.rds")
# notedf$type <- "Spanish"
# saveRDS(notedf,"solid/rds/noteMaster.rds")
notedf <- readRDS("solid/rds/noteMaster1.rds")

# n <- readRDS("solid/noteMaster.rds")

#####

#...RV's
#####-------------------------------------------------------------------

rv <- reactiveValues()
rv$nt_start <- 0
rv$nt_length <- 10

# rv$typeNow <- unique(notedf$type)
# rv$typePicked <- unique(notedf$type)
rv$typeNow <- unique(readRDS("solid/rds/noteMaster1.rds")$type)
rv$typePicked <- unique(readRDS("solid/rds/noteMaster1.rds")$type)


#####























