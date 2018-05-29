
# setwd("C:/Users/moore/Dropbox/R3/Shiny/shiny-server/MedellinMap")


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
library(leaflet)
# library(leafletplugins)
library(leaflet.extras)

options(warn=1)

#...Data Load
#####-------------------------------------------------------------------

barrios <- readRDS(paste0("solid/rds/Barrios.rds"))

espac <- readRDS(paste0("solid/rds/EspacioPublico.rds"))
colnames(espac)[1] <- "ID"

domic <- readRDS(paste0("solid/rds/NomenDomic.rds"))
colnames(domic) <- c("Long","Lat","OBJECTID","CBML","NUMERO_MEJORA",
                     "ID_NM_DOM","VIA","PLACA")
colnames(domic)[c(1,2)] <- c("Long","Lat")

usos <- readRDS(paste0("solid/rds/UsosUrbano.rds"))

vias <- readRDS(paste0("solid/rds/ViasUrbano.rds"))


#####

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

#...RV's
#####-------------------------------------------------------------------


#####




# jsCode <- '
# shinyjs.geoloc = function() {
#     navigator.geolocation.getCurrentPosition(onSuccess, onError);
#     function onError (err) {
#         Shiny.onInputChange("geolocation", false);
#     }
#     function onSuccess (position) {
#         setTimeout(function () {
#             var coords = position.coords;
#             console.log(coords.latitude + ", " + coords.longitude);
#             Shiny.onInputChange("geolocation", true);
#             Shiny.onInputChange("lat", coords.latitude);
#             Shiny.onInputChange("long", coords.longitude);
#         }, 5)
#     }
# };
# '




















