

rv$typeLock <- 0
rv$dfNow <- data.frame(var="1",bar="1")
rv$exData <- NULL
rv$exNow <- NULL
rv$exNow_ct <- 0
rv$tNow <- NULL
rv$audioNow_sf <- 0

#...Old Rv's
#####----------------------------------------------------------------------
rv$nextCt2 <- 0
rv$backCt2 <- 0

rv$startct <- 0
rv$showVerbs <- character()

rv$search_type_sf <- 1
rv$search_type_ct <- 0




rv$conj1 <- NULL

rv$audioNow_sf <- 0

ch1 <- c("Webster 575","Anki 7627")
choices = data.frame(var = ch1,num = 1:length(ch1))
mylist <- as.list(choices$num)
names(mylist) <- choices$var
#####

#...Functions
#####----------------------------------------------------------------------

uiBuildFun <- function(ui_tar1,ui_tar2){
  
  ui_build1 <- div(id= "flashUI1_sf",h3(ui_tar1))
  
  ui_build2 <- div(id= "flashUI2_sf",
                   shinyjs::hidden(div(id = "hideF2",h3(ui_tar2))))
  
  insertUI(
    selector = "#hereSe1",
    where = "afterEnd",
    ui = ui_build1
  )
  
  insertUI(
    selector = "#hereSe2",
    where = "afterEnd",
    ui = ui_build2
  )  
  
}


#####


#...UI Elements
#####----------------------------------------------------------------------

output$search_type_sf_Out <- renderUI({
  if (USER$Logged == TRUE) { 
    
    
    selectInput("search_type_sf", label = NULL,
                choices = mylist,
                selected = 1,
                multiple = F)
    
    
  }
})


output$flashBack2 <- renderUI({
  if (USER$Logged == TRUE) {
    
    
    if(!(rv$typeLock==0)){
      actionButton("backF2","Back") 
    }
    
  }
})


output$flashShow2 <- renderUI({
  if (USER$Logged == TRUE) {
  

    if(!(rv$typeLock==0)){
      actionButton("showF2","Show")
    }
  
  }
})


output$flashNext2 <- renderUI({
  if (USER$Logged == TRUE) {
    
    a <- ifelse(!(rv$typeLock==0),"Next","Start")
  
    actionButton("nextF2",a)

  }
})


output$flashAudioWeb <- renderUI({
  if (USER$Logged == TRUE) {
    
    t <- rv$audioNow_sf
    a <- rv$search_type_sf
    
    if(!(t==0)){
      
      HTML(paste0("<audio class=\"aud\" id=\"player", t,
                  "\" src=\"audio",ifelse(a==1,"2",""),"/",
                  rv$audioNow_sf,".mp3\" ",ifelse(input$ms_aa==T,"autoplay",""),
                  " controls></audio>"))         
    }
 
  }
})


# output$progressBar1 <- renderUI({
#   
#   
#   if(length(rv$exNow)>1){
#     
#     v1 <- floor(round(length(rv$exNow)/length(rv$dfNow[[1]]),0))
#     
#     pripas("v1 <- ",v1)
#     
#     a <- paste0("<div class=\"progress\"><div",
#             " class=\"progress-bar\" role=\"progressbar\" aria-valuenow=\"",v1,
#             "\" aria-valuemin=\"0\" aria-valuemax=\"100\"></div></div>")
#     
#     a <- paste0("<div class=\"progress\"><div class=\"progress-bar progress-bar",
#                 "-success\" role=\"progressbar\" aria-valuenow=\"",v1,"\" ",
#                 "aria-valuemin=\"0\" aria-valuemax=\"100\"> ",v1,"%",
#                 "</div></div>")
#   } else { a <- as.character(h4("Error"))}
#   
#   HTML(a)
#   
# })

#####


#...ObserveEvent's
#####----------------------------------------------------------------------


observeEvent(input$options_btn_sf, {shinyjs::toggle(id= "options_panel_sf")})


observeEvent(input$search_type_sf,{
  
  # pripas("input$search_type_sf <- ",input$search_type_sf)
  
  if(rv$search_type_ct>0){
    
    rv$search_type_sf <- input$search_type_sf  
  }
  
  rv$search_type_ct <- rv$search_type_ct + 1
  
  # pripas("rv$search_type_sf <- ",rv$search_type_sf)
  
  if(rv$search_type_sf == 1){
    
    shinyjs::show(id= "e501_panel_sf")
  } else {
    
    shinyjs::hide(id= "e501_panel_sf")
  }
  
})


observeEvent(input$search_result_sf,{
  
  rv$search_result_sf <- input$search_result_sf
  
})


observeEvent(input$hideSide,{
  
  #print("input$hideSide clicked")
  shinyjs::toggle("Sidebar")
  
})


observeEvent(input$backF2,{
  
  rv$backCt2 <- rv$backCt2 + 1
  shinyjs::hide(id= "hideF2")
  
})


observeEvent(input$nextF2,{
  
  print("Next Clicked")
  rv$nextCt2 <- rv$nextCt2 + 1
  shinyjs::hide(id= "hideF2")
  
})


observeEvent(input$showF2, {
  
  #print(paste0("Trying to toggle hideF2"))
  shinyjs::toggle(id= "hideF2")
  
})


#####


#...Dynamic UI
#####----------------------------------------------------------------------

observeEvent(c(rv$search_type_sf,rv$nextCt2), {
  if (USER$Logged == TRUE) {
  
  pripas("Search type or next clicked at ",Sys.time())
  pripas("rv$search_type_sf <- ",rv$search_type_sf)
  pripas("rv$typeLock <- ",rv$typeLock)

  removeUI("#flashUI1_sf")
  removeUI("#flashUI2_sf")
  removeUI("#flashUI3_sf")
  removeUI("#flashUI4_sf")

  t <- rv$search_type_sf
  
  if(!(t==rv$typeLock)){
    
    pripas("typeLock change at ",Sys.time())
    
    rv$typeLock <- t
    
    if(t==1){ 
      rv$dfNow <- audioWeb 
      # rv$exNow <- as.numeric(rv$exData[which(rv$exData$type=="W"),"row"])
      rv$exNow <- as.numeric(rv$exData[which(rv$exData$type=="W"),]$row)
      rv$exNow_ct <- length(rv$exNow)
      rv$tNow <- "W"
    }
    if(t==2){ 
      rv$dfNow  <- audioDf
      rv$exNow <- as.numeric(rv$exData[which(rv$exData$type=="A"),]$row)
      rv$exNow_ct <- length(rv$exNow)
      rv$tNow <- "A"
    }    
    
  }
  
  pripas("length(rv$dfNow[,1]) <- ",length(rv$dfNow[,1]))
  
  if(length(rv$exNow[,1])>2) {
    
    pripas("length(rv$exNow) <- ",length(rv$exNow))
    pripas("rv$exNow_ct <- ",rv$exNow_ct)
    
    if(length(rv$exNow)==(rv$exNow_ct)){
      
      pripas("Starting new row at ",Sys.time())
  
      if(length(rv$exNow)<dim(rv$dfNow)[1]){
    
        b <- rv$dfNow
        
        
        #... Limit data to only irregular
        pripas("input$ms_showIrreg <- ",input$ms_showIrreg)
        if(input$ms_showIrreg==F){b1 <- b}
        if(input$ms_showIrreg==T){b1 <- b[which(b$irregTF==T),]}
        
        
        #... Choose between new data and random
        # if(input$ms_ShowNew==T){
          rNum <- sample(rownames(b1)[!(rownames(b1) %in% rv$exNow)],1)
        # }
        # else {
        #   rNum <- sample(rownames(b1),1)
        # }

        f <- b[rNum,]
        
        rv$audioNow_sf <- f$num1
        rv$exNow_ct <- rv$exNow_ct + 1
        rv$exNow <- c(rv$exNow,rNum)
        pripas("tail of rv$exNow <- ",paste(tail(rv$exNow,10),collapse = ", "))
        logjs(paste0("tail of rv$exNow <- ",paste(tail(rv$exNow,10),collapse = ", ")))
        
        #... Choose whether to save session
        if(input$ms_SaveSession==T){
          
          lildf <- data.frame(user=USER$name,type=rv$tNow,time=Sys.time(),row=rNum)
          rv$exData <- rbind(rv$exData,lildf)
          # exDataG <<- rv$exData   
          
          saveRDS(rv$exData,"userdata/Data.rds")
        }

        
        sent2 <- sent2function(f)
        
        ui_tar1 <- HTML(sent2)
        ui_tar2 <- f[1,"eng"]
        
        
        uiBuildFun(ui_tar1,ui_tar2)
  
        
      }
      else {
  
        
        ui_build1 <- tags$div(id= "flashUI3_sf",
                              h2("Has agotado este conjunto de frases verbales")
                              )

        ui_build2 <- tags$div(id= "flashUI4_sf",
                              h2("You have exhausted this Verb Sentence Set")
        )


        insertUI(
          selector = "#hereSe1",
          where = "afterEnd",
          ui = ui_build1
        )

        insertUI(
          selector = "#hereSe2",
          where = "afterEnd",
          ui = ui_build2
        )
      }
  
    }
  
    if(rv$exNow_ct<length(rv$exNow)){
      
      pripas("rv$exNow_ct<length(rv$exNow) at ",Sys.time())
      
      rv$exNow_ct <- rv$exNow_ct + 1
      
      rNum <- rv$exNow[rv$exNow_ct]
      f <- rv$dfNow[rNum,]
      rv$audioNow_sf <- f$num1
      
      sent2 <- sent2function(f)
      
      ui_tar1 <- HTML(sent2)
      ui_tar2 <- f[1,"eng"]
      
      uiBuildFun(ui_tar1,ui_tar2)
        
      
    }
    
  }
    
  }
})


observeEvent(rv$backCt2, {
  if (USER$Logged == TRUE) {
  if(rv$backCt2>0){
  
    # pripas("Dynamic UI 2 activated")
    
    removeUI("#flashUI1_sf")
    removeUI("#flashUI2_sf")
    removeUI("#flashUI3_sf")
    removeUI("#flashUI4_sf")
    
    rv$exNow_ct <- rv$exNow_ct - 1
    
    pripas("tail of rv$exNow <- ",paste(tail(rv$exNow,5),collapse = ", "))
    
    rNum <- rv$exNow[rv$exNow_ct]
    f <- rv$dfNow[rNum,]
    rv$audioNow_sf <- f$num1

    sent2 <- sent2function(f)
    
    ui_tar1 <- HTML(sent2)
    ui_tar2 <- f[1,"eng"]
    
    uiBuildFun(ui_tar1,ui_tar2)
      
  }  
  }
})



#####

















