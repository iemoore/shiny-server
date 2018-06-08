


  
#...Rv's
#####----------------------------------------------------------------------

rv$typeLock <- 0
rv$dfNow <- data.frame(var="1",bar="1")
rv$exData <- NULL
rv$exNow <- NULL
rv$exNow_ct <- 0
rv$tNow <- NULL
rv$audioNow_sf <- 0
rv$modal_data <- NULL
rv$sf_word_now <- NULL
rv$sf_word_force <- NULL
rv$sf_word_key1 <- 0
rv$sf_word_key2 <- 0


# Old Rv's
rv$nextCt2 <- 0
rv$backCt2 <- 0

rv$startct <- 0
rv$showVerbs <- character()

rv$search_type_sf <- 1
rv$search_type_ct <- 0

rv$conj1 <- NULL


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

output$sf_Modal_fill <- renderUI({
  
  # a <- "1/2-faltar-73"
  # a <- "1-convocar-73"
  # a <- "1/2-alquilar-112"
  a <- "2-obsequiar-6/16"
  
  pripas("sf_Modal_fill re-rendering at ",Sys.time())
  
  rv$sf_word_now
  
  a <- rv$sf_word_now[1]
  # b <- rv$sf_word_key1[1]

  # if(b==1){ 
  #   
  #   isolate(rv$sf_word_key1 <- 0)
  #   isolate(rv$sf_word_key2 <- 1)
  #   
  # }
  # 

  word1 <- str_split_fixed(a,"-",3)[,1]
  verb1 <- str_split_fixed(a,"-",3)[,2]
  tense1 <- str_split_fixed(a,"-",3)[,3] 
  tense1 <-  un_sp(tense1,"/") %>% as.numeric()
  
  n <- filter(verbMaster,spn==verb1)
  head <- h3(tags$b(paste0(toupper(n$spn)," - ",n$eng))) 
  
  
  b <- filter(dictdf,verb==verb1)
  dim(b)
  
  if(length(b[[1]])>0){
    
    b1 <- unique(b$ctx)
    
    h1 <- paste0("The verb ",b$verb[1]," has ",length(b1),
                 " unique context",ifelse(length(b1)==1,"","s"))
    
    table1 <- ifelse(as.numeric(tense1[1])==0,"",paste0(verbToTable(a),"<br>"))
    # table1 <- verbToTable(a)
    
    list3 <- NULL
    for(i in 1:length(b1)){
      
      b2 <- filter(b,ctx==b1[i])
      b3 <- paste0("<b>(",b2$mean,")</b> ",b2$spn," ... ",b2$eng)
      
      b4 <- paste0("<dt><u>",toupper(b1[i]),"</u>...</dt><br><dd>",paste(b3,
                                                                         collapse = "<br><br>"),"</dd>")
      
      list3 <- paste0(list3,"<dl>",b4,"</dl><br>")
    }
    # <center>
    
    return(HTML(paste0("<center>",tags$u(head),"</center>",table1,
                       h3(tags$b(h1)),list3)))
    
  }
  

})


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


output$progressBar1 <- renderUI({


  if(length(rv$exNow)>1){

    v1 <- length(rv$exNow)
    v2 <- length(rv$dfNow[[1]])
    
    
    v3 <- round(floor((v1/v2)*100))

    pripas("v1 <- ",v3)

    a <- progressGroup("Progress", v1, 0, v2, color = "blue") %>% as.character()
    
  } else { a <- as.character("")}

  HTML(a)

})

#####


#...ObserveEvent's
#####----------------------------------------------------------------------

observeEvent(rv$sf_word_now,{
  
  a <- "2-obsequiar-6/16"
  
  a <- rv$sf_word_now[1]
  
  if(rv$sf_word_key1[1]==0){
    
    pripas("rv$sf_word_key1[1]==0")
    
    word1 <- str_split_fixed(a,"-",3)[,1]
    verb1 <- str_split_fixed(a,"-",3)[,2]
    tense1 <- str_split_fixed(a,"-",3)[,3] 
    # tense1 <-  un_sp(tense1,"/") %>% as.numeric()
    
    conj1 <- rv$sf_word_now[2]
    
    lildf <- data.frame(user=USER$name,time=Sys.time(),word=word1,verb=verb1,
                        conj=conj1,tense=tense1,type=rv$tNow,row=tail(rv$exNow,n=1))
    
    
    rv$modal_data <- rbind(lildf,rv$modal_data)
    saveRDS(rv$modal_data,paste0("userdata/modal_data.rds"))
    
  } 
  
  
})


observeEvent(rv$sf_word_key2,{
  
  if(rv$sf_word_key2==1){
    
    pripas("opening modal at ",Sys.time())
    toggleModal(session, "sf_Modal",toggle = "open")
    rv$sf_word_key2 <- 0
    
  }
  
})


observeEvent(input$flashClick_web,{
  
  a <- input$flashClick_web[1]
  
  # pripas("input$flashClick_web[1] <- ",rv$conj1[as.numeric(a)])
  
  b0 <- rv$conj1[as.numeric(a)]
  
  b <- as.character(unlist(str_split_fixed(b0,"-",3)))
  
  word1 <- b[1]
  verb1 <- b[2]
  tense1 <- unlist(str_split(b[3],"/"))
  
  c <- filter(conjdf, verb==verb1, tenseNum %in% tense1)
  c2 <- ifelse(length(c[[1]])>0,c$conj[1],verb1)
  
  # pripas(b0," <- ",paste(c$conj," = ",c$popup,collapse = ", "))
  
  rv$sf_word_now <- c(b0,c2)
  
  toggleModal(session, "sf_Modal", toggle = "toggle")
  
})


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


#...Modal Datatables
#####----------------------------------------------------------------------


output$sf_popHistModal_DF <- DT::renderDataTable({
  if (USER$Logged == TRUE) {
    
    pripas("Modal History modal activated at ",Sys.time())
    
    # input$actPopHistModal
    
    s2 <- isolate(rv$modal_data)
    
    if(!is.null(s2)){
      
      DT::datatable(s2, selection = list(mode = "single", target = "row"),
                    options = list(#ajax = list(url = action), 
                      processing = FALSE,
                      deferRender = TRUE,
                      scrollX=TRUE,
                      # columnDefs = list(list(width = '15%', targets = list(1,2)),
                      #                   list(className = 'dt-center', targets = c(0,1,3,4))),
                      
                      lengthMenu = c(5,10, 20, 50, 100),
                      language = list(search = 'Filter:')
                    ), 
                    # rownames=FALSE,
                    escape = FALSE,  
                    editable = T)       
    }
    
    
    
  }
})
proxy1 = dataTableProxy('sf_popHistModal_DF')
observe({
  
  input$actPopHistModal
  
  dataTableAjax(session, isolate(rv$modal_data), outputId = 'sf_popHistModal_DF')
  reloadData(proxy1, resetPaging = FALSE)
  
})
observeEvent(input$sf_popHistModal_DF_rows_selected,{
  
  a <- input$sf_popHistModal_DF_rows_selected
  
  pripas("popHistModal row <- ",a)
  
  b <-  rv$modal_data[a,]
  
  toggleModal(session, "sf_popHistModal", toggle = "toggle")
  
  
  rv$sf_word_now <- c(paste0(b$word,"-",b$verb,"-",b$tense),Sys.time())
  rv$sf_word_key1 <- c(1,Sys.time())
  
  # shinyjs:click('sfm_trigger')
  
  pripas("rv$sf_word_now <- ",rv$sf_word_now[1])
  
  # Sys.sleep(1)
  
  toggleModal(session, "sf_Modal", toggle = "toggle")
  
})


#####


#...Dynamic UI
#####----------------------------------------------------------------------

observeEvent(c(rv$search_type_sf,rv$nextCt2), {
  if (USER$Logged == TRUE) {
  
  # pripas("Search type or next clicked at ",Sys.time())
  # pripas("rv$search_type_sf <- ",rv$search_type_sf)
  # pripas("rv$typeLock <- ",rv$typeLock)

  removeUI("#flashUI1_sf")
  removeUI("#flashUI2_sf")
  removeUI("#flashUI3_sf")
  removeUI("#flashUI4_sf")

  t <- rv$search_type_sf
  
  if(!(t==rv$typeLock)){
    
    # pripas("typeLock change at ",Sys.time())
    
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
  
  
  if(length(rv$exNow)>2) {
    
    pripas("rv$exNow <- ",paste(tail(rv$exNow,n=5),collapse = ","))
    pripas("rv$exNow_ct <- ",rv$exNow_ct," of ",length(rv$exNow))
    
    if(length(rv$exNow)==(rv$exNow_ct)){
      
      # pripas("Starting new row at ",Sys.time())
  
      if(length(rv$exNow)<dim(rv$dfNow)[1]){
    
        b <- rv$dfNow
        
        
        #... Limit data to only irregular
        # pripas("input$ms_showIrreg <- ",input$ms_showIrreg)
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
        # pripas("tail of rv$exNow <- ",paste(tail(rv$exNow,10),collapse = ", "))
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
      
      # pripas("rv$exNow_ct<length(rv$exNow) at ",Sys.time())
      
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
    
    # pripas("tail of rv$exNow <- ",paste(tail(rv$exNow,5),collapse = ", "))
    
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

















