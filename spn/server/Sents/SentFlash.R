

#...Rv's
#####----------------------------------------------------------------------
rv$nextCt2 <- 0
rv$backCt2 <- 0

rv$startct <- 0
rv$showVerbs <- character()

rv$search_type_sf <- 1
rv$search_type_ct <- 0


rv$excludeE <- 0
rv$excludeW <- 0
rv$excludeA <- 0


rv$excludeE_ct <- 1
rv$excludeW_ct <- 1
rv$excludeA_ct <- 1

rv$search_result_sf <- "All"

rv$conj1 <- NULL

rv$audioNow_sf <- 0

ch1 <- c("Essential 501","Webster 575","Anki 7627")
choices = data.frame(var = ch1,num = 1:length(ch1))
mylist <- as.list(choices$num)
names(mylist) <- choices$var
#####


#... Search, Buttons
#####----------------------------------------------------------------------

output$search_type_sf_Out <- renderUI({
  if (USER$Logged == TRUE) { 
    
    
    selectInput("search_type_sf", label = NULL,
                choices = mylist,
                selected = 1,
                multiple = F)
    
    # selectInput("search_type_sf", label = NULL,
    #             choices = c("Essential 501","Webster 575","Anki 7627"), 
    #             selected = "Essential 501",
    #             multiple = F) 
    
  }
})


output$search_letters <- renderUI({
  if (USER$Logged == TRUE) {
  
    if(rv$search_type_sf == 1){
    
      selectInput("search_result_sf", label = NULL,
                  choices = c("All",as.character(verbdf$verb)),
                  selected = "All",
                  multiple = TRUE)  
      
    } else {
      
      tags$br()
    }
  }    
})


output$flashBack2 <- renderUI({
  if (USER$Logged == TRUE) {
    
    # a <- rv$showVerbs
    
    # if(length(a)>0) {
    isolate(actionButton("backF2","Back"))
    # }
    
  }
})


output$flashShow2 <- renderUI({
  if (USER$Logged == TRUE) {
  
  # a <- rv$showVerbs
  
  # if(length(a)>0) {
    isolate(actionButton("showF2","Show"))
  # }
  
  }
})


output$flashNext2 <- renderUI({
  if (USER$Logged == TRUE) {
  
  # a <- rv$showVerbs
  
  # if(length(a)>0) {
    isolate(actionButton("nextF2","Next"))
  # }
  
  }
})


output$e501start <- renderUI({
  if (USER$Logged == TRUE) {
    
    if(rv$search_type_sf == 1){
    # if(rv$search_type_sf == "Essential 501"){
      
      actionButton("start",label = "Start")  
      
    }
    else{
      
      tags$br()
      
    }
  }
})

output$flashAudioWeb <- renderUI({
  if (USER$Logged == TRUE) {
    
    a <- rv$search_type_sf
    
    Sys.sleep(0.1)
    
    if(!(a == 1)) {
      
      HTML(paste0("<audio class=\"aud\" id=\"player", rv$audioNow_sf,
                  "\" src=\"audio",ifelse(a==2,"2",""),"/",
                  rv$audioNow_sf,".mp3\" ",ifelse(input$ms_aa==T,"autoplay",""),
                  " controls></audio>"))      
 
    }
  }
})

#####


#... ObserveEvent's
#####----------------------------------------------------------------------


# observeEvent(input$Login,{
#   
#   
# })



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
  
  # print("Next Clicked")
  rv$nextCt2 <- rv$nextCt2 + 1
  shinyjs::hide(id= "hideF2")
  
})


observeEvent(input$showF2, {
  
  #print(paste0("Trying to toggle hideF2"))
  shinyjs::toggle(id= "hideF2")
  
})


#####


#... Dynamic UI
#####----------------------------------------------------------------------

observeEvent(c(rv$search_type_sf,rv$nextCt2), {
  # observeEvent(rv$nextCt2, {  
  # observeEvent(rv$search_type_sf, {
  
  # pripas("Dynamic UI 1 activated")

  removeUI("#flashUI1_sf")
  removeUI("#flashUI2_sf")
  removeUI("#flashUI3_sf")
  removeUI("#flashUI4_sf")

  t <- rv$search_type_sf
  b <- data.frame(var="1",bar="1")

  if(t==1){
    a2 <- rv$search_result_sf
    
    if(a2=="All") {b <- sentdf
    }else{ b <- filter(sentdf,verb %in% a2) }
    
    # b <- ifelse(a2=="All",sentdf,filter(sentdf,verb %in% a2))
    rvNow <- rv$excludeE
    rvNow_ct <- rv$excludeE_ct
  }
  if(t==2){ b <- audioWeb; rvNow <- rv$excludeW; rvNow_ct <- rv$excludeW_ct}
  if(t==3){ b <- audioDf; rvNow <- rv$excludeA; rvNow_ct <- rv$excludeA_ct}
  
  
  # pripas("t <- ",t," dim(b)[1] <- ",dim(b)[1])
  # pripas("rvNow <- ",paste(rvNow,collapse = ", "))
  # pripas("rvNow_ct <- ",rvNow_ct)
  
  
  if(length(b[,1])>1) {
  
    if(length(rvNow)==rvNow_ct){
  
      if(length(rvNow)<dim(b)[1]){
  
  
        rNum <- sample(rownames(b)[!(rownames(b) %in% rvNow)],1)
        f <- b[rNum,]
        if(!(t==1)){rv$audioNow_sf <- f$num1}
        
  
        if(t==1){
          rv$excludeE <- c(rv$excludeE, rNum)
          exE_data <<- rv$excludeE
          rv$excludeE_ct <- rv$excludeE_ct + 1
          # pripas("rv$excludeE <- ",paste(rv$excludeE,collapse = ", "))
          # pripas("rv$excludeE_ct <- ",rv$excludeE_ct)
          ui_tar1 <- h3(f[1,2])
          ui_tar2 <- f[1,3]
        }
        if(t==2){
          rv$excludeW <- c(rv$excludeW, rNum)
          exW_data <<- rv$excludeW
          rv$excludeW_ct <- rv$excludeW_ct + 1
          # pripas("rv$excludeW <- ",paste(rv$excludeW,collapse = ", "))
          # pripas("rv$excludeW_ct <- ",rv$excludeW_ct)
          
          sent2 <- sent2function(f)
          # if(nchar(f[1,4])>0){
          #   
          #   conj1 <- unlist(str_split(f[1,4],", "))
          #   rv$conj1 <- conj1
          #   
          #   sent1 <- unlist(str_split(f[1,1]," "))
          #   
          #   for(h in 1:length(conj1)){
          #     
          #     a0 <- str_split_fixed(conj1[h],"-",3)
          #     a <- as.numeric(unlist(str_split(a0[1],"/")))
          #     cnum <- as.numeric(unlist(str_split(a0[3],"/")))
          #     
          #     if(cnum[1] == 0){
          #       
          #       c <- filter(verbMaster, spn==a0[2])
          #       pop <- c$eng
          #       
          #     } else {
          #       
          #       c <- filter(conjdf, verb==a0[2], tenseNum %in% cnum) 
          #       pop <- paste(c$popup,collapse = ", ")
          #     }
          #     
          #     if(length(a)==1){
          #       
          #       sent1[a] <- paste0('<a id="gfw',h,'" data-row="',h,
          #            '" class="go-flash-web" href="#" onClick="return false" data-',
          #            'toggle="tooltip" title="',pop,'" data-placement="top">',
          #            sent1[a],'</a>')
          #       
          #     } 
          #     
          #     if(length(a)>1 && (a[2]-a[1])==1) {
          #       
          #       sent1[a[1]] <- paste0('<a id="gfw',h,'" data-row="',h,'" class="go-',
          #             'flash-web" href="#" onClick="return fals',
          #             'e" data-toggle="tooltip" title="',pop,
          #             '" data-placement="top" >',
          #             paste(sent1[a],collapse = " "),'</a>')
          #       
          #       sent1[(a[1]+1):a[length(a)]] <- ""
          #       
          #       
          #     } else {
          #       
          #       sent1[a[1]] <- paste0('<a data-row="',h,
          #                     '" class="go-flash-web" href="#" onClick="return fals',
          #                     'e">',paste(sent1[a[1]],collapse = " "),'</a>')
          #       
          #       sent1[a[2]] <- paste0('<a data-row="',h,
          #                     '" class="go-flash-web" href="#" onClick="return fals',
          #                     'e">',paste(sent1[a[2]],collapse = " "),'</a>') 
          #   
          #       
          #     }
          #     
          #     
          #   }
          #   
          #   sent2 <- paste(sent1[which(nchar(sent1)>0)],collapse = " ")            
          #   
          # } else {
          #   
          #   sent2 <- f[1,1]
          #   
          # }

          ui_tar1 <- HTML(sent2)
          ui_tar2 <- f[1,2]
  
        }
        if(t==3){
          rv$excludeA <- c(rv$excludeA, rNum)
          exA_data <<- rv$excludeA
          rv$excludeA_ct <- rv$excludeA_ct + 1
          # pripas("rv$excludeA <- ",paste(rv$excludeA,collapse = ", "))
          # pripas("rv$excludeA_ct <- ",rv$excludeA_ct)
          
          sent2 <- sent2function(f)
          
          ui_tar1 <- HTML(sent2)
          ui_tar2 <- f[1,2]
          
        }
  
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
        
        
        # if(t==2){if(nchar(f[1,4])>0){
        #   
        #   print("Adding tooltips")
        #   
        #   conj1 <- unlist(str_split(f[1,4],", "))
        #   
        #   for(h in 1:length(conj1)){
        #     
        #     a0 <- str_split_fixed(conj1[h],"-",3)
        #     a <- as.numeric(unlist(str_split(a0[1],"/")))
        #     cnum <- as.numeric(unlist(str_split(a0[3],"/")))
        #     
        #     c <- filter(conjdf, verb==a0[2], tenseNum %in% cnum)
        #     
        #     addTooltip(session, paste0("gfw",h),
        #                # title = paste(c$tense,c$person, collapse = ", "),
        #                title = paste(c$popup, collapse = ", "),
        #                placement = "top",
        #                trigger = "hover", options = NULL)
        #     
        #     # sent1[a] <- paste0("<a id=\"test",h,"\"  >",sent1[a],"</a>")
        #     
        #   }  
        # }}
        # 
        
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
  
    if(rvNow_ct<length(rvNow)){
      
        if(t==1){
          rv$excludeE_ct <- rv$excludeE_ct + 1
          rvNow_ct <- rv$excludeE_ct
        }
        if(t==2){
          rv$excludeW_ct <- rv$excludeW_ct + 1
          rvNow_ct <- rv$excludeW_ct
        }
        if(t==3){ 
          rv$excludeA_ct <- rv$excludeA_ct + 1
          rvNow_ct <- rv$excludeA_ct
        }
        
        rNum <- rvNow[rvNow_ct]
        f <- b[rNum,]
        if(!(t==1)){rv$audioNow_sf <- f$num1}
        
        
        if(t==1){
          # pripas("rv$excludeE <- ",paste(rv$excludeE,collapse = ", "))
          # pripas("rv$excludeE_ct <- ",rv$excludeE_ct)
          ui_tar1 <- f[1,2]
          ui_tar2 <- f[1,3]
        }
        if(t==2){
          # pripas("rv$excludeW <- ",paste(rv$excludeW,collapse = ", "))
          # pripas("rv$excludeW_ct <- ",rv$excludeW_ct)
          sent2 <- sent2function(f)
          
          ui_tar1 <- HTML(sent2)
          ui_tar2 <- f[1,2]
          
          # ui_tar1 <- f[1,1]
          # ui_tar2 <- f[1,2]
          
        }
        if(t==3){
          # pripas("rv$excludeA <- ",paste(rv$excludeA,collapse = ", "))
          # pripas("rv$excludeA_ct <- ",rv$excludeA_ct)
          
          sent2 <- sent2function(f)
          
          ui_tar1 <- HTML(sent2)
          ui_tar2 <- f[1,2]
          
        }
        
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
    
  }
    
})


observeEvent(rv$backCt2, {
  if(rv$backCt2>0){
  
    # pripas("Dynamic UI 2 activated")
    
    removeUI("#flashUI1_sf")
    removeUI("#flashUI2_sf")
    removeUI("#flashUI3_sf")
    removeUI("#flashUI4_sf")
    
    t <- rv$search_type_sf
    b <- data.frame(var="1",bar="1")
    
    if(t==1){
      a2 <- rv$search_result_sf
      
      if(a2=="All") {b <- sentdf}else{ b <- filter(sentdf,verb %in% a2) }
      # b <- ifelse(a2=="All",sentdf,filter(sentdf,verb %in% a2))
      
      rvNow <- rv$excludeE
      rv$excludeE_ct <- rv$excludeE_ct - 1
      rvNow_ct <- rv$excludeE_ct
    }
    if(t==2){ 
      b <- audioWeb
      rvNow <- rv$excludeW
      rv$excludeW_ct <- rv$excludeW_ct - 1
      rvNow_ct <- rv$excludeW_ct
      
  }
    if(t==3){ 
      b <- audioDf
      rvNow <- rv$excludeA
      rv$excludeA_ct <- rv$excludeA_ct - 1
      rvNow_ct <- rv$excludeA_ct
  }
    
    # pripas("Back called")
    
    if(length(b[,1])>1) {
    
      rNum <- rvNow[rvNow_ct]
      f <- b[rNum,]
      if(!(t==1)){rv$audioNow_sf <- f$num1}
      
      if(t==1){
        # pripas("rv$excludeE <- ",paste(rv$excludeE,collapse = ", "))
        # pripas("rv$excludeE_ct <- ",rv$excludeE_ct)
        ui_tar1 <- f[1,2]
        ui_tar2 <- f[1,3]
    }
      if(t==2){
        # pripas("rv$excludeW <- ",paste(rv$excludeW,collapse = ", "))
        # pripas("rv$excludeW_ct <- ",rv$excludeW_ct)
        
        sent2 <- sent2function(f)
        
        ui_tar1 <- HTML(sent2)
        ui_tar2 <- f[1,2]
        
        # ui_tar1 <- f[1,1]
        # ui_tar2 <- f[1,2]
        
    }
      if(t==3){
        # pripas("rv$excludeA <- ",paste(rv$excludeA,collapse = ", "))
        # pripas("rv$excludeA_ct <- ",rv$excludeA_ct)
        
        sent2 <- sent2function(f)
        
        ui_tar1 <- HTML(sent2)
        ui_tar2 <- f[1,2]
        
    }
      
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
  
  }  
})

#####
















