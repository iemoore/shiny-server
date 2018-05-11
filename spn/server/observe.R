



#...Observer input$sent_tbl_state, change sent_tbl rv's
#####------------------------------------------------------------------


observeEvent(input$sent_tbl_state,{
  
  print(paste0("State change initiated 2, rv$sent_length <- ",rv$sent_length,
               " , rv$sent_start <- ",rv$sent_start))
  
  #if(count1>1){

  if(!identical(rv$sent_length,rv$sent_state$length) |
     !identical(rv$sent_start,rv$sent_state$start)) {
    rv$sent_state <- input$sent_tbl_state
    rv$sent_length <- rv$sent_state$length
    rv$sent_start <- rv$sent_state$start
    
    print(paste0("State changed, rv$sent_length <- ",rv$sent_length,
                 " , rv$sent_start <- ",rv$sent_start,", count1 = ",count1))
  }

  #count1 <- count1 + 1
  #print(count1)
  
})

#####



#...Basic Conj decoder
#####------------------------------------------------------------------

observeEvent(rv$clickNow,{
  
    r1 <- input$verb_tbl_rows_selected
  
    #a2 <- "mydiva2"
    a2 <- rv$clickNow
    
    l1 <- substr(a2,6,6) 
    n1 <- as.numeric(substr(a2,7,nchar(a2)))
    
    #r <- input$verb_tbl_rows_selected 
    b1 <- str_split_fixed(conjdf$conj[r1[grep(l1,letters)]],",",144)[[(n1)]]
    
    # shinyjs::click(id = "#tab-8664-2")
    
    
    print(paste0("rv$clickNow <- ",b1))
    
    
}, ignoreInit = T)

#####



#...Check if new verbs have been selected when opening conj/sent modal
#####------------------------------------------------------------------

rv$newRows <- ""
observeEvent(input$tabBut, {
  
  r <- input$verb_tbl_rows_selected
  if(!identical(r,rv$newRows)){
    rv$newRows <- input$verb_tbl_rows_selected
    rv$sent_start <- 0
    rv$sent_length <- 10
    print("newRows changed")
  }
})
#####



#...Create onclick calls for dynamically rendered verbs %in% sentences
#####------------------------------------------------------------------

observeEvent(rv$newRows, {
  
  #print("tabBut called via rv$newRows")
  
  r <- input$verb_tbl_rows_selected
  b2 <- as.numeric(unlist(str_split(conjdf$uniqConj[r],",")))
  b2 <- b2[!is.na(b2)]
  
  lapply(1:length(r), function(ab){
    
    lapply(b2,  function(x) {
      
      shinyjs::onclick(sprintf(paste0("mydiv",letters[ab],"%s"), x),
                       list(print(paste0("input$mydiv",letters[ab],x,"' New! clicked")),
                            rv$clickNow <- paste0("mydiv",letters[ab],x)#,
                       ))
    })
  })
})
#####



#...Trigger rv$sent_now when a new sentence row is clicked
#####------------------------------------------------------------------


rv$sent_now <- ""

observeEvent(input$sent_tbl_rows_selected,{
  
  if(rv$sent_now != input$sent_tbl_rows_selected){
    rv$sent_now <- input$sent_tbl_rows_selected
    #print(paste0("rv$sent_now changed to ",rv$sent_now))
  }
  
})
#####



#...Dynamically create Verb conj. tables
#####------------------------------------------------------------------


# observeEvent(c(input$tabBut,input$checkTense), {
observeEvent(c(rv$newRows,input$checkTense), {
  
  #print("Verb tbl row selected")
  removeUI("#conjUI")
  
  if(length(input$checkTense)>0){
    
    r <- input$verb_tbl_rows_selected
    
    if(length(r)>0){
      
      source("server/Fun/verbTable.R",local = T)
      
      n <- dynamicTabs
      h1<-n[1];h4<-n[4];b1<-n[6];b4<-n[9]
      h2 <- "";h3<-"";b2<-"";b3<-""
      h2 <- sub("Home",verbdf$verb[r[1]],n[2])
      if(length(r)>1){
        for(i in 2:length(r)){
          
          h <- gsub("Menu 1",verbdf$verb[r[i]],n[3])
          h <- sub("menu1",paste0("menu",i),h)
          h3 <- c(h3,h)
          
        }
      }
      
      b2 <- sub("home content",a[1],n[7])  
      if(length(r)>1){
        for(i in 2:length(r)){
          
          b <- sub("menu1",paste0("menu",i),n[8])
          b <- sub("tab content",a[i],b)
          b3 <- c(b3,b)
          
        }
      }
      
      f <- c(h1,h2,h3,h4,b1,b2,b3,b4)
      
      ui_build <- tags$div(
        id= "conjUI",
        HTML(f)
      )  
      
      insertUI(
        selector = "#here",
        where = "afterEnd",
        ui = ui_build
      )
    }
  }  
})

#####



#... Feddback from the verb table like, dislike, and save buttons
#####------------------------------------------------------------------


observeEvent(input$icon_click_like,{ print(paste0("Liked row ",rv$sent_now))})  

observeEvent(input$icon_click_dislike,{print(paste0("Dislikes row ",rv$sent_now))}) 

observeEvent(input$icon_click_save,{print(paste0("Saved row ",rv$sent_now))}) 

#####



#...Toggle hidden options panels
#####------------------------------------------------------------------

observeEvent(input$show_options, {shinyjs::toggle(id= "options_panel")})

observeEvent(input$show_options2, {shinyjs::toggle(id= "options_panel2")})



#####



