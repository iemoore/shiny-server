
# cat,pov,av
catdf <- filter(vocabdf, theme1==unique(vocabdf$theme1)[1])[,-1]
posdf <- filter(vocabdf, theme1==unique(vocabdf$theme1)[2])[,-1]
avdf <- filter(vocabdf, theme1==unique(vocabdf$theme1)[3])[,-1]

#...Render UI
#####------------------------------------------------------------------

output$catSelect <- renderUI({
  if (USER$Logged == TRUE) {
    
    a <- count(catdf,theme2)  
    
    selectInput('chooseCat', label = NULL,
                c(Choose='', paste0(a$theme2,"   [",a$n,"]")),
                selectize=TRUE)
  }
})

output$posSelect <- renderUI({
  if (USER$Logged == TRUE) {
    
    a <- count(posdf,theme2)
    
    selectInput('choosePos', label = NULL,
                c(Choose='', paste0(a$theme2,"   [",a$n,"]")),
                selectize=TRUE)
  }  
})

output$avSelect <- renderUI({
  if (USER$Logged == TRUE) {
    
    a <- count(avdf,theme2)
    
    selectInput('chooseAv', label = NULL,
                c(Choose='', paste0(a$theme2,"   [",a$n,"]")),
                selectize=TRUE)
  }
})

output$tabVocabLearn <- renderUI({
  

    tabsetPanel(id = "subTabPanel2",
              
              tabPanel("Categories", value = "panel4_a",
                       
                      br(),uiOutput("catSelect"),DTOutput('cat_tbl')
              ),
              
              tabPanel("Parts of Speech", value = "panel4_b",
                       
                      br(),uiOutput("posSelect"),DTOutput('pos_tbl')
              ),
              
              tabPanel("Advanced", value = "panel4_c",
                       
                      br(),uiOutput("avSelect"),DTOutput('av_tbl')
              )
    )
}) 

#####


#...Vocab input rv's
#####------------------------------------------------------------------


rv$cat <- ""
rv$pos <- ""
rv$av <- ""

observeEvent(input$chooseCat, {
  
  rv$cat <- as.character(unlist(str_split_fixed(input$chooseCat,"   \\[",2))[1])
  
})

observeEvent(input$choosePos, {
  
  rv$pos <- as.character(unlist(str_split_fixed(input$choosePos,"   \\[",2))[1])    
  
})

observeEvent(input$chooseAv, {
  
  # rv$av <- as.character(input$chooseAv)
  rv$av <- as.character(unlist(str_split_fixed(input$chooseAv,"   \\[",2))[1])
})


#####


#... Datatables
#####------------------------------------------------------------------

output$cat_tbl <- DT::renderDataTable({
  if (USER$Logged == TRUE) {
    
    input$reset2  
    a <- catdf
    
    if(nchar(rv$cat)>1){
      a <- filter(a,theme2==rv$cat)[,-c(1,2)]
    }
    datatable(a, options = list(lengthMenu = c(10, 25, 50, 100)))
    
  }  
})

output$pos_tbl <- DT::renderDataTable({
  if (USER$Logged == TRUE) {
    
    input$reset3  
    a <- posdf
    
    if(nchar(rv$pos)>1){
      a <- filter(a,theme2==rv$pos)[,-c(1,2)]
    }
    datatable(a, options = list(lengthMenu = c(10, 25, 50, 100)))
    
  }  
})

output$av_tbl <- DT::renderDataTable({
  if (USER$Logged == TRUE) {
    
    input$reset4 
    a <- avdf
    
    if(nchar(rv$av)>1){
      a <- filter(a,theme2==rv$av)[,-c(1,2)]
    }
    datatable(a, options = list(lengthMenu = c(10, 25, 50, 100)))
    
  }
})

#####






























