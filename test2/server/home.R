


output$home <- renderUI({
  if (USER$Logged == TRUE) {
    list(
      
      column(width = 8,offset = 1,
      
      fluidRow(h2("Welcome to the spanish app ",toupper(USER$name))),
    
      br(),
      
      tags$p(paste0("We currently have ",length(verbdf$verb),
                    " verbs ranging from ",verbdf$verb[1]," to ",
                    verbdf$verb[length(verbdf$verb)]))
      )
    )
  }
})
















