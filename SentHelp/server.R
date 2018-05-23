

server <- function(input, output, session) {
  
  
  source("server/Flashcards.R",local = T)
  

  
  observeEvent(input$dirKeys, {if (USER$Logged == TRUE) {  
      
      a <- input$dirKeys[1]
      
      #...Left
      if(a==37){ 
        
        if(input$tabs=='flash'){
          
          shinyjs::click('back1')
        }
        
      }
      
      #...Up
      if(a==38){ 
        
        if(input$tabs=='flash'){
          
          shinyjs::click('show1')
          
        }
        
      }
      
      #...Right
      if(a==39){ 
        
        if(input$tabs=='flash'){
          
          shinyjs::click('next1')
        }
        
      }
      
      #...Down
      if(a==40){ 
        
        if(input$tabs=='flash'){
          
          shinyjs::click('show1')
          
        }
        
      }
      
      
    } 
  })
  

}