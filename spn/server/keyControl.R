
#... Page navigation and js key controls
####------------------------------------------------------------------

observeEvent(input$navPage1,{
  
  
  pripas("input$navPage1 <- ",input$navPage1)
})


observeEvent(input$enter_mes,{
  
  pripas("enter_mes recieved")
  
  a <- input$enter_mes
  
  if (USER$Logged == F) {
    
    shinyjs::click("Login")
  }  
  else {
    
  }
  
  
})


observeEvent(input$ctrEnter_mes,{
  
  pripas("input$ctrEnter_mes recieved")
})


observeEvent(input$numKeys, {
  if (USER$Logged == TRUE) {  
    
    pripas("input$numKeys <- ",input$numKeys[1])
    
    updateTabsetPanel(session, 'someID', sprintf("panel%s",input$numKeys[1]))
    
  } 
})


observeEvent(input$dirKeys, {
  if (USER$Logged == TRUE) {  
    
    a <- input$dirKeys[1]
    
    #...Left
    if(a==37){ 
        
        #...Tables
        if(input$someID=='panel1'){
        
          if(input$subTabPanel1=="panel1_a"){
            
            # pripas("Left pressed in panel1_a")
            updateTabsetPanel(session, 'subTabPanel1', 'panel1_c')
            
          }
          
          if(input$subTabPanel1=="panel1_b"){
            
            # pripas("Left pressed in panel1_b")
            updateTabsetPanel(session, 'subTabPanel1', 'panel1_a')
            
          }
          
          if(input$subTabPanel1=="panel1_c"){
            
            # pripas("Left pressed in panel1_c")
            updateTabsetPanel(session, 'subTabPanel1', 'panel1_b')
            
          }
        
        }
        if(input$someID=='panel4'){
          
          if(input$subTabPanel2=="panel4_a"){
            
            # pripas("Left pressed in panel4_a")
            updateTabsetPanel(session, 'subTabPanel2', 'panel4_c')
            
          }
          
          if(input$subTabPanel2=="panel4_b"){
            
            # pripas("Left pressed in panel4_b")
            updateTabsetPanel(session, 'subTabPanel2', 'panel4_a')
            
          }
          
          if(input$subTabPanel2=="panel4_c"){
            
            # pripas("Left pressed in panel4_c")
            updateTabsetPanel(session, 'subTabPanel2', 'panel4_b')
            
          }
          
        }
        if(input$someID=='panel7'){
          
          if(input$subTabPanel3=="panel7_a"){
            
            # pripas("Left pressed in panel7_a")
            updateTabsetPanel(session, 'subTabPanel3', 'panel7_c')
            
          }
          
          if(input$subTabPanel3=="panel7_b"){
            
            # pripas("Left pressed in panel7_b")
            updateTabsetPanel(session, 'subTabPanel3', 'panel7_a')
            
          }
          
          if(input$subTabPanel3=="panel7_c"){
            
            # pripas("Left pressed in panel7_c")
            updateTabsetPanel(session, 'subTabPanel3', 'panel7_b')
            
          }
          
        
      }
      
        #..Flashcards
        if(input$someID=='panel8'){
          
          shinyjs::click('backF2')
        }
    }
    
    #...Up
    if(a==38){ 
      
        #...Flashcards
        if(input$someID=='panel2'){
          
          shinyjs::click('showF3')
          
        }
        if(input$someID=='panel5'){
          
          shinyjs::click('showF')
          
        }
        if(input$someID=='panel8'){
          
          shinyjs::click('showF2')
          
        }
      
    }
    
    #...Right
    if(a==39){ 
        
        #...Tables
        if(input$someID=='panel1'){
          
          if(input$subTabPanel1=="panel1_a"){
            
            pripas("Right pressed in panel1_a")
            updateTabsetPanel(session, 'subTabPanel1', 'panel1_b')
            
          }
          
          if(input$subTabPanel1=="panel1_b"){
            
            pripas("Right pressed in panel1_b")
            updateTabsetPanel(session, 'subTabPanel1', 'panel1_c')
            
          }
          
          if(input$subTabPanel1=="panel1_c"){
            
            pripas("Right pressed in panel1_c")
            updateTabsetPanel(session, 'subTabPanel1', 'panel1_a')
            
          }
          
        } 
        if(input$someID=='panel4'){
          
          if(input$subTabPanel2=="panel4_a"){
            
            pripas("Right pressed in panel4_a")
            updateTabsetPanel(session, 'subTabPanel2', 'panel4_b')
            
          }
          
          if(input$subTabPanel2=="panel4_b"){
            
            pripas("Right pressed in panel4_b")
            updateTabsetPanel(session, 'subTabPanel2', 'panel4_c')
            
          }
          
          if(input$subTabPanel2=="panel4_c"){
            
            pripas("Right pressed in panel4_c")
            updateTabsetPanel(session, 'subTabPanel2', 'panel4_a')
            
          }
          
        } 
        if(input$someID=='panel7'){
        
          if(input$subTabPanel3=="panel7_a"){
            
            pripas("Right pressed in panel7_a")
            updateTabsetPanel(session, 'subTabPanel3', 'panel7_b')
            
          }
          
          if(input$subTabPanel3=="panel7_b"){
            
            pripas("Right pressed in panel7_b")
            updateTabsetPanel(session, 'subTabPanel3', 'panel7_c')
            
          }
          
          if(input$subTabPanel3=="panel7_c"){
            
            pripas("Right pressed in panel7_c")
            updateTabsetPanel(session, 'subTabPanel3', 'panel7_a')
            
          }
        
      }     
      
        #...Flashcards
        if(input$someID=='panel2'){
          
          shinyjs::click('nextF3')
        }
        if(input$someID=='panel5'){
          
          shinyjs::click('nextF')
        }
        if(input$someID=='panel8'){
          
          shinyjs::click('nextF2')
        }
      
    }
    
    #...Down
    if(a==40){ 

      
      
      
    }
    
  } 
})




#####

