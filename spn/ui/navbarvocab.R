

navbarMenu("Vocab",
           
           tabPanel("Learn", value = "panel4",
                    
                  column(uiOutput("tabVocabLearn"),offset = 1,width = 10)
                  # uiOutput("tabVocabLearn")  
                  
           ),
           tabPanel("Flashcards", value = "panel5",
                    
                    pageWithSidebar(
                      headerPanel(
                        span("Vocab:",
                             actionButton("hideSide2",label = NULL,
                                          icon=icon("cog", lib = "glyphicon")))),
                      
                      div( id ="Sidebar2",sidebarPanel( 
                        
                        #h6("sidebar"),
                        uiOutput("theme1Select"),
                        #shinyjs::hidden(id = "options_panel3",
                        
                        uiOutput("theme2Select")#,
                        # uiOutput("flashShow"),
                        # uiOutput("flashNext")
                        
                      )),
                      mainPanel(
                        
                        # a(id="here4"),
                        
                        wellPanel(id="flashPanel",a(id="hereVo1")),
                        wellPanel(id="flashPanel2",a(id="hereVo2")),
                        
                        inlineDing(uiOutput("flashShow")),
                        inlineDing(uiOutput("flashNext"))
                      )
                    )
                    
           ),
           tabPanel("Quiz", value = "panel6",
                    
                    pageWithSidebar(
                      headerPanel("Vocab Quiz:"),
                      sidebarPanel(
                        
                        uiOutput("theme1Select_Vo"),
                        uiOutput("theme2Select_Vo"),
                        uiOutput("buttons_Vo"),
                        uiOutput("sentScore_Vo")
                        
                      ),
                      mainPanel(
                        
                        wellPanel(id="flashPanel",a(id="here1_VoQ")),
                        wellPanel(id="flashPanel3",a(id="here2_VoQ")),
                        
                        tags$a(id="hereQ2"),
                        inlineDing(uiOutput("quizShow_Vo")),
                        inlineDing(uiOutput("quizNext_Vo"))
                        
                      )
                    )
                    
           )
)
