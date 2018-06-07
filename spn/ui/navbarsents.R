

navbarMenu("Sentences",
           
            tabPanel("Learn", value = "panel7",
                 
                column(uiOutput("tabSentLearn"),width = 10, offset=1
                )    
            ),
            tabPanel("Flashcards", value = "panel8",
                     
                    bsModal(id="sf_Modal", "More info", "sfm_trigger",size = "large",
                            
                            tags$a(id="hereModal1"),
                            uiOutput("sf_Modal_fill")
                            ),
                    
                    bsModal(id="sf_dataModal", "Sentence History", "actDataModal",
                            size = "large",
                            
                            DTOutput("sf_dataModal_fill")
                            
                    ),
                    
                    pageWithSidebar(
                      headerPanel(span(inlineDing("Flash:"),
                                  inlineDing(actionButton("hideSide",label = NULL,
                                    icon=icon("cog", lib = "glyphicon"))),
                                  inlineDing(actionButton("actDataModal",label = NULL,
                                    icon=icon("list", lib = "glyphicon")))
                      )),
                     
                      div( id ="Sidebar",sidebarPanel(
                       
                        
                        uiOutput("search_type_sf_Out"),
                        
                        inlineDing(actionBttn(inputId = "options_btn_sf",
                                              label = "Options", 
                                              style = "fill", 
                                              color = "primary")),
                      
                        shinyjs::hidden(wellPanel(id = "options_panel_sf",
                                                  
                            materialSwitch(inputId = "ms_ShowNew", 
                                           label = "Show New", value = TRUE, 
                                           status = "primary"),
                            
                            materialSwitch(inputId = "ms_SaveSession", 
                                           label = "Save Session", value = TRUE, 
                                           status = "primary"),
                            
                            materialSwitch(inputId = "ms_aa", 
                                           label = "Autoplay Audio", value = TRUE, 
                                           status = "primary"),
                            
                            materialSwitch(inputId = "ms_showIrreg", 
                                           label = "Show only irregular",
                                           value = FALSE, status = "primary")#,
                            
                            # uiOutput("progressBar1")
                                                  
                        ))
                        
                        
                        
                      )),
                      mainPanel(
                       
                        wellPanel(id="flashPanel",a(id="hereSe1")),
                        wellPanel(id="flashPanel2",a(id="hereSe2")),
                       
                        inlineDing(uiOutput("flashBack2")),
                        inlineDing(uiOutput("flashShow2")),
                        inlineDing(uiOutput("flashNext2")),
                        inlineDing(uiOutput("flashAudioWeb"))
                       
                      )
                    )
                    
            ),
            tabPanel("Quiz", value = "panel9",
                    
                    pageWithSidebar(
                      headerPanel("Verb Sentence Quiz:"),
                      sidebarPanel(
                        
                        uiOutput("verbSelect"),
                        tags$br(),
                        uiOutput("buttons"),
                        tags$br(),
                        uiOutput("sentScore")
                        
                      ),
                      mainPanel(
                        
                        # tags$a(id="hereQ1"),
                        
                        wellPanel(id="flashPanel",a(id="hereSeQ1")),
                        wellPanel(id="flashPanel3",a(id="hereSeQ2")),
                        
                        inlineDing(uiOutput("quizShow")),
                        inlineDing(uiOutput("quizNext"))
                        
                      )
                    )
                    
            )
)