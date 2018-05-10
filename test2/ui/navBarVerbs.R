


navbarMenu("Verbs", #id = "navPage2",
           
           
           tabPanel("Learn1", value = "panel1",
                    
                    
                    column(uiOutput("tabVerbLearn"),offset = 1,width = 10),
                    # uiOutput("tabVerbLearn"),
                    
                    column(actionButton("tabBut", "View Table"),
                           actionButton("reset", "Reset"),
                           
                           offset = 1, width = 10),
                    
                    
                    bsModal("modal1", actionButton("show_options2", "Show Tenses"),
                            "tabBut", size = "large",
                            shinyjs::hidden(wellPanel(id = "options_panel2",
                                                      
                                source("ui/checkboxTenses.R",local=T)$value  
                                                      
                            )),
                            tags$a(id="here")  
                    )
                    
           ), 
           tabPanel("Flashcards1", value = "panel2",
                    
                    pageWithSidebar(
                      headerPanel(
                        span("Verbs:",
                             actionButton("hideSide3",label = NULL,
                                          icon=icon("cog", lib = "glyphicon")))),
                      
                      #br(),
                      div( id ="Sidebar3",
                           sidebarPanel( 
                             
                             #h6("sidebar"),
                             uiOutput("verbType")
                             #shinyjs::hidden(id = "options_panel3",
                             
                             # uiOutput("theme2Select")#,
                             # uiOutput("flashShow"),
                             # uiOutput("flashNext")
                             
                           )),
                      mainPanel(
                        
                        # a(id="here6"),
                        
                        wellPanel(id="flashPanel",a(id="hereVe1")),
                        wellPanel(id="flashPanel2",a(id="hereVe2")),
                        
                        inlineDing(uiOutput("flashShow3")),
                        inlineDing(uiOutput("flashNext3"))
                        
                        
                      )
                    )
                    
           ),
           tabPanel("Quiz1", value = "panel3",
                    
                  
                  
                  pageWithSidebar(
                    headerPanel("Verb Quiz:"),
                    sidebarPanel(
                      
                      uiOutput("verbType_Ve"),
                      uiOutput("buttons_Ve"),
                      tags$br(),
                      uiOutput("sentScore_Ve")
                      
                    ),
                    mainPanel(
                      
                      wellPanel(id="flashPanel",a(id="here_VeQ1")),
                      wellPanel(id="flashPanel3",a(id="here_VeQ2")),
                      
                      # tags$a(id="hereQ7"),
                      inlineDing(uiOutput("quizShow_Ve")),
                      inlineDing(uiOutput("quizNext_Ve"))
                      
                    )
                  )
                
                    
           )
)








