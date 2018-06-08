

shinyUI(
  
  # bootstrapPage(
    fluidPage(
  # semanticPage(
    #suppressDependencies("bootstrap"),
  
  title="Spanish Verb App",
  
  
  useShinyjs(),
  useShinyalert(),

  
  includeScript("solid/js/key.js"),
  includeScript("solid/js/clicktest.js"),
  includeScript("solid/js/conjCon.js"),
  includeScript("solid/js/goFlash.js"),
  # includeScript("solid/js/popper.min.js"),



  includeCSS("solid/css/testStyles.css"),
  includeCSS("solid/css/yeti.bootstrap.min.css"),
  tags$head(tags$style(HTML('.modal-lg {width: 4000px;}'))),
  inlineCSS('#flashPanel {min-height: 150px;}'),
  inlineCSS('#flashPanel2 {min-height: 150px;}'),
  inlineCSS('#flashPanel3 {min-height: 80px;}'),

  div(class = "login",
      uiOutput("uiLogin"),
      textOutput("pass"),
      tags$head(tags$style("#pass{color: red;"))
  ),

  tags$head(
    tags$style(HTML("
          .navbar .navbar-header {float: right}
        "))
  ),
  
  
  
  navbarPage( 
    
    title = div(class="headBar",uiOutput("userPanel")),
    id = "someID",
    collapsible = T,
    position = 'static-top',
    
    tabPanel("Home", value = "panel0",
             
             uiOutput("home")
             
    ),
    
    
    source("ui/navbarverbs.R",local=T)$value,
    
    source("ui/navbarvocab.R",local=T)$value,
    
    source("ui/navbarsents.R",local=T)$value,
    
    
    tabPanel("Add Content",
             
             h4("Users will be able to contribute content here")
             
             
    ),
    
    navbarMenu("More", menuName = "menu1",
               
               tabPanel("Settings",h4("Settings will go here")),
               
               tabPanel("About",h4("Info, Source Data, and Links here"))
               
    )
  )
  
  
))  


