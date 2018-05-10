




navbarPage( 
            # title = searchInput(
            #   inputId = "id", 
            #   label = NULL, 
            #   placeholder = "This is a placeholder", 
            #   btnSearch = icon("search"), 
            #   btnReset = icon("remove"), 
            #   width = "100%"
            # ),
            title = div(class="headBar",uiOutput("userPanel")),
            # header = div(class="headBar",uiOutput("userPanel")),
            id = "someID",
            collapsible = T,
            position = 'static-top',
            
            # tags$div(tags$style(HTML(paste0("a.dropdown-toggle {z-index:10000;}")))),    
        
        tabPanel("Home", value = "panel0",
                 
            uiOutput("home")
                 
        ),
        
            
        source("ui/navbarVerbs.R",local=T)$value,

        source("ui/navbarVocab.R",local=T)$value,

        source("ui/navbarSents.R",local=T)$value,


        tabPanel("Add Content",

                 h4("Users will be able to contribute content here")
                 

        ),

        navbarMenu("More", menuName = "menu1",

                 tabPanel("Settings",h4("Settings will go here")),

                 tabPanel("About",h4("Info, Source Data, and Links here"))

        )
  )
  




