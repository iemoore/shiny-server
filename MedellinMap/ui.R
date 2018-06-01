


# Simple header -----------------------------------------------------------

dm <- dropdownMenu(type="messages")
mm <- dropdownMenu(type="notifications")
tm <- dropdownMenu(type="tasks")

# header <- dashboardHeader(title="Medellin", dm, mm, tm)
header <- dashboardHeader(title="Medellin")

# No sidebar --------------------------------------------------------------

sm <- sidebarMenu(id = "tabs",
  
  searchInput(
    inputId = "id", 
    label = "Enter your search :", 
    placeholder = "Place, Food, ect.", 
    btnSearch = icon("search"), 
    btnReset = icon("remove"),
    width = "100%"
  ),
  
  menuItem(
    text="Map",
    tabName="main",
    icon=icon("database")),
  
  menuItem(
    text="Settings",
    tabName="tab1",
    icon=icon("forward"))
  
)

sidebar <- dashboardSidebar(sm)

# Compose dashboard body --------------------------------------------------


body <- dashboardBody(
  
  useShinyjs(),
  includeScript("solid/js/key.js"),
  includeScript("solid/js/keyMessage.js"),
  # includeScript("solid/js/oldGPS.js"),
  
  HTML(paste0("<meta content='width=device-width, initial-scale=1.0,",
              " maximum-scale=1.0, user-scalable=0' name='viewport' />")),

  useShinyalert(),
  
  tabItems(
    tabItem(
      tabName="main",
      
      # bootstrapPage(
      fluidPage(
      
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        tags$style(type = "text/css",
                   "#map {height: calc(100vh - 100px) !important;}"),
        
        div(class="outer",leafletOutput("map", width = "100%"))
        
        #,
        
      )
    ),
    
    tabItem(
      tabName="tab1",
      
      fluidPage(
        

        h2("Options"),
        
        fluidRow(
          
          uiOutput("slidersUI")      
        
        )
        
        # ,
        # includeScript("solid/js/gps/newGPS.js"),
        # fluidRow(column(width = 5,
        #                 verbatimTextOutput("lat"),
        #                 verbatimTextOutput("long"),
        #                 verbatimTextOutput("geolocation"),
        #                 verbatimTextOutput("accuracy"),
        #                 verbatimTextOutput("time"))
        # 
        # )
      )
    )  
    
  )
)


shinyUI( 

  dashboardPage(

      header = header, 
      sidebar = sidebar,
      body = body,
      skin= "black"
      
  )
)  












