


# Simple header -----------------------------------------------------------

dm <- dropdownMenu(type="messages")
mm <- dropdownMenu(type="notifications")
tm <- dropdownMenu(type="tasks")

header <- dashboardHeader(title="Dashboard", dm, mm, tm)

# No sidebar --------------------------------------------------------------

sm <- sidebarMenu(id = "tabs",
  
  searchInput(
    inputId = "id", 
    label = "Enter your search :", 
    placeholder = "This is a placeholder", 
    btnSearch = icon("search"), 
    btnReset = icon("remove"),
    width = "100%"
  ),
  
  menuItem(
    text="Add Notes",
    tabName="main",
    icon=icon("database")),
  
  menuItem(
    text="View Notes",
    tabName="tab1",
    icon=icon("forward"))
  
)

sidebar <- dashboardSidebar(sm)

# Compose dashboard body --------------------------------------------------


body <- dashboardBody(
  
  useShinyjs(),
  includeScript("solid/js/key.js"),
  includeScript("solid/js/keyMessage.js"),
  tags$style(HTML("                  
  .shiny-input-container:not(.shiny-input-container-inline) {
  width: 100%;
  height: 60%;
  }")),
  useShinyalert(),

  
  tabItems(
    tabItem(
      tabName="main",
      
      fluidPage(
        

         textAreaInput("noteArea", "Enter New Note Here",resize = "vertical"),
         actionButton("saveNote","Save")

        
      )
    ),
    
    tabItem(
      tabName="tab1",
      
      fluidPage(
        
        DTOutput("note_tbl"),
        h2("Hello 2nd tab")
      )
    )  
    
  )
)


shinyUI( 
  
  dashboardPage(

      header = header, 
      sidebar = sidebar,
      body = body,
      skin="black"
      
  )
)  












