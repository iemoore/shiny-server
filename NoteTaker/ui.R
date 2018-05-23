


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
  }")),
  useShinyalert(),

  
  tabItems(
    tabItem(
      tabName="main",
      
      fluidPage(
        
        wellPanel(
          
          selectizeInput(
            'typeInput', label = NULL, choices = unique(notedf$type),
            options = list(create = TRUE)
          ),
          textAreaInput("noteArea", "Enter New Note Here",
                        resize = "vertical",rows = 10),
          actionButton("saveNote","Save")
          
        )
        
      )
    ),
    
    tabItem(
      tabName="tab1",
      
      fluidPage(
        
        pickerInput(
          inputId = "typePicker", 
          label = "Choose desired types", 
          choices = as.character(unique(notedf$type)), 
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"
          ), 
          multiple = TRUE,
          selected = as.character(unique(notedf$type))
        ),
        
        DTOutput("note_tbl")
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












