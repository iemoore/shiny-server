


# Simple header -----------------------------------------------------------

dm <- dropdownMenu(type="messages")
mm <- dropdownMenu(type="notifications")
tm <- dropdownMenu(type="tasks")

header <- dashboardHeader(title="Sentence Dashboard", dm, mm, tm)

# No sidebar --------------------------------------------------------------

sm <- sidebarMenu(id = "tabs",
  
  searchInput(
    inputId = "id", 
    label = "Enter your search :", 
    placeholder = "This is a placeholder", 
    btnSearch = icon("search"), 
    # btnReset = icon("remove"), 
    width = "100%"
  ),
  
  menuItem(
    text="Choose data",
    tabName="main",
    icon=icon("database")),
  
  menuItem(
    text="Flashcards",
    tabName="flash",
    icon=icon("forward"))
  
)

sidebar <- dashboardSidebar(sm)

# Compose dashboard body --------------------------------------------------


body <- dashboardBody(
  
  useShinyjs(),
  includeScript("solid/js/key.js"),
  includeScript("solid/js/clicktest.js"),
  includeScript("solid/js/flashClick.js"),
  useShinyalert(),
  inlineCSS('#flashPanel {min-height: 150px;}'),
  inlineCSS('#flashPanel2 {min-height: 150px;}'),
  
  tabItems(
    tabItem(
      tabName="main",
      
      fluidPage(
        
        checkboxGroupButtons(
          inputId = "which_df", label = "Make a choice :", 
          choices = which_data,
          justified = TRUE, status = "primary",
          selected = c("W", "A"),
          checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                           no = icon("remove", lib = "glyphicon"))
        ),
        
        pickerInput(
          inputId = "myPicker", 
          label = "Choose desired tenses", 
          choices = tense_Names2, 
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"
          ), 
          multiple = TRUE
        ),
        
        DTOutput('dataNowTable'),
        DTOutput('excludeTable')
 
        
      )
    ),
    
    tabItem(
      tabName="flash",
      
      fluidPage(
        
        bsModal(id="sf_Modal", "Conjugations", "modTrig",size = "large",
                
                tags$a(id="hereModal1")
                
        ),
        
        dropdownButton(
          tags$h3("List of Input"),
          materialSwitch(inputId = "ms_ss", 
                         label = "Save Session", value = TRUE, 
                         status = "primary"),
          
          materialSwitch(inputId = "ms_aa", 
                         label = "Autoplay Audio", value = TRUE, 
                         status = "primary"),
          
          materialSwitch(inputId = "ms_sh", 
                         label = "Show Hover Help", value = TRUE, 
                         status = "primary"),
          
          circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
          tooltip = tooltipOptions(title = "Click to see inputs !")
        ),
        
        wellPanel(id="flashPanel",a(id="here1")),
        wellPanel(id="flashPanel2",a(id="here2")),
        
        inlineDing(uiOutput("flashBack")),
        inlineDing(uiOutput("flashShow")),
        inlineDing(uiOutput("flashNext")),
        inlineDing(uiOutput("flashAudioWeb"))
        
        
        
      )
    )  
    
  )
)







shinyUI( 
  
  dashboardPage(

      header = header, 
      sidebar = sidebar,
      body = body,
      skin="blue"
      
  )
)  












