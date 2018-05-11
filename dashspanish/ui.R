




header <- source("ui/header.R",local = T)$value



sidebar <- dashboardSidebar(sidebarMenuOutput("menu"))




body <- dashboardBody(
  
  tabItems(
    
    tabItem(tabName = "dashboard",
            box(
              title = "Data box", status = "primary", solidHeader = TRUE,
              collapsible = TRUE,
              h3("Hello")
            )
    ),
    
    tabItem(tabName = "widgets",
            box(
              title = "Data Box", status = "primary", solidHeader = TRUE,
              collapsible = TRUE,
              h3("Whats up")
            )
    )
    
  )
  
)
  








shinyUI(dashboardPage(header, sidebar, body))