


shinyUI(
  dashboardPage(
  
  dashboardHeader(title = "Quick Example"),
  dashboardSidebar(textInput("text", "Text")),
  dashboardBody(
    valueBox(100, "Basic example"),
    tableOutput("mtcars")
    
    
    )
  
  
  
  )  
)

