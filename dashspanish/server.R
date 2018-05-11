



server <- function(input, output, session) {
  
  
  output$mtcars <- renderTable(head(mtcars))

  
  
}  