

messageData <-data.frame(from=c("ian","ben","gabby","cat"),
                         message=c("fuck","shit","ass","dick"),
                         # icon=c(paste0(icon("question")),paste0(icon("question")),
                         #        paste0(icon("life-ring")),paste0(icon("life-ring"))
                         #        )
                         icon=c("question","question","life-ring","life-ring")
                         )

# taskData <- data.frame()
# notificationData <- data.frame()
# taskItem(text = row[["message"]], color = row[["color"]], value = row[["value"]])
# notificationItem(text = row[["message"]], status = row[["status"]])

server <- function(input, output, session) {
  
  
  output$mtcars <- renderTable(head(mtcars))
  
  
  output$messageMenu <- renderMenu({
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])})
    
    dropdownMenu(type = "messages", .list = msgs)
    
  })
  
  output$menu <- renderMenu({
    sidebarMenu(
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                        label = "Search..."),
      tags$br(),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets"),
      
      menuItem('y', tabName = 'y',
               icon = icon('line-chart'),
               menuItem('a',
                        tabName = 'a',
                        icon = icon('line-chart')),
               menuItem('b',
                        tabName = 'b',
                        icon = icon('line-chart'),
                        menuSubItem('l',
                                    tabName = 'l',
                                    icon = icon('line-chart')),
                        menuSubItem('m',
                                    tabName = 'm',
                                    icon = icon('line-chart'))),
               menuItem('c',
                        tabName = 'c',
                        icon = icon('line-chart')))
      
      
    )
  })
  
  
  
  
  
  
  
  

  
  
}  