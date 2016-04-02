library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Parenclitics Networks"),
  dashboardSidebar(sidebarMenu(
    menuItem("Upload CSV file", tabName = "home", icon = icon("dashboard")),
    menuItem("Descriptive Statistics", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Parenclitics Networks", tabName = "widgets", icon = icon("th"))
  )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              fluidRow(
                h1("Upload CSV file", align = "center")
              ),
              fluidRow(
                column(4),
                column(3, 
                   fileInput('data', 'Choose file to upload', 
                     accept = c(
                       'text/csv',
                       'text/comma-separated-values',
                       '.csv'
                     )
                   )
                ),
                column(4)
              ),
              fluidRow(
                column(1),
                column(10, DT::dataTableOutput('contents')),
                column(1)
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)
