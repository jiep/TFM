library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Parenclitics Networks"),
  dashboardSidebar(sidebarMenu(
    menuItem("Upload CSV file", tabName = "home", icon = icon("dashboard")),
    menuItem("Descriptive Statistics", tabName = "descriptive", icon = icon("dashboard")),
    menuItem("Parenclitics Networks", tabName = "parenclitics", icon = icon("th")),
    menuItem("Prediction", tabName = "prediction", icon = icon("th"))
    
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
                column(4,
                  uiOutput("target"))
              ),
              fluidRow(
                column(1),
                column(10, DT::dataTableOutput('contents')),
                column(1)
              )
      ),
      
      tabItem(tabName = "descriptive",
          h2("Widgets tab content")
      ),
      tabItem(tabName = "parenclitics",
          h2("Widgets tab content")
      ),
      tabItem(tabName = "prediction",
          h2("Widgets tab content")
      )
    )
  )
)
