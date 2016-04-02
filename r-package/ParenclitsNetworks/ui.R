library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Parenclitics Networks"),
  dashboardSidebar(sidebarMenu(
    menuItem("Descriptive Statistics", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Parenclitics Networks", tabName = "widgets", icon = icon("th"))
  )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)
