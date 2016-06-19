library(shiny)
library(shinydashboard)
library(plotly)

dashboardPage(
  dashboardHeader(title = "Parenclitics Networks"),
  dashboardSidebar(sidebarMenu(
    menuItem("Upload CSV file", tabName = "home", icon = icon("dashboard")),
    menuItem("Descriptive Statistics", tabName = "descriptive", icon = icon("dashboard")),
    menuItem("Regression", tabName = "regression", icon = icon("dashboard")),
    menuItem("Parenclitics Networks", tabName = "parenclitics", icon = icon("th")),
    menuItem("Prediction", tabName = "prediction", icon = icon("th")),
    menuItem("Other prediction methods", tabName = "predictionML", icon = icon("th"))
    
  )),
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "home",
        fluidRow(
          column(width = 12,
             box(width = NULL,
                 title="Upload CSV",
                 fileInput('data', 'Choose file to upload', 
                   accept = c(
                     'text/csv',
                     'text/comma-separated-values',
                     '.csv'
                   )
                 )
             ),
             box(width=NULL,
                 title = "Target variable",
                 uiOutput("target")
             ),
             box(width = NULL,
              title = "Download report",
              downloadButton("downloadReport")
             ),
             box(width = NULL,
                 title = "Data",
                 DT::dataTableOutput("contents")
             )
          )
          
        )
      ),
      tabItem(tabName = "descriptive",
        fluidRow(
            box(uiOutput("descriptiveVariables")),
            box(title = "Summary of variable",
                DT::dataTableOutput("descriptiveSummary"))
        ),
        fluidRow(
          box(title = "Histogram",
              plotlyOutput("descriptivePlot")),
          box(title = "Histogram by target groups",
              plotlyOutput("descriptiveVariablesGroup")
          )
        )
      ),
      tabItem(tabName = "regression",
        fluidRow(
          box(width = 3,
            uiOutput("regressionVariable1"),
            uiOutput("regressionVariable2"),
            uiOutput("regressionMethod")
          ),
          box(width = 9,
            title="Regression plot by target",
            plotOutput("regressionPlot")
          )       
        )
      ),
      tabItem(tabName = "parenclitics",
        fluidRow(
          column(width = 3,
            box(width = NULL,
                uiOutput("normalVariable1"),
                uiOutput("normalVariable2"),
                uiOutput("normalRegressionMethod"),
                uiOutput("observation")
            )
          ),
          column(width = 9,
             box(width = NULL,
               title="Normal distribution by regression",
               plotOutput("normPlot")
             ),
             box(width = NULL,
               title="Parenclitic network by observation",
               plotOutput("normalPlot")
             )  
          )
        )
      ),
      tabItem(tabName = "prediction",
        fluidRow(
          column(width = 3,
             box(width = NULL,
              uiOutput("trainingSet"),
              uiOutput("regressionMethodPrediction")
             )
          ),
          column(width = 9,
             box(width = NULL,
                 title="Classification",
                 DT::dataTableOutput("classification")
             ),
             box(width = NULL,
                 title="Link density",
                 plotOutput("linkDensity")
             ),
             box(width = NULL,
                 title="Efficiency",
                 plotOutput("efficiency")
             ),
             box(width = NULL,
                 title="Clustering coefficient",
                 plotOutput("clustering")
             ),
             box(width = NULL,
                 title="Characteristic path length",
                 plotOutput("cpl")
             )
          )
        )
      ),
      tabItem(tabName = "predictionML",
        fluidRow(
          column(width = 12,
           box(width = NULL,
               title="Classification with decision tree",
               DT::dataTableOutput("classification_tree")
           ),
           box(width = NULL,
               title="Classification with neural networks",
               DT::dataTableOutput("classification_ann")
           ),
           box(width = NULL,
               title="Classification with SVM",
               DT::dataTableOutput("classification_svm")
           )
          )
        )
      )
    )
  )
)
