library(shiny)
source("scripts/loadCSV.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #file <- input$data
  
  datasetInput <- reactive({
    infile <- input$data
    if(is.null(infile))
      return(NULL)
    loadCSV(infile$datapath, header = TRUE, sep = ",")
  })
  
  output$contents = DT::renderDataTable(datasetInput())
    
})
