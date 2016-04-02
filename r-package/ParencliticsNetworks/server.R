library(shiny)
source("scripts/loadCSV.R")

shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    infile <- input$data
    if(is.null(infile))
      return(NULL)
    loadCSV(infile$datapath, header = TRUE, sep = ",")
  })
  
  output$contents = DT::renderDataTable(datasetInput())
    
})
