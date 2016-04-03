library(shiny)
source("scripts/loadCSV.R")
source("scripts/getFactorVariables.R")

shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    infile <- input$data
    if(is.null(infile))
      return(NULL)
    loadCSV(infile$datapath, header = TRUE, sep = ",")
  })
  
  output$contents = DT::renderDataTable(datasetInput())
  
  output$target = renderUI({
    
    if (identical(datasetInput(), '') || identical(datasetInput(),data.frame())) return(NULL)
    
    cols <- getFactorVariables(datasetInput())
    selectInput("target", "Select the target variable:", choices=cols)  
    
  })
    
})
