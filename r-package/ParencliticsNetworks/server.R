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
    
    targets <- getFactorVariables(datasetInput())
    selectInput("target", "Select the target variable:", choices = colnames(datasetInput()[targets]))
  })
  
  output$descriptiveVariables = renderUI({
    
    if (identical(datasetInput(), '') || identical(datasetInput(),data.frame())) return(NULL)
    
    # TODO: Quitar variable objetivo de la lista
    selectInput("descriptiveVariables", "Variable", choices = colnames(datasetInput()))
    
  })
  
  selectedData = reactive({
    datasetInput()[,input$descriptiveVariables]
  })
  
  output$descriptivePlot = renderPlot({
    hist(selectedData(), main = 
          paste("Histogram of", colnames(datasetInput())[input$descriptiveVariables]),
         xlab = colnames(datasetInput())[input$descriptiveVariables],
         col = "blue"
    )
  })
  
  output$descriptiveSummary = renderPrint({
    summary(selectedData())
  })
    
})
