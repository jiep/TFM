library(shiny)
library(plotly)
source("scripts/loadCSV.R")
source("scripts/getFactorVariables.R")

shinyServer(function(input, output) {
  
  observe({ 
    on.exit(
      assign("target", 
             input$target, .GlobalEnv) 
    ) 
  })
  
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
  
  output$descriptivePlot = renderPlotly({
    
    plot = ggplot(data=iris, aes(selectedData())) + geom_histogram() 
          
    plot = plot + scale_x_continuous(name=input$descriptiveVariables) + scale_y_continuous(name="Count")
    
    p <- ggplotly(plot)
    p
  })
  
  output$descriptiveSummary = renderPrint({
    summary(selectedData())
  })
  
  output$descriptiveVariablesGroup = renderPlotly({
    
    plot = ggplot(datasetInput(), aes(x=selectedData(), fill=datasetInput()[,input$target])) +
      geom_histogram(binwidth=.5, position="dodge")
    
    plot = plot + scale_fill_discrete(name=input$target) 
    
    plot = plot + scale_x_continuous(name=input$descriptiveVariables) + 
      scale_y_continuous(name="Count")
    
    
    p <- ggplotly(plot)
    p
    
  })
    
})
