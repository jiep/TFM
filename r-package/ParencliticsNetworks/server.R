library(shiny)
library(plotly)
source("scripts/loadCSV.R")
source("scripts/getFactorVariables.R")
source("scripts/summaryfunction.R")
source("scripts/drawRegressionLines.R")


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
    
    cols = colnames(datasetInput())
    index = which(names(datasetInput())==input$target)
    cols = cols[-index]
    selectInput("descriptiveVariables", "Variable", choices = cols)
    
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
  
  output$descriptiveSummary = DT::renderDataTable(summaryfunction(selectedData()), options = list(dom = 't'))
  
  output$descriptiveVariablesGroup = renderPlotly({
    
    plot = ggplot(datasetInput(), aes(x=selectedData(), fill=datasetInput()[,input$target])) +
      geom_histogram(binwidth=.5, position="dodge")
    
    plot = plot + scale_fill_discrete(name=input$target) 
    
    plot = plot + scale_x_continuous(name=input$descriptiveVariables) + 
      scale_y_continuous(name="Count")
    
    
    p <- ggplotly(plot)
    p
    
  })
  
  output$regressionVariable1 = renderUI({
    
    if (identical(datasetInput(), '') || identical(datasetInput(),data.frame())) return(NULL)
    
    cols = colnames(datasetInput())
    index = which(names(datasetInput())==input$target)
    cols = cols[-index]
    selectInput("regressionVariable1", "Variable 1", choices = cols)
    
  })
  
  output$regressionVariable2 = renderUI({
    
    if (identical(datasetInput(), '') || identical(datasetInput(),data.frame())) return(NULL)
    
    cols = colnames(datasetInput())
    index = which(names(datasetInput())==input$target)
    cols = cols[-index]
    selectInput("regressionVariable2", "Variable 2", choices = cols)
    
  })
  
  output$regressionMethod = renderUI({
    
    # TODO: Quitar variable objetivo de la lista
    selectInput("regressionMethod", "Regression method", choices = c("linear"))
    
  })
  
  output$regressionPlot = renderPlot({
    plots = drawRegressionLines(datasetInput(), input$target)
    
    index1 = which(names(datasetInput())==input$regressionVariable1)
    index2 = which(names(datasetInput())==input$regressionVariable2)
    
    p=plots[[index1, index2]]
    
    p
  })
    
})
