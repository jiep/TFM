library(shiny)
library(plotly)
require("rmarkdown")
require("stringi")
require("pander")
library("gridExtra")


source("scripts/loadCSV.R")
source("scripts/getFactorVariables.R")
source("scripts/summaryfunction.R")
source("scripts/drawRegressionLines.R")
source("scripts/drawParencliticsNetworks.R")
source("scripts/drawNormalPlot.R")
source("scripts/calculatePrediction.R")
source("scripts/calculatePredictionML.R")
source("scripts/multiplot.R")

shinyServer(function(input, output) {
  
  observe({ 
    on.exit(
      assign("input", 
        input$data$datapath, .GlobalEnv) 
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
    
    
    plot = ggplot(data=datasetInput(), aes(selectedData())) + geom_histogram(binwidth=.5, position="dodge") 
          
    plot = plot + scale_x_continuous(name=input$descriptiveVariables) + scale_y_continuous(name="Count")
    
    p = ggplotly(plot)
    p
  })
  
  output$descriptiveSummary = DT::renderDataTable(summaryfunction(selectedData()), options = list(dom = 't'))
  
  output$descriptiveVariablesGroup = renderPlotly({
    
    
    plot = ggplot(datasetInput(), aes(x=selectedData(), fill=datasetInput()[,input$target])) +
      geom_histogram(binwidth=.5, position="dodge")
    
    plot = plot + scale_fill_discrete(name=input$target) 
    
    plot = plot + scale_x_continuous(name=input$descriptiveVariables) + 
      scale_y_continuous(name="Count")
    
    
    p = ggplotly(plot)
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
    selectInput("regressionMethod", "Regression method", choices = c("linear", "exponential", "quadratic", "reciprocal"))
    
  })
  
  output$regressionPlot = renderPlot({
    index1 = which(names(datasetInput())==input$regressionVariable1)
    index2 = which(names(datasetInput())==input$regressionVariable2)
    
    plots = drawRegressionLines(datasetInput(), input$target, index1, index2, input$regressionMethod)
    
    p=plots[[index1, index2]]
    
    p
  })
  
  output$normalVariable1 = renderUI({
    
    if (identical(datasetInput(), '') || identical(datasetInput(),data.frame())) return(NULL)
    
    cols = colnames(datasetInput())
    index = which(names(datasetInput())==input$target)
    cols = cols[-index]
    selectInput("normalVariable1", "Variable 1", choices = cols)
    
  })
  
  output$normalVariable2 = renderUI({
    
    if (identical(datasetInput(), '') || identical(datasetInput(),data.frame())) return(NULL)
    
    cols = colnames(datasetInput())
    index1 = which(names(datasetInput())==input$target)
    index2 = which(names(datasetInput())==input$normalVariable1)
    cols = cols[-c(index1,index2)]
    selectInput("normalVariable2", "Variable 2", choices = cols)
    
  })
  
  output$normalRegressionMethod = renderUI({
    
    # TODO: Quitar variable objetivo de la lista
    selectInput("normalRegressionMethod", "Regression method", choices = c("linear", "exponential", "quadratic", "reciprocal"))
    
  })
  
  output$observation = renderUI({
    
    selectInput("observation", "Observation", choices = 1:dim(datasetInput())[1])
    
  })
  
  output$normPlot = renderPlot({
    plot = drawNormalPlot(datasetInput(), input$target, input$normalVariable1, input$normalVariable2, input$observation, input$normalRegressionMethod)
    print(plot)
    #p = ggplotly(plot)
    #p
    
  })
  
  output$normalPlot = renderPlot({
    plot = drawParenclitsNetworks(datasetInput(), input$target, input$observation, input$normalRegressionMethod)
    plot

  })
  
  predictions = reactive({
    calculatePrediction(datasetInput(), input$target, input$trainingSet, input$regressionMethodPrediction)
  })
  
  output$trainingSet = renderUI({
    
    selectInput("trainingSet", "Training set", choices = c(0.8, 0.9, 0.95))
    
  })
  
  output$regressionMethodPrediction = renderUI({
    
    selectInput("regressionMethodPrediction", "Regression Method", choices = c("linear", "exponential", "quadratic", "reciprocal"))
    
  })
  
  output$classification = DT::renderDataTable({
    DT::datatable(predictions()[[2]],options = list(dom = 't'))
  })
  
  
  output$linkDensity = renderPlot({
    plot = predictions()[[3]][[1]]
    print(plot)
    #p = ggplotly(plot)
    #p
    
  })
  
  output$efficiency = renderPlot({
    plot = predictions()[[3]][[2]]
    print(plot)
    #p = ggplotly(plot)
    #p
    
  })
  
  output$clustering = renderPlot({
    plot = predictions()[[3]][[3]]
    print(plot)
    #p = ggplotly(plot)
    #p
    
  })
  
  output$cpl = renderPlot({
    plot = predictions()[[3]][[4]]
    print(plot)
    #p = ggplotly(plot)
    #p
    
  })
  
  predictionsML = reactive({
    calculatePredictionML(datasetInput(), input$target, input$trainingSet)
  })
  
  output$classification_tree = DT::renderDataTable({
    DT::datatable(predictionsML()[[1]], options = list(dom = 't'))
  })
  
  output$classification_svm = DT::renderDataTable({
    DT::datatable(predictionsML()[[2]], options = list(dom = 't'))
  })
  
  output$classification_ann = DT::renderDataTable({
    DT::datatable(predictionsML()[[3]], options = list(dom = 't'))
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report.pdf')
    },
    content = function(file) {
      cat("Directorio actual:", getwd())
      src <- normalizePath('report.RMD')
      
      owd <- setwd(tempdir())
      cat(owd)
      on.exit(setwd(owd))
      file.copy(src, 'report.RMD', overwrite = TRUE)
      
      cat("Path:", input$data$datapath, "\n")
      
      out <- rmarkdown::render('report.RMD', encoding = 'UTF-8',
         params = list(path = gsub("\\", "/", input$data$datapath, fixed=TRUE),
                       target = input$target,
                       percentage = input$trainingSet
                      ))
      file.rename(out, file)
    }
  )
  
  
})
