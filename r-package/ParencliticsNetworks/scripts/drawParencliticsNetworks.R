source("scripts/setTargetVariable.R")

require('lme4')
require("igraph")

getParenclitsNetworks = function(data, target, observation, type = "linear"){
  targetIndex = setTargetVariable(data, target)
  adyMatrix = NULL
  if(targetIndex != -1){
    cols = colnames(data)
    classes = unique(data[,targetIndex])
    columnsLength = length(cols)
    X = data[observation,]
    data = data[setdiff(1:dim(data)[1], observation),]
    adyMatrix = matrix(rep(0,(nrow = columnsLength -1)^2), nrow = columnsLength -1, ncol = columnsLength-1)
    columns = setdiff(1:columnsLength, c(targetIndex))
    colnames(adyMatrix) = colnames(data)[columns]
    rownames(adyMatrix) = colnames(data)[columns]
    
    for(i in 1:length(columns)){
      for(j in 1:length(columns)){
        if(i<j){
          class = which(classes == X[1,targetIndex])
          probabilities = array()
          
          # Type `linear`
            probabilities = array()
            
            formula = as.formula(paste(colnames(data)[j], "~",
                                       paste(colnames(data)[i], "|", paste(colnames(data)[targetIndex], collapse = "+"),sep = "")))
            
            #print(formula)
            
            models = lmList(formula, data = data)
            #cat("models")
            #print(models)
            cont = 1
            for(model in models){
              probabilities[cont] = dnorm(X[1,i],(X[1,i]*model$coefficients[[2]] + model$coefficients[[1]]), sd(model$residuals))
              cont = cont + 1
            }
            
            #cat("probabilities")
            #print(probabilities)
            #cat("\n")
            
            prob = array()
            if(sum(probabilities) != 0 && !any(is.na(probabilities))){
              #cat("Entra en la primera condición")
              #prob = probabilities/sum(probabilities)
            }else{
              #cat("Entra en la segunda condición")
              prob = rep(0, length(probabilities))
            }
            #cat("prob")
            #print(prob)
            #cat("\n")
            
            index = which.max(x = prob)
            #cat("index", index)
            i_ = 1
            for(model in models){
              if(is.null(index) && i_ == index){
                weight = abs(X[1,j] - (model$coefficients[[2]]*X[1,i] + model$coefficients[[1]]))
              }else{
                weight = 0.5
              }
              i_ = i_+1
            }
            
            adyMatrix[i,j] = weight
            adyMatrix[j,i] = weight
          }
        }
    }
  }
  return(adyMatrix)
}

parencliticNetwork = function(trainingSet, testingSet, target, observation, type){

  targetIndex = setTargetVariable(trainingSet, target)

  adyMatrix = NULL
  if(targetIndex != -1){
    cols = colnames(trainingSet)
    classes = unique(trainingSet[,targetIndex])
    columnsLength = length(cols)
    
    X = testingSet[rownames(testingSet) == observation,]
    adyMatrix = matrix(rep(0,(nrow = columnsLength -1)^2), nrow = columnsLength -1, ncol = columnsLength-1)
    columns = setdiff(1:columnsLength, c(targetIndex))
    colnames(adyMatrix) = colnames(trainingSet)[columns]
    rownames(adyMatrix) = colnames(trainingSet)[columns]
    
    for(i in 1:length(columns)){
      for(j in 1:length(columns)){
        if(i<j){
            formula = as.formula(paste(colnames(trainingSet)[j], "~",
                                       paste(colnames(trainingSet)[i], "|", paste(colnames(trainingSet)[targetIndex], collapse = "+"),sep = "")))
            
            models = lmList(formula, data = trainingSet)
            cont = 1
            probabilities = array()
            for(model in models){
              if(!is.na(dnorm(X[1,i],X[1,i]*model$coefficients[[2]] + model$coefficients[[1]], sd(model$residuals)))){
                probabilities[cont] = dnorm(X[1,i],X[1,i]*model$coefficients[[2]] + model$coefficients[[1]], sd(model$residuals))
              }else{
                probabilities[cont] = 0
              }
              cont = cont + 1
            }
            
            prob = array()
            if(sum(probabilities) != 0 && !any(is.na(probabilities))){
              #cat("Entra en la primera condición")
              #prob = probabilities/sum(probabilities)
            }else{
              #cat("Entra en la segunda condición")
              prob = rep(0, length(probabilities))
            }
            
            index = which.max(x = prob)
            i_ = 1
            for(model in models){
              if(is.null(index) && i_ == index){
                weight = abs(X[1,j] - (model$coefficients[[2]]*X[1,i] + model$coefficients[[1]]))
              }else{
                weight = 0.5
              }
              i_ = i_+1
            }
            adyMatrix[i,j] = weight
            adyMatrix[j,i] = weight
          }
        }
      }
  }
  return(adyMatrix)
}


drawParenclitsNetworks = function(data, target, observation, type){
  ady = getParenclitsNetworks(data, target, observation, type)
  
  network = graph.adjacency(ady, weighted=T, mode = "undirected")
  
  c_scale = colorRamp(c('green','red'))
  
  x = E(network)$weight
  
  x_ = (x-min(x))/(max(x) - min(x))
  
  p=plot(network, edge.color=apply(c_scale(x_), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255)),
                  edge.width=2*E(network)$weight,
                  vertex.label.color = "black",
                  vertex.color = "white",
                  vertex.size=5*degree(network),
                  layout=layout.fruchterman.reingold)
  
  return(p)
}