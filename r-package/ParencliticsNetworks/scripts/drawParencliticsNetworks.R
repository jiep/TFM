source("scripts/setTargetVariable.R")

require('lme4')
require("igraph")

getParenclitsNetworks = function(data, target, observation, type = "linear"){
  targetIndex = setTargetVariable(data, target)
  adyMatrix = NULL
  if(targetIndex != -1){
    cols = colnames(data)
    classes = unique(data[,targetIndex])
    print(classes)
    columnsLength = length(cols)
    X = data[observation,]
    data = data[setdiff(1:dim(data)[1], observation),]
    adyMatrix = matrix(rep(0,(nrow = columnsLength -1)^2), nrow = columnsLength -1, ncol = columnsLength-1)
    columns = setdiff(1:columnsLength, c(targetIndex))
    colnames(adyMatrix) = colnames(data)[columns]
    rownames(adyMatrix) = colnames(data)[columns]
    
    print(columns)
    for(i in 1:length(columns)){
      for(j in 1:length(columns)){
        if(i<j){
          class = which(classes == X[1,targetIndex])
          probabilities = array()
          
          if(type == "exponential"){
            probabilities = array()
          
            formula = as.formula(paste("log(",colnames(data)[j],")", "~",
                                       paste(colnames(data)[i], "|", paste(colnames(data)[targetIndex], collapse = "+"),sep = "")))
            
            models = lmList(formula, data = data)
            cont = 1
            for(model in models){
              probabilities[cont] = dnorm(X[1,i],exp(X[1,i]*model$coefficients[[2]] + model$coefficients[[1]]), sd(model$residuals))
              cont = cont + 1
            }
            
            prob = array()
            if(sum(probabilities)!=0){
              prob = probabilities/sum(probabilities)
            }else{
              prob = 0
            }
            
            cat("i:", i, "j:", j, "\n")
            cat("Probabilidades: ")
            print(prob)
            
            index = which.max(x = prob)
            i_ = 1
            for(model in models){
              if(i_ == index){
                weight = abs(X[1,j] - exp(model$coefficients[[2]]*X[1,i] + model$coefficients[[1]]))
              }
              i_ = i_+1
            }
            
            adyMatrix[i,j] = weight
            adyMatrix[j,i] = weight
          }else if(type == "quadratic"){
            probabilities = array()
            
            formula = as.formula(paste("sqrt(",colnames(data)[j],")", "~",
                                       paste(colnames(data)[i], "|", paste(colnames(data)[targetIndex], collapse = "+"),sep = "")))
            
            models = lmList(formula, data = data)
            cont = 1
            for(model in models){
              probabilities[cont] = dnorm(X[1,i],sqrt(X[1,i]*model$coefficients[[2]] + model$coefficients[[1]]), sd(model$residuals))
              cont = cont + 1
            }
            
            prob = array()
            if(sum(probabilities)!=0){
              prob = probabilities/sum(probabilities)
            }else{
              prob = 0
            }
            
            cat("i:", i, "j:", j, "\n")
            cat("Probabilidades: ")
            print(prob)
            
            index = which.max(x = prob)
            i_ = 1
            for(model in models){
              if(i_ == index){
                weight = abs(X[1,j] - (model$coefficients[[2]]*X[1,i] + model$coefficients[[1]])^2)
              }
              i_ = i_+1
            }
            
            adyMatrix[i,j] = weight
            adyMatrix[j,i] = weight
          }else if(type == "reciprocal"){
            probabilities = array()
            
            formula = as.formula(paste("1/",colnames(data)[j], "~",
                                       paste(colnames(data)[i], "|", paste(colnames(data)[targetIndex], collapse = "+"),sep = "")))
            
            models = lmList(formula, data = data)
            cont = 1
            for(model in models){
              print(model)
              probabilities[cont] = 0
              if(X[1,i]*model$coefficients[[2]] + model$coefficients[[1]] != 0)
                probabilities[cont] = dnorm(X[1,i],1/(X[1,i]*model$coefficients[[2]] + model$coefficients[[1]]), sd(model$residuals))
              cont = cont + 1
            }
            
            prob = array()
            if(sum(probabilities)!=0){
              prob = probabilities/sum(probabilities)
            }else{
              prob = 0
            }
            
            cat("i:", i, "j:", j, "\n")
            cat("Probabilidades: ")
            print(prob)
            
            index = which.max(x = prob)
            i_ = 1
            for(model in models){
              if(i_ == index){
                weight = abs(X[1,j] - 1/(model$coefficients[[2]]*X[1,i] + model$coefficients[[1]]))
              }
              i_ = i_+1
            }
            
            adyMatrix[i,j] = weight
            adyMatrix[j,i] = weight
          }else {
            probabilities = array()
            
            formula = as.formula(paste(colnames(data)[j], "~",
                                       paste(colnames(data)[i], "|", paste(colnames(data)[targetIndex], collapse = "+"),sep = "")))
            
            models = lmList(formula, data = data)
            cont = 1
            for(model in models){
              probabilities[cont] = dnorm(X[1,i],sqrt(X[1,i]*model$coefficients[[2]] + model$coefficients[[1]]), sd(model$residuals))
              cont = cont + 1
            }
            
            prob = array()
            prob = probabilities/sum(probabilities)
            
            cat("i:", i, "j:", j, "\n")
            cat("Probabilidades: ")
            print(prob)
            
            index = which.max(x = prob)
            i_ = 1
            for(model in models){
              if(i_ == index){
                weight = abs(X[1,j] - (model$coefficients[[2]]*X[1,i] + model$coefficients[[1]]))
              }
              i_ = i_+1
            }
            
            adyMatrix[i,j] = weight
            adyMatrix[j,i] = weight
          }
        }
      }
    }
  }
  return(adyMatrix)
}

parencliticNetwork = function(trainingSet, testingSet, target, observation, type){

  targetIndex = setTargetVariable(trainingSet, target)
  cat(targetIndex)
  
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
    
    print(columns)
    for(i in 1:length(columns)){
      for(j in 1:length(columns)){
        if(i<j){
          if(type == "exponential"){
            formula = as.formula(paste("log(",colnames(trainingSet)[j],")" ,"~",
                                       paste(colnames(trainingSet)[i], "|", paste(colnames(trainingSet)[targetIndex], collapse = "+"),sep = "")))
            
            print(formula)
            
            models = lmList(formula, data = trainingSet)
            cont = 1
            probabilities = array()
            for(model in models){
              probabilities[cont] = dnorm(X[1,i],exp(X[1,i]*model$coefficients[[2]] + model$coefficients[[1]]), sd(model$residuals))
              cont = cont + 1
            }
            
            prob = array()
            prob = probabilities/sum(probabilities)
            
            cat("i:", i, "j:", j, "\n")
            cat("Probabilidades: ")
            print(prob)
            
            index = which.max(x = prob)
            i_ = 1
            for(model in models){
              if(i_ == index){
                weight = abs(X[1,j] - exp(model$coefficients[[2]]*X[1,i] + model$coefficients[[1]]))
              }
              i_ = i_+1
            }
            cat("Indice mayor: ", index, "\n", "weight: ", weight, "classes[ind]:", classes[index], "\n")
            adyMatrix[i,j] = weight
            adyMatrix[j,i] = weight
          }else if(type == "quadratic"){
            formula = as.formula(paste("sqrt(", colnames(trainingSet)[j],")", "~",
                                       paste(colnames(trainingSet)[i], "|", paste(colnames(trainingSet)[targetIndex], collapse = "+"),sep = "")))
            
            models = lmList(formula, data = trainingSet)
            cont = 1
            probabilities = array()
            for(model in models){
              probabilities[cont] = dnorm(X[1,i],(X[1,i]*model$coefficients[[2]] + model$coefficients[[1]])^2, sd(model$residuals))
              cat("Prob: cont ", cont, ":", probabilities[cont], "\n")
              cont = cont + 1
            }
            
            prob = array()
            if(sum(probabilities) != 0){
              prob = probabilities/sum(probabilities)
            }
            prob = 0 
            
            cat("i:", i, "j:", j, "\n")
            cat("Probabilidades: ")
            print(prob)
            
            index = which.max(x = prob)
            i_ = 1
            for(model in models){
              if(i_ == index){
                weight = abs(X[1,j] - (model$coefficients[[2]]*X[1,i] + model$coefficients[[1]])^2)
              }
              i_ = i_+1
            }
            cat("Indice mayor: ", index, "\n", "weight: ", weight, "classes[ind]:", classes[index], "\n")
            adyMatrix[i,j] = weight
            adyMatrix[j,i] = weight
          }else if(type == "reciprocal"){
            formula = as.formula(paste("1/",colnames(trainingSet)[j], "~",
                                       paste(colnames(trainingSet)[i], "|", paste(colnames(trainingSet)[targetIndex], collapse = "+"),sep = "")))
            
            models = lmList(formula, data = trainingSet)
            cont = 1
            probabilities = array()
            for(model in models){
              probabilities[cont] = dnorm(X[1,i],1/(X[1,i]*model$coefficients[[2]] + model$coefficients[[1]]), sd(model$residuals))
              cont = cont + 1
            }
            
            prob = array()
            if(sum(probabilities)!=0){
              prob = probabilities/sum(probabilities)
            }else{
              prob = 0
            }
            
            cat("i:", i, "j:", j, "\n")
            cat("Probabilidades: ")
            print(prob)
            
            index = which.max(x = prob)
            i_ = 1
            for(model in models){
              if(i_ == index){
                weight = abs(X[1,j] - (model$coefficients[[2]]*X[1,i]) + model$coefficients[[1]])
              }
              i_ = i_+1
            }
            cat("Indice mayor: ", index, "\n", "weight: ", weight, "classes[ind]:", classes[index], "\n")
            adyMatrix[i,j] = weight
            adyMatrix[j,i] = weight
          }else{
            formula = as.formula(paste(colnames(trainingSet)[j], "~",
                                       paste(colnames(trainingSet)[i], "|", paste(colnames(trainingSet)[targetIndex], collapse = "+"),sep = "")))
            
            models = lmList(formula, data = trainingSet)
            cont = 1
            probabilities = array()
            for(model in models){
              probabilities[cont] = dnorm(X[1,i],X[1,i]*model$coefficients[[2]] + model$coefficients[[1]], sd(model$residuals))
              cont = cont + 1
            }
            
            prob = array()
            if(sum(probabilities) != 0){
              prob = probabilities/sum(probabilities)
            }
            
            cat("i:", i, "j:", j, "\n")
            cat("Probabilidades: ")
            print(prob)
            
            index = which.max(x = prob)
            i_ = 1
            for(model in models){
              if(i_ == index){
                weight = abs(X[1,j] - (model$coefficients[[2]]*X[1,i]) + model$coefficients[[1]])
              }
              i_ = i_+1
            }
            cat("Indice mayor: ", index, "\n", "weight: ", weight, "classes[ind]:", classes[index], "\n")
            adyMatrix[i,j] = weight
            adyMatrix[j,i] = weight
          }
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
                  vertex.size=25*degree(network),
                  layout=layout.fruchterman.reingold)
  
  return(p)
}