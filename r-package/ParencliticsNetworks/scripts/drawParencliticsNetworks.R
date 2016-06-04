source("scripts/setTargetVariable.R")

require('lme4')
require("igraph")

getParenclitsNetworks = function(data, target, observation){
  targetIndex = setTargetVariable(data, target)
  adyMatrix = NULL
  if(targetIndex != -1){
    cols = colnames(data)
    classes = unique(data[,targetIndex])
    print(classes)
    columnsLength = length(cols)
    X = data[observation,]
    #data[-observation,]
    adyMatrix = matrix(rep(0,(nrow = columnsLength -1)^2), nrow = columnsLength -1, ncol = columnsLength-1)
    columns = setdiff(1:columnsLength, c(targetIndex))
    colnames(adyMatrix) = colnames(data)[columns]
    rownames(adyMatrix) = colnames(data)[columns]
    
    print(columns)
    for(i in 1:length(columns)){
      for(j in 1:length(columns)){
        if(i<j){
          formula = as.formula(paste(colnames(data)[j], "~",
                                     paste(colnames(data)[i], "|", paste(colnames(data)[targetIndex], collapse = "+"),sep = "")))
  
          models = lmList(formula, data = data)
          class = which(classes == X[1,targetIndex])
          cont = 1
          probabilities = array()
          for(model in models){
            probabilities[cont] = dnorm(X[1,i],X[1,i]*model$coefficients[[2]], sd(model$residuals))
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
              weight = abs(X[1,i] - model$coefficients[[2]]*X[1,i])
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
  return(adyMatrix)
}

drawParenclitsNetworks = function(data, target, observation){
  ady = getParenclitsNetworks(data, target, observation)
  
  network = graph.adjacency(ady, weighted=T, mode = "undirected")
  
  c_scale = colorRamp(c('green','red'))
  
  p=plot(network, edge.color=apply(c_scale(E(network)$weight/sum(E(network)$weight)), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255)),
                  edge.width=E(network)$weight,
                  vertex.label.color = "black",
                  vertex.color = "white",
                  vertex.size=25*degree(network),
                  layout=layout.fruchterman.reingold)
  
  return(p)
}