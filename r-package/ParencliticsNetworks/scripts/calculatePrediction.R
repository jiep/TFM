source("scripts/drawParencliticsNetworks.R")

require("igraph")
require("e1071")
require("nnet")

calculatePrediction = function(data, target, percentage, type){
  n = floor(nrow(data)*as.numeric(percentage)) + 1
  trainingIndexes = sort(sample(nrow(data), size = n))
  testingIndexes = setdiff(1:dim(data)[1], trainingIndexes)
  trainingSet = data[trainingIndexes,]
  testingSet  = data[-trainingIndexes,]
  labels_testing = data[testingIndexes,target]
  
  cat("labels indexes", length(labels_testing), "\n")
  
  parencliticsnetworks = array()
  clustering_coefficient = array()
  link_density = array()
  efficiency = array()
  caracteristic_path_length = array()
  labels = data[trainingIndexes, target]
  for(observation in trainingIndexes){
    ady = getParenclitsNetworks(data, target, observation)
    cat("calculada red parenclítica\n")
    network = graph.adjacency(ady, weighted=T, mode = "undirected")
    network_nodes = length(V(network))
    
    matrix_distance = shortest.paths(network,weights=E(network)$distance)

    clustering_coefficient[observation] = ifelse(!is.nan(transitivity(network)), transitivity(network), 0)
    #cat(clustering_coefficient[observation])
    coef = 1/(network_nodes*(network_nodes - 1))
    link_density[observation] = coef*sum(ady)
    
    efficiency[observation] = coef*sum(1/matrix_distance[row(matrix_distance)!=col(matrix_distance)])
    
    caracteristic_path_length[observation] = ifelse(!is.infinite(coef*sum(matrix_distance[row(matrix_distance)!=col(matrix_distance)])), coef*sum(matrix_distance[row(matrix_distance)!=col(matrix_distance)]), 0)
    cat(paste(caracteristic_path_length[observation],"\n"))
    
  }
  
  cat("Acaba training\n")
  
  result_training = as.data.frame(cbind(clustering_coefficient, link_density, efficiency, caracteristic_path_length))
  result_training = result_training[complete.cases(result_training),]
  result_training[,"labels"] = labels
  result_training[,"observation"] = as.numeric(rownames(result_training))
  
  #p1 = ggplot(result_training, aes(x=observation, y=link_density, colour=labels, group=labels)) +
  #  geom_line() + ggtitle("Link density")
  
  #p2 = ggplot(result_training, aes(x=observation, y=efficiency, colour=labels, group=labels)) +
  #  geom_line() + ggtitle("Efficiency")
  
  #p3 = ggplot(result_training, aes(x=observation, y=clustering_coefficient, colour=labels, group=labels)) +
  #  geom_line() + ggtitle("Clustering coeffcient")
  
  #p4 = ggplot(result_training, aes(x=observation, y=caracteristic_path_length, colour=labels, group=labels)) +
  #  geom_line() + ggtitle("Characteristic path length")
  
  plot_measures = NULL
  
  
  result_training[,"observation"] = NULL
  
  parencliticsnetworks = array()
  clustering_coefficient = array()
  link_density = array()
  efficiency = array()
  caracteristic_path_length = array()
  cat("Acaba primer for \n")
  for(test in testingIndexes){
    cat("Entra en segundo for")
    ady = parencliticNetwork(trainingSet, testingSet, target = target, test, type)
    cat("Calcula red parenclítica de test \n")
    network = graph.adjacency(ady, weighted=T, mode = "undirected")
    network_nodes = length(V(network))
    
    matrix_distance = shortest.paths(network,weights=E(network)$distance)
    
    clustering_coefficient[test] = transitivity(network)
    coef = 1/(network_nodes*(network_nodes - 1))
    link_density[test] = coef*sum(ady)
    
    efficiency[test] = coef*sum(1/matrix_distance[row(matrix_distance)!=col(matrix_distance)])
    caracteristic_path_length[test] = coef*sum(matrix_distance[row(matrix_distance)!=col(matrix_distance)])
    
  }
  
  cat("Acaba testing\n")
  
  
  result_testing = as.data.frame(cbind(clustering_coefficient, link_density, efficiency, caracteristic_path_length))
  result_testing = result_testing[complete.cases(result_testing),]
  cat("Testing indexes")
  print(testingIndexes)
  
  model = nnet(labels ~ ., data = result_training, size=50, maxit = 1000, MaxNWts = 10000)
  cat("Acaba entrenamiento red neuronal")
  predicts = predict(model, result_testing, type = "class")
  #predict = predict(model, result_testing)
  
  
  predicts = unname(predicts)
  
  
  View(testingSet)
  
  results_percentage = sum(predicts == labels_testing)/length(predicts)
  cat("results_percentage\n")
  print(results_percentage)
  
  sum(predicts == labels_testing)/length(predicts)
  
  results = as.data.frame(cbind(as.character(predicts), as.character(labels_testing)))

  results["classification"] = ifelse(predicts == labels_testing, "Good", "Bad")
  colnames(results) = c("predicted", "labels", "classification")
  rownames(results) = as.numeric(rownames(testingSet))
  
  cat("results\n")
  print(results)
  
  return(results)
}