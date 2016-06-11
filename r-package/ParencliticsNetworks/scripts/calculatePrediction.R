source("scripts/drawParencliticsNetworks.R")

require("igraph")
require("e1071")

calculatePrediction = function(data, target, percentage, type){
  n = floor(nrow(data)*as.numeric(percentage)) + 1
  trainingIndexes = sort(sample(1:dim(data)[1], size = n))
  testingIndexes = setdiff(1:dim(data)[1], trainingIndexes)
  trainingSet = data[trainingIndexes,]
  testingSet  = data[-trainingIndexes,]
  labels_testing = data[testingIndexes,target]
  
  parencliticsnetworks = array()
  clustering_coefficient = array()
  link_density = array()
  efficiency = array()
  caracteristic_path_length = array()
  labels = data[trainingIndexes, target]
  for(observation in trainingIndexes){
    ady = getParenclitsNetworks(data, target, observation)
    network = graph.adjacency(ady, weighted=T, mode = "undirected")
    network_nodes = length(V(network))
    
    matrix_distance = shortest.paths(network,weights=E(network)$distance)
    
    clustering_coefficient[observation] = transitivity(network)
    coef = 1/(network_nodes*(network_nodes - 1))
    link_density[observation] = coef*sum(ady)
    
    efficiency[observation] = coef*sum(1/matrix_distance[row(matrix_distance)!=col(matrix_distance)])
    caracteristic_path_length[observation] = coef*sum(matrix_distance[row(matrix_distance)!=col(matrix_distance)])
  }
  
  result_training = as.data.frame(cbind(clustering_coefficient, link_density, efficiency, caracteristic_path_length))
  result_training = result_training[complete.cases(result_training),]
  result_training[,"labels"] = labels
  
  parencliticsnetworks = array()
  clustering_coefficient = array()
  link_density = array()
  efficiency = array()
  caracteristic_path_length = array()
  for(test in testingIndexes){
    ady = parencliticNetwork(trainingSet, testingSet, target = target, test, type = "linear")
    network = graph.adjacency(ady, weighted=T, mode = "undirected")
    network_nodes = length(V(network))
    
    matrix_distance = shortest.paths(network,weights=E(network)$distance)
    
    clustering_coefficient[test] = transitivity(network)
    coef = 1/(network_nodes*(network_nodes - 1))
    link_density[test] = coef*sum(ady)
    
    efficiency[test] = coef*sum(1/matrix_distance[row(matrix_distance)!=col(matrix_distance)])
    caracteristic_path_length[test] = coef*sum(matrix_distance[row(matrix_distance)!=col(matrix_distance)])
    
    result_testing = as.data.frame(cbind(clustering_coefficient, link_density, efficiency, caracteristic_path_length))
    result_testing = result_testing[complete.cases(result_testing),]
  }
  
  model = svm(labels ~ . , data = result_training, scale = FALSE)
  predicts = predict(model, testingSet)
  
  predicts = unname(predicts)
  
  results_percentage = sum(predicts == labels_testing)/length(predicts)
  
  sum(predicts == labels_testing)/length(predicts)
  
  results = as.data.frame(cbind(as.character(predicts), as.character(labels_testing)))
  results["classification"] = ifelse(predicts == labels_testing, "Good", "Bad")
  colnames(results) = c("predicted", "labels", "classification")
  rownames(results) = rownames(testingSet)
  
  return(list(results_percentage, results))
}