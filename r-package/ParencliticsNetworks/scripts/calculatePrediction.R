source("scripts/drawParencliticsNetworks.R")
source("scripts/multiplot.R")

require("igraph")
require("e1071")

calculatePrediction = function(data, target, percentage, type){
  n = floor(nrow(data)*as.numeric(percentage)) + 1
  trainingIndexes = sort(sample(nrow(data), size = n))
  testingIndexes = setdiff(1:dim(data)[1], trainingIndexes)
  trainingSet = data[trainingIndexes,]
  testingSet  = data[-trainingIndexes,]
  labels_testing = data[testingIndexes,target]
  
  #cat("labels indexes", length(labels_testing), "\n")
  
  for(interval in seq(from = 0.1, to = 1, by = 0.01)){
    cat(paste("Interval:", interval, "\n"))
    parencliticsnetworks = array()
    clustering_coefficient = array()
    link_density = array()
    efficiency = array()
    caracteristic_path_length = array()
    labels = data[trainingIndexes, target]
    network_list = list()
    for(observation in trainingIndexes){
      #cat(paste("Observation:", observation, "\n"))
      ady = getParenclitsNetworks(data, target, observation)
      network_list[[length(network_list)+1]] = graph.adjacency(ady, weighted=T, mode = "undirected")
    }
    min = Inf
    max = -Inf
    nets =  rep(network_list,1)
    for(network in network_list){
      weights = E(network)$weight
      if(min(weights, na.rm = TRUE) < min){
        min = min(weights, na.rm = TRUE)
      }
      if(max(weights, na.rm = TRUE) > max){
        max = max(weights, na.rm = TRUE)
      }
    }
    
    #cat(paste("Min:", min, "\n"))
    #cat(paste("Max:", max, "\n"))
    observation = 1
    for(net in nets){
      #cat(paste("Observation:", observation,"\n"))
      network_nodes = length(V(net))
      E(net)$weight = 1/(max-min)*(E(net)$weight - min) 
      network.copy = delete.edges(net, which(E(net)$weight >= interval))

      matrix_distance = shortest.paths(network.copy,weights=E(network.copy)$distance)

      clustering_coefficient[observation] = transitivity(network.copy)
      coef = 1/(network_nodes*(network_nodes - 1))
      link_density[observation] = coef*sum(get.adjacency(net))
      efficiency[observation] = coef*sum(1/matrix_distance[row(matrix_distance)!=col(matrix_distance)])
      caracteristic_path_length[observation] = coef*sum(matrix_distance[row(matrix_distance)!=col(matrix_distance)])
      
      observation = observation + 1 
    }
    
    
    
    result_training = as.data.frame(cbind(clustering_coefficient, link_density, efficiency, caracteristic_path_length))
    result_training = do.call(data.frame,lapply(result_training[,c(1,2,3,4)], function(x) replace(x, !is.finite(x),0)))
    #result_training = result_training[complete.cases(result_training),]

    #cat(paste("Length 3", length(labels),"\n"))
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
    
    #plot_measures = list(p1, p2, p3, p4)
    
    
    result_training[,"observation"] = NULL
    
    parencliticsnetworks = array()
    clustering_coefficient = array()
    link_density = array()
    efficiency = array()
    caracteristic_path_length = array()
    network_list2 = list()
    for(test in testingIndexes){
      ady = parencliticNetwork(trainingSet, testingSet, target = target, test, type)
      network_list[[length(network_list)+1]] = graph.adjacency(ady, weighted=T, mode = "undirected")
    }
    
    min = Inf
    max = -Inf
    nets2 = rep(network_list2, 1)
    for(network in network_list2){
      weights = E(network)$weight
      if(min(weights) < min){
        min = min(weights)
      }
      if(max(weights) > max){
        max = max(weights)
      }
    }
    
    for(network in nets2){
      network_nodes = length(V(network))
      
      E(network)$weight = 1/(max-min)*(E(network)$weight - min) 
      
      network.copy <- delete.edges(network, which(E(network)$weight >= interval))
      
      matrix_distance = shortest.paths(network.copy, weights=E(network.copy)$distance)
      
      
      clustering_coefficient[observation] = transitivity(network)
      coef = 1/(network_nodes*(network_nodes - 1))
      link_density[observation] = coef*sum(get.adjacency(network.copy))
      
      efficiency[observation] = coef*sum(1/matrix_distance[row(matrix_distance)!=col(matrix_distance)])
      caracteristic_path_length[observation] = coef*sum(matrix_distance[row(matrix_distance)!=col(matrix_distance)])
    }
    
    result_testing = as.data.frame(cbind(clustering_coefficient, link_density, efficiency, caracteristic_path_length))
    result_testing = do.call(data.frame,lapply(result_testing[,c(1,2,3,4)], function(x) replace(x, !is.finite(x),0)))
    #result_testing = result_testing[complete.cases(result_testing),]
    #cat("Testing indexes")
    print(testingIndexes)
    
    model = nnet(labels ~ ., data = result_training, size=50, maxit = 1000, MaxNWts = 10000)
    predicts = predict(model, result_testing, type = "class")
    #predict = predict(model, result_testing)
    
    #cat("Predicts\n")
    #print(predicts)
    predicts = unname(predicts)
    
    
    View(testingSet)
    
    results_percentage = sum(predicts == labels_testing)/length(predicts)
    cat(paste("%:", results_percentage, "\n"))
    
    sum(predicts == labels_testing)/length(predicts)
    
    #cat(paste("l1", length(predicts),"\n"))
    #cat(paste("l2", length(labels_testing),"\n"))
    
    results = as.data.frame(cbind(as.character(predicts), as.character(labels_testing)))
    results["classification"] = ifelse(predicts == labels_testing, "Good", "Bad")
    colnames(results) = c("predicted", "labels", "classification")
    rownames(results) = as.numeric(rownames(testingSet))
    
    cat(paste("Classification with type", type, "and interval", interval, "is", results_percentage, "\n"))
  
  }
  
  #return(list(results_percentage, results, plot_measures, labels_testing))
}