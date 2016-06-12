require("rpart")
require("e1071")
require("nnet")

calculatePredictionML = function(data, target, percentage){
  n = floor(nrow(data)*as.numeric(percentage)) + 1
  trainingIndexes = sort(sample(1:dim(data)[1], size = n))
  testingIndexes = setdiff(1:dim(data)[1], trainingIndexes)
  trainingSet = data[trainingIndexes,]
  testingSet  = data[-trainingIndexes,]
  labels_testing = data[testingIndexes,target]
  
  formula = as.formula(paste(target, "~", paste(".",sep = "")))
  
  model_tree = rpart(formula, data = trainingSet)
  predicts_tree = unname(predict(model_tree, testingSet, type="class"))
  results_percentage_tree = sum(predicts_tree == labels_testing)/length(predicts_tree)
  results_tree = as.data.frame(cbind(as.character(predicts_tree), as.character(labels_testing)))
  results_tree["classification"] = ifelse(predicts_tree == labels_testing, "Good", "Bad")
  colnames(results_tree) = c("predicted", "labels", "classification")
  rownames(results_tree) = as.numeric(rownames(testingSet))
  
  model_svm = svm(formula, data = trainingSet)
  predicts_svm = unname(predict(model_svm, testingSet))
  results_percentage_svm = sum(predicts_svm == labels_testing)/length(predicts_svm)
  results_svm = as.data.frame(cbind(as.character(predicts_svm), as.character(labels_testing)))
  results_svm["classification"] = ifelse(predicts_svm == labels_testing, "Good", "Bad")
  colnames(results_svm) = c("predicted", "labels", "classification")
  rownames(results_svm) = as.numeric(rownames(testingSet))
  
  model_ann = nnet(formula, size = 10, data = trainingSet,trace = FALSE)
  predicts_ann = unname(predict(model_ann, testingSet, type = "class"))
  results_percentage_ann = sum(predicts_ann == labels_testing)/length(predicts_ann)
  results_ann = as.data.frame(cbind(as.character(predicts_ann), as.character(labels_testing)))
  results_ann["classification"] = ifelse(predicts_ann == labels_testing, "Good", "Bad")
  colnames(results_ann) = c("predicted", "labels", "classification")
  rownames(results_ann) = as.numeric(rownames(testingSet))
  
  
  return(list(results_tree, results_svm, results_ann))
  
}