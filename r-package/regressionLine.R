source("setTargetVariable.R")

regressionLine = function(data, target){
  targetIndex = setTargetVariable(data, target)
  if(targetIndex != -1){
    target = data[,targetIndex]
    data[,targetIndex] = NULL
    cols = colnames(data)
    columnsLength = length(cols)
    linearModels = matrix(list(), nrow = columnsLength, ncol = columnsLength)
    for(i in 1:columnsLength){
      for(j in 1:columnsLength){
        if(i < j){
          formula = paste(colnames(data)[i], "~", colnames(data)[j])
          linearModels[[i, j]] = by(data, target, function(x){ lm(formula, data=x)})
        }
      }
    }
  }
  return(linearModels)
}