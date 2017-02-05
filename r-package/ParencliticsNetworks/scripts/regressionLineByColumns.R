source("setTargetVariable.R")

regressionLineByColumns = function(data, target, i, j){
  targetIndex = setTargetVariable(data, target)
  linearModel = list()
  if(targetIndex != -1){
    target = data[,targetIndex]
    data[,targetIndex] = NULL
    cols = colnames(data)
    columnsLength = length(cols)
    if(i < j){
      formula = paste(i, "~", j)
      linearModels = by(data, target, function(x){ lm(formula, data=x)})
    }
  }
  return(linearModels)
}