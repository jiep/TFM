source("setTargetVariable.R")

require('lattice')

drawRegressionLines = function(data, target){
  targetIndex = setTargetVariable(data, target)
  if(targetIndex != -1){
    target = data[,targetIndex]
    data[,targetIndex] = NULL
    cols = colnames(data)
    columnsLength = length(cols)
    linearPlots = matrix(list(), nrow = columnsLength, ncol = columnsLength)
    for(i in 1:columnsLength){
      for(j in 1:columnsLength){
        if(i < j){

          linearPlots[[i, j]] = xyplot(data[,i] ~ data[,j], data, groups = target, pch = 20, type=c("p","r"), lwd = 4, group = target, auto.key = list(columns = nlevels(target)), xlab = cols[i], ylab = cols[j])
        }
      }
    }
  }
  return(linearPlots)
}