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
          
          linearPlots[[i, j]] = xyplot(
            data[,i]  ~ data[,j],
            groups = target,
            data = data,
            panel = function(x, y, ...) {
              panel.superpose(x, y, ...,
                              panel.groups = function(x,y, col, col.symbol, ...) {
                                panel.xyplot(x, y, col=col.symbol, ...)
                                panel.abline(lm(y~x), col.line=col.symbol)
                              }
              )
            },
            auto.key = list(columns = nlevels(target)),
            xlab = cols[i], 
            ylab = cols[j]
          )
        }
      }
    }
  }
  return(linearPlots)
}