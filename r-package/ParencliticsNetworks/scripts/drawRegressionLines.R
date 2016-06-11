source("scripts/setTargetVariable.R")

require('lattice')

drawRegressionLines = function(data, target, i, j, type="linear"){
  targetIndex = setTargetVariable(data, target)
  if(targetIndex != -1){
    target = data[,targetIndex]
    data[,targetIndex] = NULL
    cols = colnames(data)
    columnsLength = length(cols)
    linearPlots = matrix(list(), nrow = columnsLength, ncol = columnsLength)
    #for(i in 1:columnsLength){
      #for(j in 1:columnsLength){
          linearPlots[[i, j]] = xyplot(
            data[,i]  ~ data[,j],
            groups = target,
            data = data,
            panel = function(x, y, ...) {
              panel.superpose(x, y, ...,
                panel.groups = function(x,y, col, col.symbol, ...) {
                  panel.xyplot(x, y, col=col.symbol, ...)
                    fit = NULL
                    if(type == "exponential"){
                      cat("exponencial")
                      fit = lm(log(y)~x)
                      panel.curve(exp(predict(fit, newdata = list(x = x))), col.line=col.symbol)
                    }else if(type == "quadratic"){
                      cat("quadratic")
                      fit = lm(sqrt(y)~x)
                      panel.curve((predict(fit, newdata = list(x = x)))^2, col.line=col.symbol)
                    }else if(type=="reciprocal"){
                      cat("reciprocal")
                      fit = lm(1/y~x)
                      panel.curve(1/(predict(fit, newdata = list(x = x))), col.line=col.symbol)
                    }else{
                      cat("lineal")
                      fit = lm(y~x)
                      panel.lines(sort(x), fitted(fit)[order(x)], col.line=col.symbol)
                    }
                
                }
              )
            },
            auto.key = list(columns = nlevels(target)),
            xlab = cols[i], 
            ylab = cols[j]
          )
      }
    #}
  #}
  return(linearPlots)
}
