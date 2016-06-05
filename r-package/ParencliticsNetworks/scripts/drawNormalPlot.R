source("scripts/setTargetVariable.R")

require('lme4')
require("ggplot2")


drawNormalPlot = function(data, target, v1, v2, observation){
  targetIndex = setTargetVariable(data, target)
  if(targetIndex != -1){
    formula = as.formula(paste(v2, "~", paste(v1, "|", paste(colnames(data)[targetIndex], collapse = "+"),sep = "")))
    classes = unique(data[,targetIndex])
    X = data[observation,]
    data = data[setdiff(1:dim(data)[1], observation),]
    fits = lmList(formula = formula, data = data)
    
    mean = array()
    sd = array()
    n = 10000
    norms = matrix(,ncol = length(classes), nrow = n)
    i = 1
    for(fit in fits){
      mean[i] = fit$coefficients[[2]]*X[1,v1] + fit$coefficients[[1]]
      sd[i] = sd(fit$residuals)
      norms[,i] = rnorm(n, mean[i], sd[i])
      i = i + 1
    }
    
    colnames(norms) = classes
    df = as.data.frame(norms)
    
    dfs = stack(df)
    
    plot = ggplot(dfs, aes(x=values)) + geom_density(aes(y = ..scaled.., group=ind, colour=ind, fill=ind), alpha=0.3)
    plot = plot + labs(x = v2) 
    plot = plot + labs(y  = "Density")
    plot = plot + scale_fill_discrete(name = target)
    plot = plot + geom_vline(xintercept = X[1,v2]) 

    
    return(plot)
    
  }
}