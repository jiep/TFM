getFactorVariables =  function(data){
  indexes = which(sapply(iris, class) == "factor")[[1]]
  return(colnames(data)[indexes])
}