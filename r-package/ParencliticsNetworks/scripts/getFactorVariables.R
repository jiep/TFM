getFactorVariables =  function(data){
  indexes = -1
  if(dim(data)[2]> 1){
    indexes = which(sapply(data, class) == "factor")[[1]]
  }
  return(indexes)
}