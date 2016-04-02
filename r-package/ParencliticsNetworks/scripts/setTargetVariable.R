setTargetVariable = function(data, target){
  targetVariable = which(colnames(data) == target)
  if(length(targetVariable) == 0){
    return(-1)
  }
  return(targetVariable)
}