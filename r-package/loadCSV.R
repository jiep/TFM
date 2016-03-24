loadCSV = function(path, sep = ",", header = TRUE){
  return (read.csv(path, header = header, sep = sep))
}