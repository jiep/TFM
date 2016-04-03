# Extraído de 
# http://www.r-bloggers.com/summary-function-that-is-compatible-with-xtable/

summaryfunction = function (x){
  if( is.numeric(x)!=TRUE) {stop("Supplied X is not numeric")}
  mysummary = data.frame(
    "Min." =as.numeric( min(x)),
    "1st Qu." = quantile(x)[2],
    "Median" = median(x),
    "Mean" = mean(x),
    "3rd Qu." = quantile(x)[4],
    "Max." = max(x),
    row.names=""
    
  )
  names(mysummary) = c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")
  return( mysummary )
}