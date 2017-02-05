file = "C:\\Users\\JIEP1\\Documents\\GitHub\\TFM\\r-package\\data\\breast-cancer.csv"

data = read.csv(file, header = TRUE, sep = ",")

library("lme4")
library("nnet")

source("scripts/calculatePrediction.R")
types = c("linear", "exponential", "quadratic", "reciprocal")
for(type in types){
  cat(paste("--------------------------------------\nType:", type, "\n--------------------------------------\n\n"))
  calculatePrediction(data, "Class", 0.8, type)
}