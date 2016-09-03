file = "C:\\Users\\JIEP1\\Documents\\GitHub\\TFM\\r-package\\data\\glass.csv"
data = read.csv(file, header = TRUE, sep=",")
setwd("C:\\Users\\JIEP1\\Documents\\GitHub\\TFM\\r-package\\ParencliticsNetworks")
source("scripts/calculatePrediction.R")
output = calculatePrediction(data, "Type", 0.9, "linear")
write.csv(output, file="salida.csv")