require('lme4')
require("igraph")


calculateROC = function(){
  model1 = calculatePrediction(cancer, "Class", 0.8, "linear")
  pred1  = prediction(model1[[4]],model1[[5]])
  perf1  = performance(pred1,"tpr","fpr")
  
  model2 = calculatePrediction(cancer, "Class", 0.8, "exponential")
  pred2  = prediction(model2[[4]],model2[[5]])
  perf2  = performance(pred2,"tpr","fpr")
  
  model3 = calculatePrediction(cancer, "Class", 0.8, "quadratic")
  pred3  = prediction(model3[[4]],model3[[5]])
  perf3  = performance(pred3,"tpr","fpr")
  
  model7 = calculatePrediction(cancer, "Class", 0.8, "reciprocal")
  pred7  = prediction(model7[[4]],model7[[5]])
  perf7  = performance(pred7,"tpr","fpr")
  
  model4 = calculatePredictionML(cancer, "Class", 0.8)
  pred4  = prediction(model4[[4]][,2],model4[[7]])
  perf4  = performance(pred4,"tpr","fpr")
  
  pred5  = prediction(attr(model4[[5]], "probabilities")[,2],model4[[7]])
  perf5  = performance(pred5,"tpr","fpr")
  
  pred6  = prediction(model4[[6]],model4[[7]])
  perf6  = performance(pred6,"tpr","fpr")
  
  plot(perf1, col = 1)
  plot(perf2, add=TRUE, col = 2)
  plot(perf3, add=TRUE, col = 3)
  plot(perf4, add=TRUE, col = 4)
  plot(perf5, add=TRUE, col = 5)
  plot(perf6, add=TRUE, col = 6)
  plot(perf7, add=TRUE, col = 7)
  legend("bottomright", legend = c("Redes parenclíticas (lineal)", "Redes parenclíticas (exponencial)", "Redes parenclíticas (cuadrático)", "Redes parenclíticas (recíproco)", "Árboles de decisión", "SVM", "Redes neuronales"), col = c(1:7), lty = 1, cex=0.5)
  
  abline(a=0, b=1)
}