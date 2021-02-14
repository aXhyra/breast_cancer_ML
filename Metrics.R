# Compute accuracy, precision, recall, ROC and AUC for the given model

# model: caret model (best fit of the 10-fold)
# testset: test set to perform metrics scores
analyze_result <- function(model, testset, modeltype, testset_type){

  predictions <- predict(model, testset[,! names(testset) %in% "diagnosis"])

  table(predictions, testset[, "diagnosis"])

  result <- confusionMatrix(predictions,
                            factor(testset[, "diagnosis"]))

  result2 <- confusionMatrix(predictions,
                             factor(testset[, "diagnosis"]),
                             mode = "prec_recall")

  print(result)
  print(result2)

  pred.prob <- predict(model,
                       testset[, ! names(testset) %in% "diagnosis"],
                       type = "prob")

  pred.to.roc <- pred.prob[, 2]
  pred.rocr <- prediction(pred.to.roc, factor(testset$diagnosis))
  perf.rocr <- performance(pred.rocr, measure = "auc", x.measure = "cutoff")
  perf.tpr.rocr <- performance(pred.rocr, "tpr", "fpr")
  filename <- paste0("Plots/", modeltype, "/auc_", modeltype, "_", testset_type, ".png")
  png(filename)
  plot(perf.tpr.rocr, colorize=T,main=paste(modeltype, " AUC:",(perf.rocr@y.values)))
  abline(a=0, b=1)
  dev.off()

  print(opt.cut(perf.tpr.rocr, pred.rocr))
}

opt.cut <- function(perf, pred) {
  cut.ind <- mapply(FUN=function(x, y, p) {
    d <- (x - 0)^2 + (y-1)^2
    ind <- which(d == min(d))
    c(sensitivity = y[[ind]],
      specificity = 1-x[[ind]],
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

analyze_results <- function(models, testset, testset_type) {
  analyze_result(models[[1]], testset, 'Naive_Bayes', testset_type)
  analyze_result(models[[2]], testset, 'SVM', testset_type)
  analyze_result(models[[3]], testset, 'Neural_Network', testset_type)
}