# Compute accuracy, precision, recall, ROC and AUC for the given model

# model: caret model (best fit of the 10-fold)
# testset: test set to perform metrics scores
# modeltype: name of the used model e.g. Naive_Bayes, SVm...
# testset_type: type of dataset e.g. normalized, standardized...
analyze_result <- function(model, testset, modeltype, testset_type){
  
  # Prepare file to write
  filename <- paste0("Logs/",
                     modeltype,
                     "/",
                     modeltype,
                     "_",
                     testset_type,
                     "_results",
                     ".log")
  fileConn <- file(filename)
  
  # Make predictions for the test set
  predictions <- predict(model, testset[,! names(testset) %in% "diagnosis"])
  
  # Create confusion matrix
  result <- confusionMatrix(predictions,
                            factor(testset[, "diagnosis"]))

  # Save confusion matrix in a file
  write.csv(result$table,paste0("Logs/",
                                modeltype,
                                "/",
                                modeltype,
                                "_",
                                testset_type,
                                "_finalModel_confusion_matrix_test.log"))
  
  # Save metrics info about predictions in a file
  writeLines(c("Accuracy:",
               result$overall[1],
               "Sensitivity:",
               result$byClass[1],
               "Specificity:",
               result$byClass[2],
               "Precision:",
               result$byClass[5],
               "Recall:",
               result$byClass[6],
               "F1:",
               result$byClass[7]), fileConn)

  # Get probabilistic predictions for the test set
  probabilistic.predictions <- predict(model,
                       testset[, ! names(testset) %in% "diagnosis"],
                       type = "prob")

  # Perform ROC
  single.class.probabilities <- probabilistic.predictions[, 2]
  prediction.result <- prediction(single.class.probabilities,
                                  factor(testset$diagnosis))
  
  rocr.performance <- performance(prediction.result,
                                  measure="auc",
                                  x.measure="cutoff")
  
  perf.tpr.rocr <- performance(prediction.result, "tpr", "fpr")
  
  # Save ROC
  png(paste0("Plots/",
             modeltype,
             "/auc_",
             modeltype,
             "_",
             testset_type,
             ".png"))

  plot(perf.tpr.rocr,
       colorize=T,
       main=paste(modeltype, " AUC:", (rocr.performance@y.values)))
  abline(a=0, b=1)
  dev.off()
  
  # Close file connection
  close(fileConn)
  return(perf.tpr.rocr)
}

# Support function to execute analysis on the 3 chosen models
analyze_results <- function(models, testset, testset_type) {
  
  res.Naive_Bayes <- analyze_result(models[[1]],
                                    testset,
                                    'Naive_Bayes',
                                    testset_type)
  
  res.SVM <- analyze_result(models[[2]],
                            testset,
                            'SVM',
                            testset_type)
  
  res.Neural_Network <- analyze_result(models[[3]],
                                       testset,
                                       'Neural_Network',
                                       testset_type)
  
  png(paste0("Plots/Comparisons/ROC_models_", testset_type, ".png"))
  
  plot(res.Naive_Bayes,
       col="red",
       main = "ROC comparison between models")
  plot(res.SVM, col="blue", add=TRUE)
  plot(res.Neural_Network, col="green", add=TRUE)
  
  legend(x = "bottomright",
         legend = c("Naive Bayes",
                    "Support Vector Machine",
                    "Neural Network"),
         fill = c("red","blue","green"))
  
  abline(a=0, b=1)
  dev.off()
}