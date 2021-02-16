# Perform 10-fold validation and return results

# dataset: dataset used to train the model (training set)
# tuningGrid: a data frame with possible tuning values
# modelType: name of the model used choose from the classification models of caret at:
#            http://topepo.github.io/caret/train-models-by-tag.html#Support_Vector_Machines
# seed: seed used for the computation
# mType: name of the model e.g. Naive_bayes, SVM...
# dType: type of dataset e.g. normalized, standardized...
train_model <- function(dataset, tuningGrid, modelType, seed, mType, dType){

  #compute 10-fold validation for the selected model
  #-----------------------

  # Set random seed
  set.seed(seed)
  
  # Initialize connection with file to write
  filename <- paste0("Logs/", mType, "/", mType, "_", dType, "_opt_info", ".log")
  fileConn<-file(filename)
  
  
  # Prepare training settings
  train_control <- trainControl(method="repeatedcv", #repeated cross validation to decrease variance
                                number=10, # 10 folds
                                repeats=5, # repeat 5 times
                                classProbs=TRUE # compute class probabilities for the classification model
                                )
  
  # Ten-fold and parameters tuning
    model <- train(diagnosis ~.,
                   data=dataset,
                   method=modelType,
                   tuneGrid=tuningGrid,
                   trControl=train_control)
  
  # Save a report on a file
  writeLines(c("method:",
               model$method,
               "model:",
               model$modelInfo$label,
               "best tuning parameters:",
               toString(model$modelInfo$parameters$parameter),
               toString(model$bestTune),
               "optimized metric:",
               model$metric,
               "Time to perform computation (in seconds):",
               model$times$everything[3]), fileConn)
  
  # Close connection with file
  close(fileConn)
  
  # Repeat predictions on the train set using the best fit to save the relative confusion matrix
  predictions <- predict(model, dataset[,! names(dataset) %in% "diagnosis"])
  result <- confusionMatrix(predictions,
                            factor(dataset[, "diagnosis"]))$table
  
  # Save the confusion matrix
  write.csv(result,paste0("Logs/", mType, "/", mType, "_", dType, "_finalModel_confusion_matrix_train.log"))
  
  # In case of neural network save the info about the network (weights, layers and neurons)
  if(mType=="Neural_Network"){
    filename <- paste0("Logs/", mType, "/", mType, "_", dType, "_net_info", ".log")
    fileConn<-file(filename)
    writeLines(c(model[["finalModel"]][["snnsObject"]]@variables[["serialization"]],"neural net"),fileConn)
    close(fileConn)
  }
  
  return(model)
}

# Support function to perform train_model with the 3 chosen models
train_models <- function(set, seed, dType) {
  
  #library(doParallel)
  #cores <- detectCores()
  #registerDoParallel(cores = cores)
  #cluster <- makeCluster(cores)

  #import tuning parameters configuration
  source("configuration.R")
  
  # Perform Naive Bayes
  Bayes.model <- train_model(set,
                           bayes_tuning_grid,
                           'naive_bayes',
                           seed,
                            "Naive_Bayes",
                             dType)
  
  # Perform Support Vector Machine
  final.tuning.grid = svm_tuning_grid
  if(dType=="PCA" | dType =="Corr"){
    final.tuning.grid = svm_reducted_tuning_grid
  }
  SVM.model <- train_model(set,
                           final.tuning.grid,
                           "svmPoly",
                           seed,
                           "SVM",
                           dType)

  # Perform Neural Network
  NN.model <- train_model(set, nn_tuning_grid, 'mlpML', seed,
                            "Neural_Network",
                             dType)

  #stopCluster(cluster)

  return(list(Bayes.model, SVM.model, NN.model))
}