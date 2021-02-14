# Perform 10-fold validation and return results

# dataset: dataset used to train the model (training set)
# tuningGrid: a data frame with possible tuning values
# modelType: name of the model used choose from the classification models of caret at:
#            http://topepo.github.io/caret/train-models-by-tag.html#Support_Vector_Machines
# seed: seed used for the computation
train_model <- function(dataset, tuningGrid, modelType, seed, mType, dType){

  #compute 10-fold validation for the selected model
  #-----------------------

  set.seed(seed)
  filename <- paste0("Logs/", mType, "/", modelType, "_", dType, ".log")
  sink(filename)
  
  train_control <- trainControl(method="repeatedcv", #repeated cross validation to decrease variance
                                number=10, # 10 folds
                                repeats=5, # repeat 5 times
                                classProbs=TRUE # compute class probabilities for the classification model
                                )
  
  if (is.null(tuningGrid)) { # if tuning parameters are specified use them
    
    model <- train(diagnosis ~.,
                   data=dataset,
                   method=modelType,
                   tuneGrid=tuningGrid,
                   trControl=train_control)

  } else { # default tuning otherwise
    
    model <- train(diagnosis ~.,
                   data=dataset,
                   method=modelType,
                   trControl=train_control)
  }

  cat(toString(model))
  
  #-----------------------
  sink()
  return(model)
  
  #-----------------------
}

train_models <- function(set, seed, dType) {
  # Initialize processing cluster
  #-----------------------

  library(doParallel)
  cores <- detectCores()
  registerDoParallel(cores = cores)
  cluster <- makeCluster(cores)

  source("configuration.R")

  Bayes.model <- train_model(set,
                           bayes_tuning_grid,
                           'naive_bayes',
                           seed,
                            "Naive_Bayes",
                             dType
                  )

  SVM.model <- train_model(set,
                         svm_tuning_grid,
                         "svmPoly",
                         seed,
                            "SVM",
                             dType)

  NN.model <- train_model(set, nn_tuning_grid, 'mlpML', seed,
                            "Neural_Network",
                             dType)

  #Terminate parallel computing and return outputs
  #-----------------------

  stopCluster(cluster)

  return(list(Bayes.model, SVM.model, NN.model))
}