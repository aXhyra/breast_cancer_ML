# RNG seed
#-----------------------
seed <- 42

# Naive Bayes tuning parameters
#-----------------------

bayes_tuning_grid <- NULL


# SVM tuning parameters
#-----------------------

# si testa kernel con grado del polinomio 1,2,3,4 e 5
svm_kern_degree <- c(1, 2, 3, 4, 5)
svm_scale <- 1/30
svm_reducted_scale = 1/16
svm_C <- c(0.25, 0.50, 0.75, 1, 1.25, 1.50, 1.75, 2)
svm_tuning_grid <- expand.grid(degree=svm_kern_degree,
                               scale=svm_scale,
                               C=svm_C)
svm_reducted_tuning_grid <- expand.grid(degree=svm_kern_degree,
                                        scale=svm_reducted_scale,
                                        C=svm_C)


# Neural network tuning parameters
#-----------------------

nn_tuning_grid <- expand.grid(layer1=3:10, layer2=0:5, layer3=0)
