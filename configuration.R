#-----------------------
bayes_tuning_grid <- NULL
#-----------------------
svm_kern_degree <- c(1, 2, 3, 4, 5) #si testa kernel con grado del polinomio 1,2,3,4 e 5
svm_scale <- 1/30
svm_C <- c(0.25, 0.50, 0.75, 1, 2)
svm_tuning_grid <- expand.grid(degree=svm_kern_degree, scale=svm_scale, C=svm_C)
#-----------------------
nn_tuning_grid <- expand.grid(layer1=1:10, layer2=1:10, layer3=1:10)
#-----------------------