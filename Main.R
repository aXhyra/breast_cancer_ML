list.of.packages <- c("ggplot2", "dplyr","caret","e1071","mlbench","corrplot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(ggplot2)
library(dplyr)
library(caret)
library(e1071)
library(mlbench)
library(corrplot)

library(tidyverse)
library(ROCR)

source("Models.R")
source("Metrics.R")
source("PCA.R")
source("preprocessing.R")

save_graphics_comparisons  <- function(models, dType){
  
  models.comparator = resamples(list(Bayes=models[[1]], SVM = models[[2]], NN = models[[3]]))
  
  png(paste0("Plots/comparisons/dotplot_",dType,".png"),width=1350,height=900)
  print(dotplot(models.comparator))
  dev.off()
  
  png(paste0("Plots/comparisons/bwplot_",dType,".png"),width=1350,height=900)
  print(bwplot(models.comparator, layout = c(2, 1)))
  dev.off()
  
  png(paste0("Plots/comparisons/splom_",dType,".png"),width=1350,height=900)
  print(splom(models.comparator))
  dev.off()
}


#Load dataset
#-----------------------

data <- read.csv("Breast_cancer/data.csv", stringsAsFactors=F)
data <- data[,2:32]
preprocessing(data)

#-----------------------


#Create training and test partition
#-----------------------

ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.8, 0.2))
trainset <- data[ind == 1,]
testset <- data[ind == 2,]

cols.min <- apply(trainset[,2:31],2,min)
cols.max <- apply(trainset[,2:31],2,max)

trainset.norm <- data.frame(trainset[,1], scale(trainset[,2:31], cols.min, (cols.max - cols.min)))
testset.norm <- data.frame(testset[,1], scale(testset[,2:31], cols.min, (cols.max - cols.min)))

cols.mean <- apply(trainset[,2:31],2, mean)
cols.std <- apply(trainset[,2:31],2, sd)

trainset.std <- data.frame(trainset[,1], scale(trainset[,2:31], cols.mean, cols.std))
testset.std <- data.frame(testset[,1], scale(testset[,2:31], cols.mean, cols.std))

colnames(trainset.norm)[1] <- "diagnosis"
colnames(testset.norm)[1] <- "diagnosis"
colnames(trainset.std)[1] <- "diagnosis"
colnames(testset.std)[1] <- "diagnosis"

# Compute PCA dataset
#-----------------------

computed.pca <- compute_pca(trainset[, 2:31], testset[, 2:31], 99)

trainset.pca <- data.frame(trainset[1], computed.pca[1])
testset.pca <- data.frame(testset[1], computed.pca[2])

#-----------------------

models.norm <- train_models(trainset.norm, Sys.time(), "Normalized")

analyze_results(models.norm, testset.norm, "Normalized")

save_graphics_comparisons(models.norm, "Normalized")


models.std <- train_models(trainset.std, Sys.time(), "Standardized")

analyze_results(models.std, testset.std, "Standardized")

save_graphics_comparisons(models.std, "Standardized")


models.pca <- train_models(trainset.pca, Sys.time(), "PCA")

analyze_results(models.pca, testset.pca, "PCA")

save_graphics_comparisons(models.pca, "PCA")