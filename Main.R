#This file execute all the project scripts to perform the whole analysis

#install packages and load libraries and resources
#-----------------------

list.of.packages <- c("ggplot2",
                      "dplyr",
                      "caret",
                      "e1071",
                      "mlbench",
                      "corrplot",
                      "doParallel",
                      "naivebayes",
                      "ROCR",
                      "kernlab",
                      "tidyverse",
                      "factoextra",
                      "FactoMineR",
                      "RSNNS")

# Try to install CRAN ggrepel
if (!(require("ggrepel"))) install.packages("ggrepel")

# Install github version if CRAN fails
# https://github.com/slowkow/ggrepel/issues/184
if (!(require("ggrepel"))) remotes::install_github("slowkow/ggrepel") 

new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, Ncpus = 4)

library(ggplot2)
library(dplyr)
library(caret)
library(e1071)
library(mlbench)
library(corrplot)
library(tidyverse)
library(ROCR)

source("Data_exploration.R")
source("PCA.R")
source("Models.R")
source("Metrics.R")
source("Configuration.R")


# Support function to save comparisons between models
#-----------------------

# models: result of train_models method
# dType: type of dataset e.g. normalized, standardized...
save_models_comparisons  <- function(models, dType){
  
  formatted.data = resamples(list(Bayes=models[[1]],
                                  SVM=models[[2]],
                                  NN=models[[3]]))
  
  png(paste0("Plots/comparisons/dotplot_",dType,".png"),width=1350,height=900)
  print(dotplot(formatted.data,
                metric="Accuracy",
                main="Accuracy comparison",
                scales=list(cex=1.5)))
  dev.off()
  
  png(paste0("Plots/comparisons/bwplot_",dType,".png"),width=1350,height=900)
  print(bwplot(formatted.data,
               metric="Accuracy",
               main="Accuracy comparison",
               cex.axis=5,
               scales=list(cex=1.5)))
  dev.off()
  
  png(paste0("Plots/comparisons/splom_",dType,".png"),width=1350,height=900)
  print(splom(formatted.data,
              scales=list(cex=1.5)))
  dev.off()
}


# Load dataset
#-----------------------

data <- read.csv("Breast_cancer/data.csv", stringsAsFactors=F)
data <- data[,2:32]


# Perform data exploration
#-----------------------

corr.var <- data_exploration(data) # Names of highly correlated features


# Create training and test partitions
#-----------------------

set.seed(seed)
# Get partitions indexes
partition.indexes <- sample(2, nrow(data), replace=TRUE, prob=c(0.8, 0.2))
trainset <- data[partition.indexes == 1,] # Create train partition
testset <- data[partition.indexes == 2,] # Create test partition

cols.min <- apply(trainset[,2:31],2,min) # Get min value for each feature
cols.max <- apply(trainset[,2:31],2,max) # Get max value for each feature

trainset.norm <- data.frame(trainset[,1], # Create normalized trainset partition
                            scale(trainset[,2:31],
                                  cols.min,
                                  (cols.max - cols.min)
                            )
)

testset.norm <- data.frame(testset[,1], # Create normalized test partition
                           scale(testset[,2:31],
                                 cols.min,
                                 (cols.max - cols.min)
                           )
)

# Get mean value for each feature
cols.mean <- apply(trainset[,2:31], 2, mean)
# Get standard deviation for each feature distribution
cols.std <- apply(trainset[,2:31], 2, sd)

# Create standardized train partition
trainset.std <- data.frame(trainset[,1],
                           scale(trainset[,2:31],
                                 cols.mean,
                                 cols.std)
)

# Create standardized test partition
testset.std <- data.frame(testset[,1],
                          scale(testset[,2:31],
                                cols.mean,
                                cols.std)
)

# Select not highly correlated features from trainset.std and testset.std
# (feature reduction)
trainset.corr <- data.frame(trainset[,1],
                            trainset.std[,2:31]
                            [,!(colnames(trainset.std[,2:31]) %in% corr.var)] 
)

testset.corr <- data.frame(testset[,1],
                           testset.std[,2:31]
                           [,!(colnames(testset.std[,2:31]) %in% corr.var)]
)

# Rename target column of the created sets
colnames(trainset.norm)[1] <- "diagnosis"
colnames(testset.norm)[1] <- "diagnosis"
colnames(trainset.std)[1] <- "diagnosis"
colnames(testset.std)[1] <- "diagnosis"
colnames(trainset.corr)[1] <- "diagnosis"
colnames(testset.corr)[1] <- "diagnosis"

# Compute PCA dataset
#-----------------------

computed.pca <- compute_pca(trainset[, 2:31], testset[, 2:31], 99)

trainset.pca <- data.frame(trainset[,1], computed.pca[1])
testset.pca <- data.frame(testset[,1], computed.pca[2])

# Rename target column of the created sets
colnames(trainset.pca)[1] <- "diagnosis"
colnames(testset.pca)[1] <- "diagnosis"

# Perform training, analyze data and compare data for each set
#-----------------------

models.norm <- train_models(trainset.norm, seed, "Normalized")
analyze_results(models.norm, testset.norm, "Normalized")
save_models_comparisons(models.norm, "Normalized")


models.std <- train_models(trainset.std, seed, "Standardized")
analyze_results(models.std, testset.std, "Standardized")
save_models_comparisons(models.std, "Standardized")


models.pca <- train_models(trainset.pca, seed, "PCA")
analyze_results(models.pca, testset.pca, "PCA")
save_models_comparisons(models.pca, "PCA")


models.corr <- train_models(trainset.corr, seed, "Corr")
analyze_results(models.corr, testset.corr, "Corr")
save_models_comparisons(models.corr, "Corr")