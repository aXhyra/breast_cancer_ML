#Perform PCA and select the most important features (to explain variance)

compute_pca <- function(trainset, testset, threshold){
  
  library("FactoMineR")
  library("factoextra")
  res.pca <- prcomp(trainset, center = TRUE, scale = TRUE)
  eig.val <- get_eigenvalue(res.pca)

  png("Plots/PCA/fviz_eig.png")
  fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
  dev.off()

  png("Plots/PCA/fviz_pca_var.png")
  print(fviz_pca_var(res.pca, col.var = "black"))
  dev.off()
  
  var <- get_pca_var(res.pca)
  head(var$coord, 4)
  
  print(eig.val)
  x <- 1

  for (i in seq_len(ncol(res.pca$x))) {
    if(eig.val$cumulative.variance.percent[i] <= threshold){
      x <- i
    }
  }
  print(x)
  
  dataset_transformed <- res.pca$x[,1:x]
  
  testset <- scale(testset, res.pca$center, res.pca$scale) %*% res.pca$rotation[,1:x]
  
  return(list(as.data.frame(dataset_transformed),as.data.frame(testset)))
}
