#Perform PCA and select the most important features (to explain variance)

compute_pca <- function(trainset, testset, threshold){
  
  library("FactoMineR")
  library("factoextra")
  
  fileConn<-file("Logs/PCA/PCA.log")

  res.pca <- prcomp(trainset, center = TRUE, scale = TRUE)
  eig.val <- get_eigenvalue(res.pca)

  png("Plots/PCA/fviz_eig.png", width=1920, height = 780)
  print(fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50), ncp = 30), vp=grid::viewport(gp=grid::gpar(cex=1.8)))
  dev.off()

  png("Plots/PCA/fviz_pca_var.png")
  print(fviz_pca_var(res.pca, col.var = "black"))
  dev.off()
  
  var <- get_pca_var(res.pca)

  x <- 1

  for (i in seq_len(ncol(res.pca$x))) {
    if(eig.val$cumulative.variance.percent[i] <= threshold){
      x <- i
    }
  }
  print(x)
  
  write.csv(var$coord,"Logs/PCA/Correlation.log")
  
  writeLines(c("Eigenvalues:",
               toString(eig.val),
               "number of principal components used:",
               x),
             fileConn)
  
  dataset_transformed <- res.pca$x[,1:x]
  
  testset <- scale(testset, res.pca$center, res.pca$scale) %*% res.pca$rotation[,1:x]

  close(fileConn)
  return(list(as.data.frame(dataset_transformed),as.data.frame(testset)))
}
