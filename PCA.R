#Perform PCA and select the most important features (to explain variance)

# trainset: training set
# testset: test set
# threshold: threshold of cumulated variance to explain
compute_pca <- function(trainset, testset, threshold){
  
  library("FactoMineR")
  library("factoextra")
  
  # Initialize file connection
  fileConn<-file("Logs/PCA/PCA.log")
  
  # Perform PCA on centered and scaled dataset
  pca.results <- prcomp(trainset, center = TRUE, scale = TRUE)
  eigenvalues <- get_eigenvalue(pca.results)
  
  # Save graph about variance of each principal component
  png("Plots/PCA/fviz_eig.png", width=1920, height = 780)
  print(fviz_eig(pca.results,
                 addlabels=TRUE,
                 ylim=c(0, 50), ncp=30),
        vp=grid::viewport(gp=grid::gpar(cex=1.8)))
  
  dev.off()

  # Save graph about feature correlation
  png("Plots/PCA/fviz_pca_var.png")
  print(fviz_pca_var(pca.results, col.var = "black"))
  dev.off()
  
  # Saving corrplot pca dataset
  png("Plots/PCA/corrplot_pca.png", width=1024, height=1024)
  corrplot(pca.results$rotation,
           number.cex=1,
           method="square",
           type="full",
           tl.cex=1,
           tl.col="black")
  dev.off()
  
  # Save coordinates of the features on the principal components
  var <- get_pca_var(pca.results)
  write.csv(var$coord,"Logs/PCA/Correlation.log")

  # Select how many principal components use based on the threshold
  x <- 1
  for (i in seq_len(ncol(pca.results$x))) {
    if(eigenvalues$cumulative.variance.percent[i] <= threshold){
      x <- i
    }
  }
  x <- x + 1
  
  # Save Eigenvalues and number of used principal components on a file
  writeLines(c("Eigenvalues:",
               toString(eigenvalues),
               "number of principal components used:",
               x),
             fileConn)
  
  # Created transformed trainset and testset (transformation on the PC spaces)
  transformed.dataset <- pca.results$x[,1:x]
  testset <- scale(testset,
                   pca.results$center,
                   pca.results$scale) %*% pca.results$rotation[,1:x]

  # Close file connection
  close(fileConn)
  
  return(list(as.data.frame(transformed.dataset),as.data.frame(testset)))
}
