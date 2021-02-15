############################################################
# Install and load required packages


############################################################
data_exploration <- function(dataset) {
  ############################################################
  # Class frequencies
  # Frequency table
  diagnosis.table <- table(dataset$diagnosis)
  colors <- terrain.colors(2)
  # Pie chart
  diagnosis.prop.table <- prop.table(diagnosis.table)*100
  diagnosis.prop.df <- as.data.frame(diagnosis.prop.table)
  pielabels <- sprintf("%s - %3.1f%s", diagnosis.prop.df[,1], diagnosis.prop.table, "%")

  png("Plots/Data_exploration/nclass_pie.png",width=1350,height=900)
  pie(diagnosis.prop.table,
      labels=pielabels,
      clockwise=TRUE,
      col=colors,
      border="gainsboro",
      radius=0.8,
      cex=1.8,
      cex.main=1.8,
      main="Diagnosis proportion")
  legend(1, .8, legend=diagnosis.prop.df[,1], cex = 1.6, fill = colors)
  dev.off()

  ############################################################
  # Density plots
  tmpdataset <- dataset[,2:31]
  scales <- list(x=list(relation="free"), y=list(relation="free"), cex=1)
  # First 15 features
  png("Plots/Data_exploration/density_plots_pt1.png",width=958,height=1008)
  featurePlot(x=tmpdataset[,1:15],
              y=dataset[,1],
              plot="density",
              scales=scales,
              layout=c(3,5),
              auto.key=list(columns=2),
              pch="|")
  dev.off()
  # Last 15 features
  png("Plots/Data_exploration/density_plots_pt2.png",width=958,height=1008)
  featurePlot(x=tmpdataset[,16:30],
              y=dataset[,1],
              plot="density",
              scales=scales,
              layout=c(3,5),
              auto.key=list(columns=2),
              pch="|")
  dev.off()

  ############################################################
  # Density plots of single variables
  png("Plots/Data_exploration/density_plots_tot_pt1.png",width=958,height=1008)
  par(mar=c(3,3,3,3))
  par(mfrow=c(5,3))
  for(i in 1:15) {
    plot(density(tmpdataset[,i]),ylab="",main="",cex.axis=1.3)
    title(colnames(tmpdataset[i]),line=0.2)
  }
  dev.off()

  png("Plots/Data_exploration/density_plots_tot_pt2.png",width=958,height=1008)
  par(mar=c(3,3,3,3))
  par(mfrow=c(5,3))
  for(i in 16:30) {
    plot(density(tmpdataset[,i]),ylab="",main="",cex.axis=1.3)
    title(colnames(tmpdataset[i]),line=0.2)
  }
  dev.off()

  ############################################################
  # Boxplots of the first 15 features
  png("Plots/Data_exploration/boxplot1.png",width=958,height=1008)
  par(mar=c(3,3,3,3))
  par(mfrow=c(5,3))
  for(i in 1:15) {
    boxplot(tmpdataset[,i],ylab="",main="",cex.axis=1.3)
    title(colnames(tmpdataset[i]),line=0.2)
  }
  dev.off()

  # Boxplots of the last 15 features
  png("Plots/Data_exploration/boxplot2.png",width=958,height=1008)
  par(mar=c(3,3,3,3))
  par(mfrow=c(5,3))
  for(i in 16:30) {
    boxplot(tmpdataset[,i],ylab="",main="",cex.axis=1.3)
    title(colnames(tmpdataset[i]),line=0.2)
  }
  dev.off()

  ############################################################
  # Shapiro-wilk test for normality
  sw <- c()
  for(i in 1:30) {
    test <- shapiro.test(tmpdataset[,i])
    sw <- rbind(sw,c(round(test$statistic,digits=5),round(test$p.value,digits=5)))
  }
  write.csv(sw,"Logs/Data_exploration/sw_test.csv")

  ############################################################
  # Correlation matrix
  png("Plots/Data_exploration/corrplot.png",width=1920,height=1017)
  tmpdataset <- dataset[,2:31]
  correlations <- cor(tmpdataset,method="pearson")
  corrplot(correlations, number.cex = 1, method = "square",
           hclust.method = "ward", order = "FPC",
           type = "full", tl.cex=1,tl.col = "black")
  t <- 0.81
  corr.var <- findCorrelation(correlations, cutoff = t, verbose = TRUE, exact = TRUE, names = TRUE)
  return(corr.var)
  dev.off()
}
