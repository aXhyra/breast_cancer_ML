library(caret)
library(doParallel)

cores <- 16
registerDoParallel(cores = 16)
cluster <- makeCluster(cores)

data <-  read.csv("Breast_cancer/data.csv", stringsAsFactors = F)

data <- data[,2:32]


set.seed(666)

train_control <- trainControl(method = "repeatedcv",
                              number = 10, repeats = 2)


tg <- expand.grid(layer1=c(50,100),layer2=20,layer3=0)


model <- train(diagnosis ~., data = data,
               method = 'mlpML',
               tuneGrid=tg,
               trControl = train_control)

print(model)

stopCluster(cluster)