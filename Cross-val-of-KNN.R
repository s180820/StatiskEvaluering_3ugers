################################
### 10 fold cross-validation ###
################################

#Preprocessing of the data
d <- armdata[[3]]
print(d)

x0 <-c()
for (i in 1:10){
  for (j in 1:10){
    for (k in 1:3){
      x0 <- c(x0, d[[i]][[j]][,k])
    }
  }
}
x0 <- matrix(x0,nrow=300, byrow=F)
y <- rep(1:10, each=10)

x0 <- data.frame(t(x0))
#trans <- t(x)
#y <- rep(1:10, times=10, each=3)

data <- cbind(x0,y)
#Tjek for rows without data
any(is.na(x0))

library(ISLR)
library(caret)

set.seed(300)
#Spliting data as training and test set. Using createDataPartition() function from caret
indxTrain <- createDataPartition(y = data$y,p = 0.90,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,]

#Checking distibution in original data and partitioned data
prop.table(table(training$y)) * 100

trainX <- training[,names(training) != "Y"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues

#Training and control
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(y ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
#NB! Crossvalidation is repeated 3 times

#Output of kNN fit
knnFit

#Plotting the fit
plot(knnFit, main = 'K-Nearest Neighbor')
#RMSE -> Prediction error (standard deviation of the residuals)

knnPredict <- predict(knnFit,newdata = testing )

#Get the confusion matrix to see accuracy value and other parameter values
u <- union(round(knnPredict), testing$y)
t <- table(factor(round(knnPredict), u), factor(testing$y, u))
confusionMatrix(t)
#NB! Giver måske meget god mening at den er så lav, når den predicter en accuracy af kommatal som afrundes...

