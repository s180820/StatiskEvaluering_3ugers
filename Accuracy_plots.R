##############
## KNN & RF ##
##############


require(tree);
library(class);
library(randomForest);

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
rep <- rep(1:10, times=10)

x0 <- data.frame(t(x0))
#trans <- t(x)
#y <- rep(1:10, times=10, each=3)

data <- cbind(y,rep,x0)
#Tjek for rows without data
any(is.na(x0))

knnacc <- c()
set.seed(122)
for (j in 1:20) {
  knn.preds <- rep(NA,100)
  for (i in 1:100) {
    #rf.model <- randomForest(as.factor(y) ~ . - rep, data = data, subset = setdiff(1:100, i));
    #tree.preds[i] <- predict(rf.model, data[i, ], type = "class");
    knn.preds[i] <- knn(data[setdiff(1:100, i), 3:302], data[i, 3:302], cl = data[setdiff(1:100, i), ]$y, k = j);
  }
  knnacc <- c(knnacc, mean(knn.preds == data$y))  
}

N <- 1:20
print(knnacc)
plot(N,knnacc, xlab = "Number of Neighbors", ylab = "Accuracy", main = "KNN Accuracy")

set.seed(122)
treeacc <- c()
for (j in 550:570) {
  tree.preds <- rep(NA,100)
  for (i in 1:100) {
    rf.model <- randomForest(as.factor(y) ~ . - rep, data = data, subset = setdiff(1:100, i),ntree = j);
    tree.preds[i] <- predict(rf.model, data[i, ], type = "class");
    #knn.preds[i] <- knn(data[setdiff(1:100, i), 3:302], data[i, 3:302], cl = data[setdiff(1:100, i), ]$y, k = j);
  }
  treeacc <- c(treeacc, mean(tree.preds == data$y)) 
}

treeacc

set.seed(122)
treeacc <- c()
  for (i in 1:100) {
    rf.model <- randomForest(as.factor(y) ~ . - rep, data = data, subset = setdiff(1:100, i));
    tree.preds[i] <- predict(rf.model, data[i, ], type = "class");
    #knn.preds[i] <- knn(data[setdiff(1:100, i), 3:302], data[i, 3:302], cl = data[setdiff(1:100, i), ]$y, k = j);
  }
treeacc <- c(treeacc,tree.preds == data$y); 

treeacc

N <- 1:20
print(knnacc)
plot(N,knnacc, xlab = "Number of Neighbors", ylab = "Accuracy", main = "KNN Accuracy")



