#Project 1

#Load data from experiment 3
d <- armdata[[3]]
print(d)

plot(d[[10]][[10]][,1],d[[10]][[10]][,2])

x <-c()
for (i in 1:10){
  for (j in 1:10){
    for (k in 1:3){
      x <- c(x, d[[i]][[j]][,k])
    }
  }
}
x <- matrix(x,nrow=100, byrow=F)
y <- rep(1:10, each=10)

