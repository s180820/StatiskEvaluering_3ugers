#Project 1

#Load data from experiment 3
d <- armdata[3]
print(d)

repetitions <- 10
participants <- 10
experiments <- 16

y <- as.matrix(read.table('armTrajectories.dat'))
dim(y) <- c(nrow(y), 3, repetitions , participants , experiments)

yvelo <- as.matrix(read.table('armVelocity.dat'))
dim(yvelo) <- c(nrow(yvelo), 3, repetitions , participants , experiments)





