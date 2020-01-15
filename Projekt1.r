#Project 1

#Load data from experiment 3
d <- armdata[[3]]

#preprocess data
x <-c()
for (i in 1:10){
  for (j in 1:10){
    for (k in 1:3){
      x <- c(x, d[[i]][[j]][,k])
    }
  }
}
x <- matrix(x,nrow=300, byrow=F)
y <- rep(1:10, each=10)
rep <- rep(1:10, times=10)

x <- data.frame(t(x))
#trans <- t(x)
#y <- rep(1:10, times=10, each=3)
features <- c(sprintf("x%03d", seq(1,100)),sprintf("y%03d", seq(1,100)),sprintf("z%03d", seq(1,100)))
colnames(x) <- features


data <- cbind(y,rep,x)
#Tjek for rows without data
any(is.na(x))

#create model to perform statistics


# ASGERS KODE OVER ALLE EXPERIMENTER
raw_movement <- unlist(armdata, recursive = T)
coordinate <- rep( c(
  rep("x", 100),
  rep("y", 100),
  rep("z", 100)
),
1600
)
repetition <- c()
subject <- c()
experiment <- c()
for (i in 1:16){ #experiments
  print(i)
  for (j in 1:10){ #persons
    for (k in 1:10){ #repetitions
      experiment <- c(experiment, rep(i, 300))
      subject <- c(subject, rep(j, 300))
      repetition <- c(repetition, rep(k, 300))
    }
  }
}

arm_dataframe <- data.frame(
  pos <- raw_movement,
  coordinate <- as.factor(coordinate),
  repetition <- as.factor(repetition),
  subject <- as.factor(subject),
  experiment <- as.factor(experiment)
)
#Use last (control) experiment as reference
levels(arm_dataframe$experiment) <-c(2:16, 1)

model <- lm(pos ~ coordinate + repetition + subject + experiment)
anova(model)
summary(model)

