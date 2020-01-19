#Project 1

#Load data from experiment 3
d <- armdata[[3]]
library("scatterplot3d")
x1 <- d[[1]][[1]][,1]
y1 <- d[[1]][[1]][,2]
z1 <- d[[1]][[1]][,3]

x2 <- d[[1]][[10]][,1]
y2 <- d[[1]][[10]][,2]
z2 <- d[[1]][[10]][,3]

x3 <- d[[5]][[1]][,1]
y3 <- d[[5]][[1]][,2]
z3 <- d[[5]][[1]][,3]

x4 <- d[[5]][[10]][,1]
y4 <- d[[5]][[10]][,2]
z4 <- d[[5]][[10]][,3]

par(mfrow=c(2,2))

scatterplot3d(x1, y1, z1, highlight.3d = TRUE, col.axis = "blue",
              col.grid = "lightblue", main = "1. Person, First repitition", pch = 20, angle=12)

scatterplot3d(x2, y2, z2, highlight.3d = TRUE, col.axis = "blue",
              col.grid = "lightblue", main = "1. Person, Tenth repitition", pch = 20, angle=12)

scatterplot3d(x3, y3, z3, highlight.3d = TRUE, col.axis = "blue",
              col.grid = "lightblue", main = "5. Person, First repitition", pch = 20, angle=12)

scatterplot3d(x4, y4, z4, highlight.3d = TRUE, col.axis = "blue",
              col.grid = "lightblue", main = "5. Person, Tenth repitition", pch = 20, angle=12)
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


##Max coordinates
d <- armdata
x <-c()
for (h in 1:16){
  for (i in 1:10){
    for (j in 1:10){
      for (k in 1:3){
        x <- c(x, d[[h]][[i]][[j]][,k])
      }
    }
  }
}
data <- matrix(x,nrow=300, byrow=F)
data <- data.frame(t(data))

diff_x <- c()
diff_y <- c()
diff_z <- c()
for(i in 1:1600){
  xmax <- max(data[i,][1:100])
  ymax <- max(data[i,][101:200])
  zmax <- max(data[i,][201:300])
  xmin <- min(data[i,][1:100])
  ymin <- min(data[i,][101:200])
  zmin <- min(data[i,][201:300])
  diff_x <- c(diff_x,xmax-xmin)
  diff_y <- c(diff_y,ymax-ymin)
  diff_z <- c(diff_z,zmax-zmin)
}

experiment <- rep(1:16, each=100)
temp <- cbind(as.factor(experiment), diff_x, diff_y, diff_z)
temp <- data.frame(temp)

tempmodel <- lm(experiment ~ diff_x + diff_y + diff_z, data = temp)

# ANOVA when splitted into three models

model1 <- lm(diff_x ~ experiment, data=temp)
model2 <- lm(diff_y ~ experiment, data=temp)
model3 <- lm(diff_z ~ experiment, data=temp)

anova(model1)
anova(model2)
anova(model3)

# Adjusting p-value due to multiple comparisons

pvals <- c(anova(model1)[[5]][[1]],anova(model2)[[5]][[1]],anova(model3)[[5]][[1]])
pvals
p.adjust(pvals,method = "bonferroni")
p.adjust(pvals,method = "BH")
p.adjust(pvals,method = "hochberg")
p.adjust(pvals,method = "fdr")

# Significant effect of experiment on the x,y and z direction

anova(tempmodel)
temp
summary(temp)


