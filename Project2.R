### Project 2

#Load data 
phos_data <- Phosphorous
phos_data$location <- rep(1:9, each =4)
phos_data <- na.omit(phos_data) #as.dataframe(phos_data, drop.


#Michaelis-Menten model (non-linear regression )
#y=a*x/(1+b*x), hvor x=DGT, y=yield.
phos_DGT.model <- nls(yield ~ alpha * DGT/(beta+DGT), data = Phosphorous, start = list(alpha = 90, beta = 1))
phos_Olsen.model <- nls(yield ~ alpha * olsenP/(beta+olsenP), data = Phosphorous, start = list(alpha = 90, beta = 1))

summary(phos_DGT.model) # Residual standard error=10.84 and df=32 and p=<2e-16
summary(phos_Olsen.model) # Residual standard error=14.65 and df=32 and p=3.33e-11



v1<-seq(0,170,0.1)
v2<-seq(0,9,0.1)
yfitted1 <- predict(phos_DGT.model,list(DGT=v1))
yfitted2 <- predict(phos_Olsen.model,list(olsenP=v2))


yfitted1 <- predict(phos_DGT.model,phos_data)
yfitted2 <- predict(phos_Olsen.model)

par(mfrow=c(1,2),oma=c(0,0,2,0))

plot(phos_data$DGT, phos_data$yield, col=factor(phos_data$location), xlab = "DGT", ylab = "Yield")
lines(v1,yfitted1)

mtext("Prediction of yield for the two nls models", line=0, side=3, outer=TRUE, cex=2)

plot(phos_data$olsenP, phos_data$yield,xlim=c(0,9),col=factor(phos_data$location), xlab = "olsenP", ylab = "Yield")
lines(v2,yfitted2)

par(mfrow=c(1,2))
plot((phos_data$yield-yfitted1)**2,ylim=c(0,1400), main = "DGT", xlab = "Yield index", ylab = "Quadric loss")
mtext("Loss of the two models", line=0, side=3, outer=TRUE, cex=2)
plot((phos_data$yield-yfitted2)**2,ylim=c(0,1400), main = "olsenP", xlab = "Yield index" )

#Leave-one-out cross

fittedDGT <- c()
fittedolsenP <- c()
lossDGT <- c()
lossolsenP <- c()
for(i in 1:9){
  d <- phos_data[!(phos_data$location==i),]
  phos_DGT.model <- nls(yield ~ alpha * DGT/(beta+DGT), data = d, start = list(alpha = 90, beta = 1))
  phos_Olsen.model <- nls(yield ~ alpha * olsenP/(beta+olsenP), data = d, start = list(alpha = 90, beta = 1))
  yfitted1 <- predict(phos_DGT.model,d)
  yfitted2 <- predict(phos_Olsen.model,d)
  fittedDGT <- c(fittedDGT, d$yield-yfitted1)
  fittedolsenP <- c(fittedolsenP, d$yield-yfitted2)
  lossDGT <- c(lossDGT, mean((d$yield-yfitted1)**2))
  lossolsenP <- c(lossolsenP, mean((d$yield-yfitted2)**2))
}


#Loss mean
mean(lossDGT)
mean(lossolsenP)


fittedDGT
fittedolsenP

par(mfrow=c(1,2))
plot(lossDGT,ylim=c(0,300), main = "DGT", xlab = "CV fold", ylab = "Quadric loss")
mtext("Mean quadric loss of the two models", line=0, side=3, outer=TRUE, cex=2)
plot(lossolsenP,ylim=c(0,300), main = "olsenP", xlab = "CV fold" )



t.test(lossDGT, lossolsenP)

mean(lossDGT)
mean(lossolsenP)

mean(abs(fittedDGT))
mean(abs(fittedolsenP))

par(mfrow=c(1,2))
#qq-plot for at tjekke normalfordeling af yield. The errors are normally distributed. 
qqnorm(phos_data$yield)
qqline(phos_data$yield)
hist(phos_data$yield)

qqnorm(phos_data$DGT)
qqline(phos_data$DGT)
hist(phos_data$DGT)#strongly positive skewed due to median 42 and mean 65
summary(phos_data$DGT)

qqnorm(phos_data$olsenP)
qqline(phos_data$olsenP)
hist(phos_data$olsenP) #strongly positive skewed due to median 4 and mean 4.3
summary(phos_data$olsenP)


#Scatterplots af data
par(mfrow=c(1,2))
plot(phos_data$DGT, phos_data$yield, col = phos_data$location)
plot(phos_data$olsenP, phos_data$yield, col = phos_data$location)

set.seed(123)
## 75% to training data, 25% to test data
train_ids <- 1:30

# Split data
phos_train <- phos_data[train_ids, ]
phos_test <- phos_data[- train_ids, ]

#Predictions
predictions <- predict(phos_DGT.model, phos_test)
MSPE_DGT <- mean((phos_test$DGT - predictions)^2) ## Mean squared prediction error
MSPE_DGT #1121

predictions <- predict(phos_Olsen.model, phos_test)
MSPE_OLSEN <- mean((phos_test$olsenP - predictions)^2) ## Mean squared prediction error
MSPE_OLSEN #4470.073



#mean af hver variabel i samme enhed
mean(phos_data$DGT*(1/10000)) #0.0065
mean(phos_data$olsenP) #4.3059


#Residuals
# Plots of residuals
formula1 <- as.formula(yield ~ alpha * DGT/(beta+DGT))
O2K.nls1 <- nls(formula1, data = Phosphorous, start = list(alpha = 90, beta = 1))
O2K.res1 <- nlsResiduals(O2K.nls1)
plot(O2K.res1, which = 0)

# Histogram and qq-plot
plot(O2K.res1, which = 5)
plot(O2K.res1, which = 6)

# Tests
test.nlsResiduals(O2K.res1)

qqnorm(phos_DGT.model$residuals)


#OlsenP
formula2 <- as.formula(yield ~ alpha * olsenP/(beta+olsenP))
O2K.nls2 <- nls(formula2, data = Phosphorous, start = list(alpha = 90, beta = 1))
O2K.res2 <- nlsResiduals(O2K.nls2)
plot(O2K.res2, which = 0)

# Histogram and qq-plot
plot(O2K.res2, which = 5)
plot(O2K.res2, which = 6)

# Tests
test.nlsResiduals(O2K.res2)



