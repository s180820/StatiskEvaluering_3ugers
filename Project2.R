### Project 2

#Load data 
phos_data <- Phosphorous

#Michaelis-Menten model (non-linear regression )
#y=a*x/(1+b*x), hvor x=DGT, y=yield.
phos_DGT.model <- nls(yield ~ alpha * DGT/(beta+DGT), data = Phosphorous, start = list(alpha = 90, beta = 1))
phos_Olsen.model <- nls(yield ~ alpha * olsenP/(beta+olsenP), data = Phosphorous, start = list(alpha = 90, beta = 1))

summary(phos_DGT.model) # Residual standard error=10.84 and df=32 and p=<2e-16
summary(phos_Olsen.model) # Residual standard error=14.65 and df=32 and p=3.33e-11

plot(phos_data)

par(mfrow=c(1,2))
plot(phos_data$DGT, phos_data$yield, col = phos_data$location)
plot(phos_data$olsenP, phos_data$yield, col = phos_data$location)




