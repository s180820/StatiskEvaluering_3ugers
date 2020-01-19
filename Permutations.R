#Permutations 

perm_data <- phos_data
n <- 10000
results_OGDGT <- c()
results_OGolsenP <- c()
results_DGT <- c()
results_olsenP <- c()
results_DGT_lm <- c()
results_olsenP_lm <- c()

for (i in 1:n){
  permutations <- sample(nrow(perm_data))
  
  phos.model.DGT_perm <- nls(yield[permutations]~alpha *DGT/(DGT+beta), 
                            data=perm_data, start=list(alpha=90, beta=1), nls.control(warnOnly = T))
  phos.model.olsenP_perm <- nls(yield[permutations]~alpha *olsenP/(olsenP+beta), 
                            data=perm_data, start=list(alpha=90, beta=1), nls.control(warnOnly = T))
  
  phos.model.DGT_lm_perm <- lm(yield[permutations]~DGT, data = perm_data)
  phos.model.olsenP_lm_perm <- lm(yield[permutations]~olsenP, data = perm_data)
  
  results_DGT <- rbind(results_DGT , coef(phos.model.DGT_perm))
  results_olsenP <- rbind(results_olsenP, coef(phos.model.olsenP_perm))
  results_DGT_lm <- rbind(results_DGT_lm, coef(phos.model.DGT_lm_perm))
  results_olsenP_lm <- rbind(results_olsenP_lm, coef(phos.model.olsenP_lm_perm))
  
}

for(i in 1:34){
  phos.model.DGT <- nls(yield~alpha *DGT/(DGT+beta), 
                             data=phos_data, start=list(alpha=90, beta=1), nls.control(warnOnly = T))
  phos.model.olsenP <- nls(yield~alpha *olsenP/(olsenP+beta), 
                                data=phos_data, start=list(alpha=90, beta=1), nls.control(warnOnly = T))
  results_OGDGT <- rbind(results_DGT , coef(phos.model.DGT))
  results_OGolsenP <- rbind(results_olsenP, coef(phos.model.olsenP))
}


par(mfrow = c(2,2))
hist(results_DGT[,1])#Dette er alpha
hist(results_DGT[,2])#Dette er en beta

hist(results_olsenP[,1])
hist(results_olsenP[,2])



hist(results_DGT_lm[,1])
abline(v=coef(lmDGT[1])) #fra vores project2.R (den rigtige værdi inden permutation)







par(mfrow=c(1,2),oma=c(0,0,2,0))

plot(phos_data$DGT, results_DGT, col=factor(phos_data$location), xlab = "DGT", ylab = "Yield")
lines(v1,yfitted1)

mtext("Prediction of yield for the two nls models", line=0, side=3, outer=TRUE, cex=2)

plot(phos_data$olsenP, phos_data$yield,xlim=c(0,9),col=factor(phos_data$location), xlab = "olsenP", ylab = "Yield")
lines(v2,yfitted2)

