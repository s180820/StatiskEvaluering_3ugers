#Permutations 
perm_data <- split(phos_data$yield,phos_data$location)
permutations_data <- phos_data

#perm_data <- phos_data
n <- 10000

results_DGT <- c()
results_olsenP <- c()
results_DGT_lm <- c()
results_olsenP_lm <- c()

for (i in 1:n){
  permutations <- sample(9)
  permutations_data$yield <- unlist(perm_data[permutations]) 
  phos.model.DGT_perm <- nls(yield~alpha * DGT/(DGT+beta), data = permutations_data,
                            start=list(alpha=90, beta=1), nls.control(warnOnly = T))
  phos.model.olsenP_perm <- nls(yield[permutations]~alpha *olsenP/(olsenP+beta), 
                            data=permutations_data, start=list(alpha=90, beta=1), nls.control(warnOnly = T))
  
  phos.model.DGT_lm_perm <- lm(yield~DGT, data = permutations_data)
  phos.model.olsenP_lm_perm <- lm(yield~olsenP, data = permutations_data)
  
  results_DGT <- rbind(results_DGT , coef(phos.model.DGT_perm))
  results_olsenP <- rbind(results_olsenP, coef(phos.model.olsenP_perm))
  results_DGT_lm <- rbind(results_DGT_lm, coef(phos.model.DGT_lm_perm))
  results_olsenP_lm <- rbind(results_olsenP_lm, coef(phos.model.olsenP_lm_perm))

}

results_DGT[,2]
results_DGT[,1]
results_DGT[,1][1]

par(mfrow = c(1,2))
hist(results_DGT[,1])
abline(v=results_DGT[,1][1], col='red')
hist(results_DGT[,2])
abline(v=results_DGT[,2][1], col='red')

#P-værdi for alpha
mean(results_DGT[,1] > results_DGT[,1][1])

#P-værdi for beta
mean(results_DGT[,2] > results_DGT[,2][1])

hist(results_DGT_lm[,1])
abline(v=coef(lmDGT[1])) #fra vores project2.R (den rigtige værdi inden permutation)
