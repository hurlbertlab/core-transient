# Simulating beta distributions that differ in modality
# to test the H-W bimodality function

source('scripts/R-scripts/core-transient_functions.R')



# Set of beta distribution parameters to use
beta = c(0.5, 1, 10, 10, 1)
alpha = c(0.5, 1, 1, 10, 10)

params = data.frame(alpha, beta)

reps = 1000
nTime = 10

output = c()
for (i in 1:reps) {
  for (p in 1:nrow(params)) {
    tempocc = ceiling(nTime*rbeta(100, shape1 = params$alpha[p], shape2 = params$beta[p]))/nTime
    bimodality = bimodalityFun(tempocc, nTime)
    pBimodal = pBimodalFun(tempocc, nTime, 999)
    output = rbind(output, c(params$alpha[p], params$beta[p], i, bimodality, pBimodal))
    }
}
output = data.frame(output)
names(output) = c('alpha', 'beta', 'i', 'bimodality', 'pBimodal')