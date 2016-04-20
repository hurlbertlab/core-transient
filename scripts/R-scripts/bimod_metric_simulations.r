# Simulating beta distributions that differ in bimodality
# to test the H-W bimodality function

# This script generates 100 random samples of occupancy
# from each of 5 different beta distributions (each with
# parameters alpha and beta).

# (0.5, 0.5) should be strongly bimodal
# (1, 1) should be uniform
# All others should be strongly unimodal

source('scripts/R-scripts/core-transient_functions.R')



# Set of beta distribution parameters to use
beta = c(0.5, 1, 10, 10, 1)
alpha = c(0.5, 1, 1, 10, 10)

params = data.frame(alpha, beta)

reps = 100
nTime = 10

output = c()
for (i in 76:reps) {
  for (p in 1:nrow(params)) {
    tempocc = ceiling(nTime*rbeta(100, shape1 = params$alpha[p], shape2 = params$beta[p]))/nTime
    bimodality = bimodalityFun(tempocc, nTime)
    pBimodal = pBimodalFun(tempocc, nTime, 999)
    output = rbind(output, c(p, params$alpha[p], params$beta[p], i, bimodality, pBimodal))
    }
}
output = data.frame(output)
names(output) = c('distID', 'alpha', 'beta', 'i', 'bimodality', 'pBimodal')
write.csv(output, 'output/tabular_data/bimodality_simulation_test.csv', row.names=F)


# Plotting
par(mfrow = c(2, 1), mar = c(4, 4, 1, 1))
boxplot(bimodality ~ distID, data = output)
boxplot(pBimodal ~ distID, data = output)