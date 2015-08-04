###############################################
# Code for running core-transient analysis
# and data summaries over all formatted datasets.
#
# Input files are named propOcc_XXX.csv where
# XXX is the dataset ID.

source('scripts/R-scripts/core-transient_functions.R')

# Maximum occupancy of transient species
# (and hence the minimum occupancy of core species is 1 - threshold)
threshold = 1/3

# Number of replicates for randomization tests
reps = 999

##################################################################

# If running summaries for the first time (or wanting to start
# anew because all formatted datasets have changed) and a
# 'core-transient_summary.csv' file does not exist yet in the
# output/tabular_data folder, or if you just want to get summary
# stats for one or a few datasets into R, run this section

# Specify here the datasetIDs and then run the code below.
datasetIDs = c()

summaries = c()
for (d in datasetIDs) {
  newsumm = summaryStatsFun(d, threshold, reps)
  summaries = rbind(summaries, newsumm)
}

write.csv(summaries, 'output/tabular_data/core-transient_summary.csv', 
          row.names = F)

##################################################################

##################################################################

# If running summaries for the newly updated or created formatted
# datasets to be appended to the existing 'core-transient_summary.csv'
# file, then run this section.

# If you do not want to re-write the existing file, set write = FALSE.

# Also, this command can be used instead of the section above to
# create the 'core-transient_summary.csv' file from scratch for all
# datasets with formatted data.

summ = addNewSummariesFun(threshold, reps, write = TRUE)


#####################

# Plotting summary results across datasets for Core-Transient analysis

summ = read.csv('output/tabular_data/core-transient_summary.csv', header=T)
summ$taxa = factor(summ$taxa)
summ$system = factor(summ$system)
summ4 = subset(summ, datasetID != 99)
dsets = unique(summ4[, c('datasetID', 'system','taxa')])

taxorder = c('Bird', 'Plankton', 'Arthropod', 'Benthos', 'Fish', 'Plant', 'Mammal')

dsetsBySystem = table(dsets$system)
dsetsByTaxa = table(dsets$taxa)
sitesBySystem = table(summ4$system)
sitesByTaxa = table(summ4$taxa)

dsetsByTaxa = dsetsByTaxa[taxorder]
sitesByTaxa = sitesByTaxa[taxorder]

colors7 = c(rgb(29/255, 106/255, 155/255),
            colors()[612],
            colors()[552],
            colors()[144],
            rgb(0, 54/255, 117/255),
            rgb(86/255, 141/255, 27/255),
            colors()[547])

symbols7 = c(16:18,15, 17, 167,18)

par(mfrow = c(2, 2), mar = c(1,1,1,1), cex = 1.25)
pie(dsetsBySystem, main = paste("By dataset (n = ", nrow(dsets), ")", sep = ""),
    col = c('skyblue', 'burlywood'))
pie(sitesBySystem, main = paste("By site (n = ", nrow(summ4), ")", sep = ""),
    col = c('skyblue', 'burlywood'))
pie(dsetsByTaxa, col = colors7, init.angle = 30)
pie(sitesByTaxa, col = colors7, init.angle = 60)



#####################################################
# Boxplots showing distribution of core and transient
# species by taxon.
library(wesanderson)

summ4$propNeither = 1 - summ4$propCore - summ4$propTrans

coreCol = rgb(102/255, 102/255, 255/255, alpha = 1)
nonCol = 'gray70'
transCol = rgb(204/255, 88/255, 0, alpha = 1)

meanCoreByTaxa = aggregate(summ4$propCore, by = list(summ4$taxa), mean)
uniqTaxa = meanCoreByTaxa$Group.1[order(meanCoreByTaxa$x, decreasing = T)]

par(mfrow = c(1,1), mar = c(5, 5, 1, 1), mgp = c(3, 1, 0))
boxplot(summ4$propCore, xlim = c(0, (3*length(uniqTaxa)-2)), ylim = c(0, 1.2), 
        border = 'white', col = 'white', ylab = "Fraction of species", cex.lab = 2, las = 1, 
        cex.axis = 1.25)
for (i in 1:length(uniqTaxa)) {
  tax = uniqTaxa[i]
  boxplot(summ4$propTrans[summ4$taxa == tax], add = T, col = transCol, staplewex = 0, outline = F,
          at = 3*(i-1), yaxt = "n")
  boxplot(summ4$propNeither[summ4$taxa == tax], add =T, col = nonCol, staplewex = 0, outline = F,
          at = 3*(i-1)+.5, yaxt = "n")
  boxplot(summ4$propCore[summ4$taxa == tax], add =T, col = coreCol, staplewex = 0, outline = F,
          at = 3*(i-1)+1, yaxt = "n")
}
axis(1, uniqTaxa, at = 3*(1:7)-2.5, cex.axis = 1.4)
rect(.5, 1.1, 1.5, 1.2, col = transCol, border=F)
rect(6.5, 1.1, 7.5, 1.2, col = nonCol, border=F)  
rect(12.5, 1.1, 13.5, 1.2, col = coreCol, border=F)  
text(c(3.4, 9, 14.5), c(1.15, 1.15, 1.15), c('Transient', 'Neither', 'Core'), cex = 1.75)


#########################################################################################
# Summarizing datasets based on beta distribution parameters
par(mfrow = c(1,1), mar = c(6,6,1,1), mgp = c(4,1, 0))
plot(summ3$alpha, summ3$beta, type = "n", xlim = c(0,4), xlab = "alpha", ylab = "beta")
points(summ3$alpha, summ3$beta, pch = summ3$pch, col = summ3$color, font = 5, cex = 2)
abline(a=0, b=1, lty = 'dotted', lwd = 2)
rect(-1, -1, 1, 1, lty = 'dashed', lwd = 4)
legend('topleft', legend = unique(summ$taxa), pch = symbols7, 
       col = c(colors7[1:5], 'white', colors7[7]), pt.cex = 2, cex = 1.5)
points(-.025, 3.3, pch = symbols7[6], font = 5, col = colors7[6], cex = 2)

# Example beta distributions
bimodist = dbeta(0:100/100, 0.8, 0.8)
coredist = dbeta(0:100/100, 5, 1.2)
trandist = dbeta(0:100/100, 1.2, 5)

par(mfrow = c(1,1), mar = c(6, 6, .4, .4), mgp = c(2, 0, 0))
plot(bimodist, type = 'l', lwd = 6, xaxt = "n", yaxt = "n", xlab = "Occupancy",
     ylab = "Frequency", cex.lab = 4)
plot(coredist, type = 'l', lwd = 6, xaxt = "n", yaxt = "n", xlab = "Occupancy",
     ylab = "Frequency", cex.lab = 4)
plot(trandist, type = 'l', lwd = 6, xaxt = "n", yaxt = "n", xlab = "Occupancy",
     ylab = "Frequency", cex.lab = 4)



####################################################################################
# Summary of other distribution statistics by taxa

par(mfrow = c(1,1), mar = c(6, 6, 1, 1), mgp = c(4, 1, 0))
summ4$taxa = with(summ4, reorder(taxa, bimodality, function(x) mean(x, na.rm = T)))
boxplot(summ4$bimodality ~ summ4$taxa, cex.axis = 1.25, ylab = "Bimodality", boxwex = .6)
summ5 = summ4
summ5$taxa = with(summ5, reorder(taxa, pBimodal, function(x) mean(x, na.rm = T)))
boxplot(summ5$pBimodal ~ summ5$taxa, cex.axis = 1.25, ylab = "p (Bimodal)", boxwex = .6)


#####################################################
# Boxplots showing distribution of core and transient
# species by system (terrestrial/marine).

meanCoreBySystem = aggregate(summ$propCore, by = list(summ$system), mean)
uniqsystem = meanCoreBySystem$Group.1[order(meanCoreBySystem$x, decreasing = T)]

par(mfrow = c(1,1), mar = c(5, 5, 1, 1), mgp = c(3, 1, 0))
boxplot(summ$propCore, xlim = c(0.25, 4.75), ylim = c(0, 1.2), 
        border = 'white', col = 'white', ylab = "Fraction of species", cex.lab = 2, las = 1, 
        cex.axis = 1.25)
for (i in 1:length(uniqsystem)) {
  syst = uniqsystem[i]
  boxplot(summ$propTrans[summ$system == syst], add = T, col = transCol, #width = 0.1, 
          at = 3*(i-1)+.5, yaxt = "n")
  boxplot(summ$propNeither[summ$system == syst], add =T, col = nonCol, #width = 0.2,
          at = 3*(i-1)+1, yaxt = "n")
  boxplot(summ$propCore[summ$system == syst], add =T, col = coreCol, #width = 1,
          at = 3*(i-1)+1.5, yaxt = "n")
}
axis(1, uniqsystem, at = 3*(1:2 - 1)+1, cex.axis = 2)
rect(.2, 1.1, .45, 1.2, col = transCol, border=F)
rect(1.5, 1.1, 1.75, 1.2, col = nonCol, border=F)  
rect(2.7, 1.1, 2.95, 1.2, col = coreCol, border=F)  
text(c(.9, 2.1, 3.15), c(1.15, 1.15, 1.15), c('Transient', 'Neither', 'Core'), cex = 1.75)

