###############################################
# Code for running core-transient analysis
# and data summaries over all formatted datasets.
#
# Input files are named propOcc_XXX.csv where
# XXX is the dataset ID.

# setwd("C:/git/core-transient")

library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
# library(sp)

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
dataformattingtable = read.csv('data_formatting_table.csv', header = T) 

datasetIDs = dataformattingtable$dataset_ID[dataformattingtable$format_flag == 1]

datasetIDs = datasetIDs[!datasetIDs %in% c(1)]

summaries = c()
for (d in datasetIDs) {
  newsumm = summaryStatsFun(d, threshold, reps)
  summaries = rbind(summaries, newsumm)
  print(d)
}

write.csv(summaries, 'output/tabular_data/core-transient_summary_test.csv', 
          row.names = T)

                                               

##################################################################

# If running summaries for the newly updated or created formatted
# datasets to be appended to the existing 'core-transient_summary.csv'
# file, then run this section.

# If you do not want to re-write the existing file, set write = FALSE.

# Also, this command can be used instead of the section above to
# create the 'core-transient_summary.csv' file from scratch for all
# datasets with formatted data.

summ = addNewSummariesFun(threshold, reps, write = TRUE)


#####################lump reptile and ampibian into herptile, get rid of invert if possible - other category?, do a table of communities

# Plotting summary results across datasets for Core-Transient analysis

summ = read.csv('output/tabular_data/core-transient_summary.csv', header=T)
summ$taxa = factor(summ$taxa)
summ$system = factor(summ$system)
summ2 = subset(summ, !datasetID %in% c(99, 85, 90, 91, 92, 97, 124))
dsets = unique(summ2[, c('datasetID', 'system','taxa')])

taxorder = c('Bird', 'Plant', 'Mammal', 'Fish', 'Arthropod', 'Benthos', 'Plankton', 'Invertebrate', 'Herptile')

dsetsBySystem = table(dsets$system)
dsetsByTaxa = table(dsets$taxa)
sitesBySystem = table(summ2$system)
sitesByTaxa = table(summ2$taxa)

colors7 = c(rgb(29/255, 106/255, 155/255),
            colors()[552],
            colors()[612],
            colors()[144],
            rgb(0, 54/255, 117/255),
            colors()[600],
            colors()[551],
            rgb(86/255, 141/255, 27/255), #added!
            colors()[91]) #added!

symbols7 = c(16, 18, 167, 15, 17, 1, 3, 20, 24) # added 19-20!

taxcolors = data.frame(taxa = unique(summ$taxa), color = colors7, pch = symbols7)

pdf('output/plots/data_summary_hists.pdf', height = 8, width = 10)
par(mfrow = c(2, 2), mar = c(6,6,1,1), cex = 1.25, oma = c(0,0,0,0), las = 1,
    cex.lab = 1.5)
barplot(dsetsBySystem, col = c('skyblue', 'navy', 'burlywood'), cex.names = 1)
mtext("# Datasets", 2, cex = 1.5, las = 0, line = 2.5)
barplot(log10(sitesBySystem), col = c('skyblue', 'navy', 'burlywood'), cex.names = 1,
        yaxt = "n", ylim = c(0,3))
axis(2, 0:3)
mtext(expression(log[10] ~ " # Assemblages"), 2, cex = 1.5, las = 0, line = 2.5)
bar1 = barplot(dsetsByTaxa[taxorder], xaxt = "n", axisnames = F,
        col = as.character(taxcolors$color[match(taxorder, taxcolors$taxa)]))
text(bar1, par("usr")[3], taxorder, srt = 60, adj = c(1, 1), xpd = TRUE, cex = 1.25)
axis(2, 0:5)
mtext("# Datasets", 2, cex = 1.5, las = 0, line = 2.5)
bar2 = barplot(log10(sitesByTaxa[taxorder]), axes = F, axisnames = F, ylim = c(0,3),
        col = as.character(taxcolors$color[match(taxorder, taxcolors$taxa)]))
text(bar2, par("usr")[3], taxorder, srt = 60, adj = c(1, 1), xpd = TRUE, cex = 1.25)
axis(2, 0:3)
mtext(expression(log[10] ~ " # Assemblages"), 2, cex = 1.5, las = 0, line = 2.5)
dev.off()



#####################################################
# Boxplots showing distribution of core and transient
# species by taxon.

summ2$propNeither = 1 - summ2$propCore - summ2$propTrans

coreCol = rgb(102/255, 102/255, 255/255, alpha = 1)
nonCol = 'gray70'
transCol = rgb(204/255, 88/255, 0, alpha = 1)

meanCoreByTaxa = aggregate(summ2$propCore, by = list(summ2$taxa), mean)
uniqTaxa = meanCoreByTaxa$Group.1[order(meanCoreByTaxa$x, decreasing = T)]

pdf('output/plots/CT_boxplots_byTaxa.pdf', height = 6, width = 8)
par(mfrow = c(1,1), mar = c(6, 5, 1, 1), mgp = c(3, 1, 0), oma = c(0,0,0,0))
box1 = boxplot(summ2$propCore, xlim = c(0, (3*length(uniqTaxa)-2)), ylim = c(0, 1), 
        border = 'white', col = 'white', ylab = "Fraction of species", cex.lab = 1, las = 1, 
        cex.axis = 1.25)
for (i in 1:length(uniqTaxa)) {   ##### wonky labelling somewhere in here
  tax = uniqTaxa[i]
  boxplot(summ2$propTrans[summ2$taxa == tax], add = T, col = transCol, staplewex = 0, outline = F,
          at = 3*(i-1), yaxt = "n")
  boxplot(summ2$propNeither[summ2$taxa == tax], add =T, col = nonCol, staplewex = 0, outline = F,
          at = 3*(i-1)+.5, yaxt = "n")
  boxplot(summ2$propCore[summ2$taxa == tax], add =T, col = coreCol, staplewex = 0, outline = F,
          at = 3*(i-1)+1, yaxt = "n")
}
text(3 *(1:9) - 2.5, par("usr")[3], uniqTaxa, srt = 45, xpd = T, cex = 1, adj = c(1.1,1.1)) 
rect(.5, 0.9, 1.5, 1.0, col = transCol, border=F)
rect(6.5, 0.9, 7.5, 1.0, col = nonCol, border=F)  
rect(12.5, 0.9, 13.5, 1.0, col = coreCol, border=F)  
text(c(3.4, 9, 14.5), c(0.95, 0.95, 0.95), c('Transient', 'Neither', 'Core'), cex = 1.5)
dev.off()


##################################################
# Merge taxa color and symbol codes into summary data
summ3 = merge(summ2, taxcolors, by = 'taxa', all.x = T)
summ3$color = as.character(summ3$color)
notbbs = subset(summ3, datasetID != 1)
bbssumm = subset(summ3, datasetID == 1)



######################################################################################### legend not right
# Summarizing datasets based on beta distribution parameters
pdf('output/plots/alpha_vs_beta.pdf', height = 6, width = 8)
par(mfrow = c(1,1), mar = c(5,5,1,1), mgp = c(3,1, 0), cex.axis = 1, cex.lab = 1, las = 1)
plot(summ3$alpha, summ3$beta, type = "n", xlim = c(-0.25,3.5), xlab = "alpha", ylab = "beta",
     ylim = c(0,4), yaxt = "n")
axis(2, 0:4, cex = 1)
points(summ3$alpha, summ3$beta, pch = summ3$pch, col = summ3$color, font = 5, cex = 1)
abline(a=0, b=1, lty = 'dotted', lwd = 4)
rect(-1, -1, 1, 1, lty = 'dashed', lwd = 2)
legend('topleft', legend = unique(summ$taxa), pch = symbols7, 
       col = c(colors7, 'white', colors7), pt.cex = 1.5, cex = 1.25)
points(-.28, 2.65, pch = symbols7[6], font = 5, col = colors7[6], cex = 1.7)
text(3,3.2, substitute(paste(alpha, " = ", beta)), srt = 40, cex = 2)
dev.off()

# Tally fraction of sites in each of 3 groups
# Fraction bimodal
nrow(summ3[summ3$alpha < 1 & summ3$beta < 1,])/nrow(summ3)
# Fraction core-dominated
nrow(summ3[summ3$alpha >= 1 & summ3$beta < summ3$alpha,])/nrow(summ3)
nrow(summ3[summ3$beta >= 1 & summ3$beta > summ3$alpha,])/nrow(summ3)

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
summ3$taxa = with(summ3, reorder(taxa, bimodality, function(x) mean(x, na.rm = T)))
boxplot(summ3$bimodality ~ summ3$taxa, cex.axis = 1.25, ylab = "Bimodality", boxwex = .6)
summ4 = summ3
summ4$taxa = with(summ4, reorder(taxa, pBimodal, function(x) mean(x, na.rm = T)))
boxplot(summ4$pBimodal ~ summ4$taxa, cex.axis = 1.25, ylab = "p (Bimodal)", boxwex = .6)




#########################################################
# Scale analyses with BBS data compared to other datasets

counts5 = read.csv('data/raw_datasets/dataset_1_full.csv', header=T)
occupancy.matrix = as.matrix(
  read.csv('scripts/R-scripts/scale_analysis/occ_matrix_BBS.csv', header=T, row.names = 1))
fifty = read.csv('scripts/R-scripts/scale_analysis/BBS_fiftystop_MD_CO_CA_OR_1996-2010.csv')
routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')
routes$stateroute = 1000*routes$statenum + routes$Route

# All-BBS scale:
uniqSpYr = unique(counts5[, c('Year', 'Aou')])
BBS.occ = data.frame(table(uniqSpYr$Aou)/15)

# MD BBS data
md.counts = subset(counts5, statenum==46)
md.occ.mat = occupancy.matrix[floor(as.numeric(row.names(occupancy.matrix))/1000)==46,]
md.uniq = unique(md.counts[,c('Year','Aou')])
# MD statewide temporal occupancy (27 routes)
md.occ = data.frame(table(md.uniq$Aou)/15)

#Scale of 10 BBS point count stops (specifically stops 1-10)
md10 = unique(md.counts[md.counts$Count10!=0,c('stateroute','Year','Aou')])
md10.rt.occ = data.frame(table(md10[,c('stateroute','Aou')])/15)
md10.rt.occ2 = md10.rt.occ[md10.rt.occ$Freq!=0,]

# Scale of 1 BBS point count stop
fiftyMD1 = subset(fifty, stateroute %in% unique(md10.rt.occ$stateroute) & year > 1995 & year < 2011 & Stop1!=0, 
                  select = c('stateroute','year','AOU','Stop1'))
md1.rt.occ = data.frame(table(fiftyMD1[,c('stateroute','AOU')])/15)
md1.rt.occ2 = md1.rt.occ[md1.rt.occ$Freq!=0,]

########################################################################################

#density plots
par(mfrow=c(1,1), mgp = c(2,1,0), mar = c(4,4,1,1))
col1 = 'darkblue'
col2 = 'blue'
col3 = colors()[128]
col4 = colors()[431]

# 5 scales, sequentially adding layers
# BBS route
pdf('output/plots/occupancy_vs_scale_BBS1.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 3), mgp = c(4, 1, 0), 
    oma = c(0,0,3,0), cex.axis = 1.5, cex.lab = 2, las = 1)
plot(density(md.occ.mat[!is.na(md.occ.mat)]), main="", xlab = "Temporal Occupancy", ylab = "Density", 
     col=col2, ylim = c(0, 5.5), xlim = c(-.1, 1.1), lwd = 4)
dev.off()

# BBS route + 10 stops
pdf('output/plots/occupancy_vs_scale_BBS2.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 3), mgp = c(4, 1, 0), 
    oma = c(0,0,3,0), cex.axis = 1.5, cex.lab = 2, las = 1)
plot(density(md.occ.mat[!is.na(md.occ.mat)]), main="", xlab = "Temporal Occupancy", ylab = "Density", 
     col=col2, ylim = c(0, 5.5), xlim = c(-.1, 1.1), lwd = 4)
points(density(md10.rt.occ2$Freq), col=col3, type='l', lwd = 4)
dev.off()

# BBS route + 10 stops + 1 stop
pdf('output/plots/occupancy_vs_scale_BBS3.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 3), mgp = c(4, 1, 0), 
    oma = c(0,0,3,0), cex.axis = 1.5, cex.lab = 2, las = 1)
plot(density(md.occ.mat[!is.na(md.occ.mat)]), main="", xlab = "Temporal Occupancy", ylab = "Density", 
     col=col2, ylim = c(0, 5.5), xlim = c(-.1, 1.1), lwd = 4)
points(density(md10.rt.occ2$Freq), col=col3, type='l', lwd = 4)
points(density(md1.rt.occ2$Freq), col=col4, type='l', lwd = 4)
dev.off()

# BBS route + 10 stops + 1 stop + MD
pdf('output/plots/occupancy_vs_scale_BBS4.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 3), mgp = c(4, 1, 0), 
    oma = c(0,0,3,0), cex.axis = 1.5, cex.lab = 2, las = 1)
plot(density(md.occ.mat[!is.na(md.occ.mat)]), main="", xlab = "Temporal Occupancy", ylab = "Density", 
     col=col2, ylim = c(0, 5.5), xlim = c(-.1, 1.1), lwd = 4)
points(density(md10.rt.occ2$Freq), col=col3, type='l', lwd = 4)
points(density(md1.rt.occ2$Freq), col=col4, type='l', lwd = 4)
points(density(md.occ$Freq), col = col1, type = 'l', lwd = 4)
dev.off()

# BBS route + 10 stops + 1 stop + MD + all BBS
pdf('output/plots/occupancy_vs_scale_BBS5.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 3), mgp = c(4, 1, 0), 
    oma = c(0,0,3,0), cex.axis = 1.5, cex.lab = 2, las = 1)
plot(density(md.occ.mat[!is.na(md.occ.mat)]), main="", xlab = "Temporal Occupancy", ylab = "Density", 
     col=col2, ylim = c(0, 5.5), xlim = c(-.1, 1.1), lwd = 4)
points(density(md10.rt.occ2$Freq), col=col3, type='l', lwd = 4)
points(density(md1.rt.occ2$Freq), col=col4, type='l', lwd = 4)
points(density(md.occ$Freq), col = col1, type = 'l', lwd = 4)
points(density(BBS.occ$Freq), type = 'l', lwd = 4, lty = 'dashed')
legend('topleft',
       c('United States (497 BBS routes)', 'Maryland (27 BBS routes)','Single BBS route (50 stops)','10 point count stops','1 point count stop'),
       col = c('black', col1, col2, col3, col4), lty = c('dashed', rep('solid', 4)), cex = 1.25, lwd = 4)
dev.off()

# BBS route + 10 stops + 1 stop + MD + all BBS, No legend
pdf('output/plots/occupancy_vs_scale_BBS5_nolegend.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 3), mgp = c(4, 1, 0), 
    oma = c(0,0,3,0), cex.axis = 1.5, cex.lab = 2, las = 1)
plot(density(md.occ.mat[!is.na(md.occ.mat)]), main="", xlab = "Temporal Occupancy", ylab = "Density", 
     col=col2, ylim = c(0, 5.5), xlim = c(-.1, 1.1), lwd = 4)
points(density(md10.rt.occ2$Freq), col=col3, type='l', lwd = 4)
points(density(md1.rt.occ2$Freq), col=col4, type='l', lwd = 4)
points(density(md.occ$Freq), col = col1, type = 'l', lwd = 4)
points(density(BBS.occ$Freq), type = 'l', lwd = 4, lty = 'dashed')
dev.off()



####################################################################################
# Mean occupancy values at each scale
meanOcc.allBBS = mean(BBS.occ$Freq)
meanOcc.MD = mean(md.occ$Freq)
meanOcc.singleBBS = mean(md.occ.mat[!is.na(md.occ.mat)])
meanOcc.MD10 = mean(md10.rt.occ2$Freq)
meanOcc.MD01 = mean(md1.rt.occ2$Freq)

meanOcc = c(meanOcc.allBBS, meanOcc.MD, meanOcc.singleBBS, meanOcc.MD10, meanOcc.MD01)


####################################################################################
# Mean spatial scale of each of the BBS scales
scale.1stop = pi*0.4^2 # in square kilometers
meanSpatScale = c(scale.1stop*50*497, #area of 497 BBS routes
                  scale.1stop*50*27,  #area of 27 BBS routes (in MD)
                  scale.1stop*50,     #area of 1 BBS route
                  scale.1stop*10,     #area of 10 point count stops
                  scale.1stop)        #area of 1 point count stop

pdf('output/plots/occ_vs_spatialScale_BBS.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 1), mgp = c(4, 1, 0), 
    cex.axis = 1.5, cex.lab = 2, las = 1)
plot(log10(meanSpatScale), meanOcc, 
     xlab = expression(paste(plain(log)[10]," Spatial scale (", plain(km)^2, ")")), 
     ylab = 'Mean occupancy', pch = 16, col = c('white', col1, col2, col3, col4), 
     cex = 4, ylim = c(0.2, 1.15), xlim = c(-1,4))
legend('topleft',
       c('Maryland (27 BBS routes)',
         'Single BBS route (50 stops)','10 point count stops','1 point count stop'),
       col = c(col1, col2, col3, col4), pch = 16, cex = 1.5, pt.cex = 2)
dev.off()

#####################################################################################
# Get mean community size (per year) for each of the above scales of BBS data
numMDroutes = length(unique(md.counts$stateroute))

meanN.allBBS = sum(counts5$SpeciesTotal)/15
meanN.MD = sum(md.counts$SpeciesTotal)/15
meanN.singleBBS = meanN.MD/numMDroutes
meanN.MD10 = sum(md.counts$Count10)/15/numMDroutes
meanN.MD01 = sum(fiftyMD1$Stop1)/15/numMDroutes

meanN = c(meanN.allBBS, meanN.MD, meanN.singleBBS, meanN.MD10, meanN.MD01)

# Regression not including all BBS scale
BBS.lm = lm(meanOcc[2:5] ~ log10(meanN[2:5]))

# Mean occupancy vs community size for BBS
pdf('output/plots/occ_vs_communitySize_BBS.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 1), mgp = c(4, 1, 0), 
    cex.axis = 1.5, cex.lab = 2, las = 1)
plot(log10(meanN), meanOcc, 
     xlab = expression(paste(plain(log)[10], " Community size")), 
     ylab = 'Mean occupancy', pch = 16, col = c('white', col1, col2, col3, col4), 
     cex = 4, ylim = c(0.2, 1.15), xlim = c(0.8,5))
lines(range(log10(meanN[2:5])), range(log10(meanN[2:5]))*BBS.lm$coefficients[2] + BBS.lm$coefficients[1],
      lwd = 4, lty = 'dashed')
legend('topleft',
       c('Maryland (27 BBS routes)',
         'Single BBS route (50 stops)','10 point count stops','1 point count stop'),
       col = c(col1, col2, col3, col4), pch = 16, cex = 1.5, pt.cex = 2)
dev.off()



# Dataset-level means of community abundance and occupancy
datasetMeanN = aggregate(summ3$meanAbundance, by = list(summ3$datasetID), mean)
datasetMeanOcc = aggregate(summ3$mu, by = list(summ3$datasetID), mean)
datasetMean = merge(datasetMeanN, datasetMeanOcc, by = 'Group.1', all = T)
names(datasetMean) = c('datasetID', 'meanN', 'meanOcc') 
datasetMean = merge(datasetMean, unique(summ3[, c('datasetID', 'taxa', 'color', 'pch')]), 
                    by = 'datasetID')

# Dataset level linear relationship between log10 size and occupancy by taxa
mean.lm = lm(meanOcc ~ log10(meanN) + taxa + taxa*log10(meanN), data = datasetMean)

# Arthropods are the baseline group so they need to be dealt with separately,
# but other taxa can be plotted using the function below.
arthXrange = log10(range(datasetMean$meanN[datasetMean$taxa == 'Arthropod']))
arthYpred = mean.lm$coefficients[1] + arthXrange * mean.lm$coefficients[2]

plotRegLine = function(data, lmObject, taxon) {
  xrange = log10(range(data$meanN[data$taxa == taxon]))
  ypred = lmObject$coefficients[1] + 
          lmObject$coefficients[paste("taxa", taxon, sep = "")] +
          (lmObject$coefficients[2] +
             lmObject$coefficients[paste("log10(meanN):taxa", taxon, sep = "")])*
          xrange
  lines(xrange, ypred, lwd = 3, col = as.character(data$color[data$taxa == taxon][1]))
}



# Plot dataset level means along with BBS
pdf('output/plots/occ_vs_communitySize_byDataset_noLines.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 1), mgp = c(4, 1, 0), 
    cex.axis = 1.5, cex.lab = 2, las = 1)
plot(log10(meanN), meanOcc, xlab = expression(paste(plain(log)[10]," Community Size")), 
     ylab = 'Mean occupancy', pch = 16, col = c('black', col1, col2, col3, col4), 
     cex = 4, ylim = c(0.2, 1.15), xlim = c(.8,5))
lines(range(log10(meanN[2:5])), range(log10(meanN[2:5]))*BBS.lm$coefficients[2] + BBS.lm$coefficients[1],
      lwd = 4, lty = 'dashed')
points(log10(meanN), meanOcc, pch = 16, col = c('black', col1, col2, col3, col4), cex = 4)

points(log10(datasetMean$meanN), datasetMean$meanOcc, pch = datasetMean$pch, 
       cex = 3, col = datasetMean$color, font = 5)
legend('topleft', legend = unique(summ$taxa), pch = symbols7, 
       col = c(colors7[1:5], 'white', colors7[7]), pt.cex = 2, cex = 1.5)
points(0.79, 0.79, pch = symbols7[6], font = 5, col = colors7[6], cex = 2)
dev.off()


# Plot dataset level means along with BBS (with regression lines)
pdf('output/plots/occ_vs_communitySize_byDataset_withLines.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 1), mgp = c(4, 1, 0), 
    cex.axis = 1.5, cex.lab = 2, las = 1)
plot(log10(meanN), meanOcc, xlab = expression(paste(plain(log)[10]," Community Size")), 
     ylab = 'Mean occupancy', pch = 16, col = c('black', col1, col2, col3, col4), 
     cex = 4, ylim = c(0.2, 1.15), xlim = c(.8,5))
lines(range(log10(meanN[2:5])), range(log10(meanN[2:5]))*BBS.lm$coefficients[2] + BBS.lm$coefficients[1],
      lwd = 4, lty = 'dashed')
points(log10(meanN), meanOcc, pch = 16, col = c('black', col1, col2, col3, col4), cex = 4)

points(log10(datasetMean$meanN), datasetMean$meanOcc, pch = datasetMean$pch, 
       cex = 3, col = datasetMean$color, font = 5)
# Plot regression lines
lines(arthXrange, arthYpred, lwd = 3, 
      col = as.character(datasetMean$color[datasetMean$taxa == 'Arthropod'][1]))
for (s in unique(datasetMean$taxa)) {
  plotRegLine(datasetMean, mean.lm, s)
}
legend('topleft', legend = unique(summ$taxa), pch = symbols7, 
       col = c(colors7[1:5], 'white', colors7[7]), pt.cex = 2, cex = 1.5)
points(0.79, 0.79, pch = symbols7[6], font = 5, col = colors7[6], cex = 2)
dev.off()


# Plot occupancy vs community size across datasets, illustrating within BBS variation
pdf('output/plots/occ_vs_communitySize_byDataset_withinBBSvariation.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 1), mgp = c(4, 1, 0), 
    cex.axis = 1.5, cex.lab = 2, las = 1)
plot(log10(meanN), meanOcc, xlab = expression(paste(plain(log)[10]," Community Size")), 
     ylab = 'Mean occupancy', pch = 16, col = c('black', col1, col2, col3, col4), 
     cex = 1.5, ylim = c(0.2, 1.15), xlim = c(.8,5))
lines(range(log10(meanN[2:5])), range(log10(meanN[2:5]))*BBS.lm$coefficients[2] + BBS.lm$coefficients[1],
      lwd = 4, lty = 'dashed')
points(log10(meanN), meanOcc, pch = 16, col = c('black', col1, col2, col3, col4), cex = 1.5)

points(log10(datasetMean$meanN), datasetMean$meanOcc, pch = datasetMean$pch, 
       cex = 1.5, col = 'gray80', font = 5)
#Add BBS points
points(log10(bbssumm$meanAbundance), bbssumm$mu, pch = 16, cex = 1, 
       col = as.character(taxcolors$color[taxcolors$taxa == "Bird"]))
legend('topleft', legend = unique(summ$taxa), pch = symbols7, 
       col = c(colors7[1], rep('gray80', 4), 'white', 'gray80'), pt.cex = 2, cex = 1.5)
points(0.79, 0.79, pch = symbols7[6], font = 5, col = 'gray80', cex = 2)
dev.off()






# Get summary data for ALL SITES of all other datasets
pdf('output/plots/occ_vs_communitySize_allSites.pdf', height = 6, width = 7.5)
par(mfrow = c(1, 1), mar = c(6, 6, 1, 1), mgp = c(4, 1, 0), 
    cex.axis = 1.5, cex.lab = 2, las = 1)
plot(log10(meanN), meanOcc, xlab = expression(paste(plain(log)[10]," Community Size")), 
     ylab = 'Mean occupancy', pch = 16, col = c('black', col1, col2, col3, col4), 
     cex = 4, ylim = c(0.15, 1.15), xlim = c(.8,5))
points(log10(bbssumm$meanAbundance), bbssumm$mu, pch = 16, cex = 1, col = colors7[1])
points(log10(meanN), meanOcc, pch = 16, col = c('black', col1, col2, col3, col4), cex = 1)

points(log10(notbbs$meanAbundance), notbbs$mu, pch = notbbs$pch, cex = 1, col = notbbs$color, font = 5)
legend('topleft', legend = unique(summ$taxa), pch = symbols7, 
       col = c(colors7, 'white', colors7), pt.cex = 1, cex = 1)
points(0.78, 0.78, pch = symbols7[6], font = 5, col = colors7[6], cex = 1)
dev.off()


##########################################################################
# Explaining variation in mean occupancy within BBS

env = read.csv('data/raw_datasets/dataset_1RAW/env_data.csv')
bbsumm = merge(bbssumm, env, by.x = 'site', by.y = 'stateroute')

par(mfrow = c(2,1), mar = c(6, 4, 1, 1), mgp = c(3, 1, 0), 
    oma = c(0, 4, 0, 0), las = 1, cex.axis = 1.5, cex.lab = 2)
plot(bbsumm$sum.NDVI.mean, bbsumm$mu, xlab = "NDVI", ylab = "", pch = 16, col = 'gray40')
lm.ndvi = lm(mu ~ sum.NDVI.mean, data = bbsumm)
abline(lm.ndvi, col = 'red', lty = 'dashed', lwd = 4)
text(0.25, 0.85, bquote(R^2 ~ "=" ~ .(round(summary(lm.ndvi)$r.squared, 2))), cex = 1.5)
plot(bbsumm$elev.mean, bbsumm$mu, xlab = "Elevation (m)", ylab = "", pch = 16, col = 'gray40')
lm.elev = lm(mu ~ elev.mean, data = bbsumm)
abline(lm.elev, col = 'red', lty = 'dashed', lwd = 4)
text(2600, 0.85, bquote(R^2 ~ "=" ~ .(round(summary(lm.elev)$r.squared, 2))), cex = 1.5)
mtext("Mean occupancy", 2, outer = T, cex = 2, las = 0)


#####################################################
# Summary figure(s) showing site level density estimates

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE) )
  do.call(rbind, tables)
}

site_data <- read.csv("output/tabular_data/core-transient_summary.csv")
sp_data <- load_data("data/propOcc_datasets/")
dataset_taxa_link <- site_data %>%
  select(datasetID, system, taxa) %>%
  distinct()
sp_data_full <- inner_join(sp_data, dataset_taxa_link)

densities_by_taxa_gaus <- ggplot(data = sp_data_full, aes(x = propOcc, group = site)) +
  geom_line(stat="density", alpha=0.1, size = 2) +
  facet_wrap(~taxa, scales = "free") +
  scale_y_sqrt()

densities_by_taxa_rect <- ggplot(data = sp_data_full, aes(x = propOcc, group = site)) +
  stat_density(kernel = "rectangular", geom = "line", position = "identity", alpha = 0.1, size = 2) +
  facet_wrap(~taxa, scales = "free") +
  scale_y_sqrt()

ggsave("output/plots/densitites_by_taxa_gaus.png", densities_by_taxa_gaus)
ggsave("output/plots/densitites_by_taxa_rect.png", densities_by_taxa_rect)

###############################################################
# Alpha vs Beta comparison by taxa

alpha_beta_by_taxa <- ggplot(data = site_data, aes(x = alpha, y = beta, color = datasetID)) +
  geom_point() +
  facet_wrap(~taxa) +
  scale_x_log10() +
  scale_y_log10()

ggsave("output/plots/alpha_beta_by_taxa.png", alpha_beta_by_taxa)

###############################################################
# Violin plots of proportions of core and transitient by taxa

stacked_site_data <- site_data %>%
  select(datasetID, site, system, taxa, propCore, propTrans) %>%
  mutate(propNeither = 1 - propCore - propTrans) %>%
  gather(key = sp_category, value = prop, propTrans, propNeither, propCore)

core_trans_prop_violins <- ggplot(stacked_site_data, aes(x = sp_category, y = prop, color = sp_category)) +
  geom_violin() +
  facet_wrap(~taxa)

ggsave("output/plots/core_trans_prop_violins.png")


#################################################################
# Map of each community - a few for now
pdf('fig1map.pdf', height = 8, width = 10)
par(pin = c(7, 9))

world = map(database='world')

# merge in lat/long
latlongs = read.csv('data_formatting_table.csv', header = T)
plotdata_all = merge(dsets, latlongs, by.x = "datasetID", by.y = "dataset_ID")
plotdata_all$Lat = plotdata_all$CentralLatitude
plotdata_all$Lon = plotdata_all$CentralLongitude
plotdata_sub = plotdata_all[,c(1, 3, 54, 55)]
# select(plotdata_all$datasetID, plotdata_all$Lat, plotdata_all$Lon, plotdata_all$taxa.x)
##### hide messy lat long cleaning #####
plot1 = read.csv("data/latlongs/bbs_96-10_latlongs.csv", header = T)
plot1$Lat = plot1$Lati
plot1$Lon = plot1$Longi
plot1$datasetID = 1
plot1$taxa.x = 'Bird'
plot1_sub = plot1[,c(7:10)]

plot247 = read.csv("data/latlongs/d247_latlongs_unique.csv", header = T)
plot247$Lat = plot247$lat
plot247$Lon = plot247$long
plot247$datasetID = 247
plot247$taxa.x = 'Arthopod'
plot247_sub = plot247[,c(4:7)]

plot248 = read.csv("data/latlongs/d248_latlongs.csv", header = T)
plot248$Lat = plot248$lat
plot248$Lon = plot248$long
plot248$datasetID = 248
plot248$taxa.x = 'Fish'
plot248_sub = plot248[,c(4:7)]

plot269 = read.csv("data/latlongs/d269_latlongs.csv", header = T)
plot269$Lat = plot269$lat
plot269$Lon = plot269$long
plot269$datasetID = 269
plot269$taxa.x = 'Bird'
plot269_sub = plot269[,c(4:7)]

plot289 = read.csv("data/latlongs/d289_latlongs.csv", header = T)
plot289$Lat = plot289$Latitude
plot289$Lon = plot289$Longitude
plot289$datasetID = 289
plot289$taxa.x = 'Plant'
plot289_sub = plot289[,c(10:13)]

plot308 = read.csv("data/latlongs/d308_latlongs.csv", header = T)
plot308$Lat = plot308$RouteLattitude
plot308$Lon = plot308$RouteLongitude
plot308$datasetID = 308
plot308$taxa.x = 'Bird'
plot308_sub = plot308[,c(12:15)]

plot309 = read.csv("data/latlongs/d309_latlongs.csv", header = T)
plot309$Lat = plot309$lat
plot309$Lon = plot309$long
plot309$datasetID = 309
plot309$taxa.x = 'Fish'
plot309_sub = plot309[,c(4:7)]

plot315 = read.csv("data/latlongs/d315_latlongs.csv", header = T)
plot315$Lat = plot315$lat
plot315$Lon = plot315$long
plot315$datasetID = 315
plot315$taxa.x = 'Arthropod'
plot315_sub = plot315[,c(5:8)]
#### end messy cleaning ####
# merge all together
alldata_latlong = rbind(plotdata_sub, plot1_sub, plot247_sub, plot248_sub, plot269_sub, plot289_sub, plot308_sub, plot309_sub, plot315_sub)

points(alldata_latlong$Lon, alldata_latlong$Lat, pch = 20, col = as.integer(alldata_latlong$taxa.x))

points(plot248$long, plot248$lat, col = "green", pch = 20)
points(plot269$long, plot269$lat, col = "blue", pch = 20)
points(plot289$Longitude, plot289$Latitude, col = "cyan", pch = 20) 
points(plot308$RouteLongitude, plot308$RouteLattitude, col = "brown", pch = 20)
points(plot309$long, plot309$lat, col = "gold", pch = 20)
points(plot315$long, plot315$lat, col = "black", pch = 20)
points(plot247$long, plot247$lat, col = "purple", pch = 20)

out.file<-"output/tabular_data/fig1map.pdf"

