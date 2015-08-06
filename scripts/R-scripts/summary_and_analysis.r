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
summ4 = subset(summ, !datasetID %in% c(99, 85, 90, 91, 92, 97, 124))
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


fiftyMD1 = subset(fifty, stateroute %in% unique(md10.rt.occ$stateroute) & year > 1995 & year < 2011 & Stop1!=0, 
                  select = c('stateroute','year','AOU','Stop1'))
md1.rt.occ = data.frame(table(fiftyMD1[,c('stateroute','AOU')])/15)
md1.rt.occ2 = md1.rt.occ[md1.rt.occ$Freq!=0,]


# OTHER REGIONS ##########################################################################

# CA/OR BBS data
ca.counts = subset(counts5, statenum==14 | statenum == 69)
ca.occ.mat = occupancy.matrix[floor(as.numeric(row.names(occupancy.matrix))/1000)==14 |
                                floor(as.numeric(row.names(occupancy.matrix))/1000)==69,]
ca.uniq = unique(ca.counts[,c('Year','Aou')])
# CA/OR statewide temporal occupancy (27 routes)
ca.occ = data.frame(table(ca.uniq$Aou)/15)

#Scale of 10 BBS point count stops (specifically stops 1-10)
ca10 = unique(ca.counts[ca.counts$Count10!=0,c('stateroute','Year','Aou')])
ca10.rt.occ = data.frame(table(ca10[,c('stateroute','Aou')])/15)
ca10.rt.occ2 = ca10.rt.occ[ca10.rt.occ$Freq!=0,]

# Scale of 1 BBS point count stop
fiftyca1 = subset(fifty, stateroute %in% unique(ca10.rt.occ$stateroute) & year > 1995 & year < 2011 & Stop1!=0, 
                  select = c('stateroute','year','AOU','Stop1'))
ca1.rt.occ = data.frame(table(fiftyca1[,c('stateroute','AOU')])/15)
ca1.rt.occ2 = ca1.rt.occ[ca1.rt.occ$Freq!=0,]



#Colorado route
routesCO = subset(routes, statenum == 17)
routesCO_SWcorner = subset(routesCO, Longi < -106 & Lati < 39)

co.counts = subset(counts5, stateroute %in% routesCO_SWcorner$stateroute)
co.occ.mat = occupancy.matrix[as.numeric(row.names(occupancy.matrix)) %in% routesCO_SWcorner$stateroute,]

co.uniq = unique(co.counts[,c('Year','Aou')])
co.occ = data.frame(table(co.uniq$Aou)/15)

#Scale of 10 BBS point count stops (specifically stops 1-10)
co10 = unique(co.counts[co.counts$Count10!=0,c('stateroute','Year','Aou')])
co10.rt.occ = data.frame(table(co10[,c('stateroute','Aou')])/15)
co10.rt.occ2 = co10.rt.occ[co10.rt.occ$Freq!=0,]

#Scale of 1 BBS point count stop (specifically stop 1)

fiftyCO1 = subset(fifty, stateroute %in% unique(co10.rt.occ$stateroute) & year > 1995 & year < 2011 & Stop1!=0, 
                  select = c('stateroute','year','AOU','Stop1'))
co1.rt.occ = data.frame(table(fiftyCO1[,c('stateroute','AOU')])/15)
co1.rt.occ2 = co1.rt.occ[co1.rt.occ$Freq!=0,]

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
# Mean spatial scale of each of the BBS scales
scale.1stop = pi*0.4^2 # in square kilometers
meanSpatScale = c(scale.1stop*50*497, #area of 497 BBS routes
                  scale.1stop*50*27,  #area of 27 BBS routes (in MD)
                  scale.1stop*50,     #area of 1 BBS route
                  scale.1stop*10,     #area of 10 point count stops
                  scale.1stop)        #area of 1 point count stop
par(mar = c(6, 6, 1, 1), mgp = c(4, 1, 0), cex.lab = 2.5)
plot(log10(meanSpatScale), meanOcc, 
     xlab = expression(paste(plain(log)[10]," Spatial scale (", plain(km)^2, ")")), 
     ylab = 'Mean occupancy', pch = 16, col = c('white', col1, col2, col3, col4), 
     cex = 4, ylim = c(0.1, 1.15), xlim = c(-1,4))
legend('topleft',
       c('Maryland (27 BBS routes)',
         'Single BBS route (50 stops)','10 point count stops','1 point count stop'),
       col = c(col1, col2, col3, col4), pch = 16, cex = 1.5, pt.cex = 2)


#####################################################################################
# Get mean community size (per year) for each of the above scales of BBS data
numMDroutes = length(unique(md.counts$stateroute))

meanN.allBBS = sum(counts5$SpeciesTotal)/15
meanN.MD = sum(md.counts$SpeciesTotal)/15
meanN.singleBBS = meanN.MD/numMDroutes
meanN.MD10 = sum(md.counts$Count10)/15/numMDroutes
meanN.MD01 = sum(fiftyMD1$Stop1)/15/numMDroutes

meanN = c(meanN.allBBS, meanN.MD, meanN.singleBBS, meanN.MD10, meanN.MD01)

meanOcc.allBBS = mean(BBS.occ$Freq)
meanOcc.MD = mean(md.occ$Freq)
meanOcc.singleBBS = mean(md.occ.mat[!is.na(md.occ.mat)])
meanOcc.MD10 = mean(md10.rt.occ2$Freq)
meanOcc.MD01 = mean(md1.rt.occ2$Freq)

meanOcc = c(meanOcc.allBBS, meanOcc.MD, meanOcc.singleBBS, meanOcc.MD10, meanOcc.MD01)

# Get summary data for all other datasets
summ = read.csv('output/tabular_data/core-transient_summary.csv')

colors7 = c(rgb(29/255, 106/255, 155/255),
            colors()[612],
            colors()[552],
            colors()[144],
            rgb(0, 54/255, 117/255),
            rgb(86/255, 141/255, 27/255),
            colors()[547])

symbols7 = c(16:18,15, 17, 167,18)

taxcolors = data.frame(taxa = unique(summ$taxa), color = colors7, pch = symbols7)
summ2 = merge(summ4, taxcolors, by = 'taxa', all.x = T)
summ2$color = as.character(summ2$color)
summ3 = subset(summ2, !datasetID %in% c(1, 99))
bbssumm = subset(summ2, datasetID==1)

par(mar = c(6, 6, 1, 1), las = 1)
plot(log10(meanN), meanOcc, xlab = expression(paste(plain(log)[10]," Community Size")), 
     ylab = 'Mean occupancy', pch = 16, col = c('black', col1, col2, col3, col4), 
     cex = 4, ylim = c(0.1, 1.15), xlim = c(.8,5))
points(log10(bbssumm$meanAbundance), bbssumm$mu, pch = 16, cex = 2, col = colors7[1])
points(log10(meanN), meanOcc, pch = 16, col = c('black', col1, col2, col3, col4), cex = 4)

points(log10(summ3$meanAbundance), summ3$mu, pch = summ3$pch, cex = 2, col = summ3$color, font = 5)
legend('topleft', legend = unique(summ$taxa), pch = symbols7, 
       col = c(colors7[1:5], 'white', colors7[7]), pt.cex = 2, cex = 1.5)
points(0.76, 0.85, pch = symbols7[6], font = 5, col = colors7[6], cex = 2)



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







