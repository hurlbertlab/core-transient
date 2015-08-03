#Load Jes Coyle's analysis workspace for the Coyle et al. 2013 paper (doi: 10.1086/669903).
#Calculate temporal occupancy distributions at 3 different spatial scales for
#grant proposal preliminary analysis: 1) scale of 10 BBS stops, 
# 2) BBS route, 3) aggregate of 27 BBS routes within state of MD.

library(raster)
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
            rgb(98/255, 83/255, 108/255),
            rgb(120/255, 21/255, 21/255),
            rgb(171/255, 167/255, 46/255),
           
            rgb(0, 54/255, 117/255),
            
            rgb(86/255, 141/255, 27/255),
            rgb(186/255, 103/255, 30/255))

symbols7 = c(16:18,15, 17, 16,18)

taxcolors = data.frame(taxa = unique(summ$taxa), color = colors7, pch = symbols7)
summ2 = merge(summ, taxcolors, by = 'taxa', all.x = T)
summ2$color = as.character(summ2$color)
summ3 = subset(summ2, datasetID != 1)

par(mar = c(6, 6, 1, 1), las = 1)
plot(log10(meanN), meanOcc, xlab = expression(paste(plain(log)[10]," Community Size")), 
     ylab = 'Mean occupancy', pch = 16, col = c('black', col1, col2, col3, col4), 
     cex = 4, ylim = c(0.1, 1.15), xlim = c(.8,5))

points(log10(summ3$meanAbundance), summ3$mu, pch = summ3$pch, cex = 2, col = summ3$color)
legend('topleft', legend = unique(summ$taxa), pch = symbols7, col = colors7, pt.cex = 2)









# Panel b - occupancy vs community size
plot(log10(occsize$N), occsize$meanocc, xlab = expression(paste(plain(log)[10]," Community Size")), ylab = 'Mean occupancy', 
     pch = occsize$pch, col = occsize$col, cex = 3, ylim = c(0.2, 1.05), xlim = c(.8,5))
text(log10(occsize$N) + occsize$offset.x, occsize$meanocc + .08*occsize$offset.y, occsize$Organism, col = occsize$col, cex = 2)
text(log10(occsize$N), occsize$meanocc + .03*occsize$offset.y, occsize$scale, col = occsize$col, cex = 1.5)
mtext("(b)", 3, outer = T, adj = 0.5, cex = 3)
dev.off()









pdf('output/plots/occupancy_vs_scale.pdf',height=6,width=15)
par(mfrow = c(1, 2), mar = c(6, 6, 1, 3), mgp = c(4, 1, 0), 
    oma = c(0,0,3,0), cex.axis = 2, cex.lab = 3, las = 1)
# Panel a - kernel density estimates of occupancy for 4 scales of bird data
plot(density(md.occ$Freq), main="", xlab = "Temporal Occupancy", ylab = "Density", 
     col=col1, ylim = c(0, 4.5), lwd = 4)
points(density(md.occ.mat[!is.na(md.occ.mat)]), col=col2, type='l', lwd = 4)
points(density(md10.rt.occ2$Freq), col=col3, type='l', lwd = 4)
points(density(md1.rt.occ2$Freq), col=col4, type='l', lwd = 4)

legend('topleft',
       c('Maryland (27 BBS routes)','Single BBS route (50 stops)','10 point count stops','1 point count stop'),
       col = c(col1,col2,col3,col4), cex = 1.5, lwd = 4)
#mtext("(a)", 3, outer = T, adj = 0, cex = 3)


col5 = 'darkred'
col6 = 'red'
col7 = colors()[527]
col8 = colors()[421]

# Panel (b) - CA/OR
plot(density(ca.occ$Freq), main="", xlab = "Temporal Occupancy", ylab = "Density", 
     col=col5, ylim = c(0, 4.5), lwd = 4)
points(density(ca.occ.mat[!is.na(ca.occ.mat)]), col=col6, type='l', lwd = 4)
points(density(ca10.rt.occ2$Freq), col=col7, type='l', lwd = 4)
points(density(ca1.rt.occ2$Freq), col=col8, type='l', lwd = 4)
text(0.2, 4.3,'California/Oregon', cex = 1.5)
dev.off()




















