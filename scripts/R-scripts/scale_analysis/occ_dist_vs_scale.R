#Load Jes Coyle's analysis workspace for the Coyle et al. 2013 paper (doi: 10.1086/669903).
#Calculate temporal occupancy distributions at 3 different spatial scales for
#grant proposal preliminary analysis: 1) scale of 10 BBS stops, 
# 2) BBS route, 3) aggregate of 27 BBS routes within state of MD.

library(raster)
counts5 = read.csv('data/raw_datasets/dataset_1_full.csv', header=T)
occupancy.matrix = as.matrix(
  read.csv('scripts/R-scripts/scale_analysis/occ_matrix_BBS.csv', header=T, row.names = 1))

md.counts = subset(counts5, statenum==46)
md.occ.mat = occupancy.matrix[floor(as.numeric(row.names(occupancy.matrix))/1000)==46,]

par(mfrow=c(5,6), mgp=c(2,1,1),mar=c(2,2,1,1))
for (i in 1:27){hist(md.occ.mat[i,],main="",xlab="",ylab="")}

#Statewide temporal occupancy
md.uniq = unique(md.counts[,c('Year','Aou')])
md.occ = data.frame(table(md.uniq$Aou)/15)
hist(md.occ$Freq,main="",xlab="",ylab="",col='red')

#Histogram of all occupancy values from all species on all routes (rough equivalent of mean
#occupancy distribution at the route scale)
hist(md.occ.mat,main="",xlab="",ylab="",col='blue')

#Scale of 10 BBS point count stops (specifically stops 1-10)
md10 = unique(md.counts[md.counts$Count10!=0,c('stateroute','Year','Aou')])
md10.rt.occ = data.frame(table(md10[,c('stateroute','Aou')])/15)
md10.rt.occ2 = md10.rt.occ[md10.rt.occ$Freq!=0,]
hist(md10.rt.occ2$Freq, main="", xlab="", ylab="", col = 'green')

#Scale of 1 BBS point count stop (specifically stop 1)
fifty = read.csv('scripts/R-scripts/scale_analysis/BBS_fiftystop_MD_CO_1996-2010.csv')
routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')

fiftyMD1 = subset(fifty, stateroute %in% unique(md10.rt.occ$stateroute) & year > 1995 & year < 2011 & Stop1!=0, 
                  select = c('stateroute','year','AOU','Stop1'))
md1.rt.occ = data.frame(table(fiftyMD1[,c('stateroute','AOU')])/15)
md1.rt.occ2 = md1.rt.occ[md1.rt.occ$Freq!=0,]

#density plots
par(mfrow=c(1,1), mgp = c(2,1,0), mar = c(4,4,1,1))
col1 = 'darkblue'
col2 = 'blue'
col3 = colors()[128]
col4 = colors()[431]

# Read in community size vs mean occupancy data for example datasets
setwd('//bioark.bio.unc.edu/hurlbertallen/proposals/coreoccasional/analyses/')
occsize = read.table('example_taxa_meanocc_vs_commsize.txt', header=T, sep='\t', as.is = c(1,5,6))

pdf('//bioark.bio.unc.edu/hurlbertallen/proposals/coreoccasional/analyses/occupancy_vs_scale.pdf',height=6,width=15)
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
mtext("(a)", 3, outer = T, adj = 0, cex = 3)

# Panel b - occupancy vs community size
plot(log10(occsize$N), occsize$meanocc, xlab = expression(paste(plain(log)[10]," Community Size")), ylab = 'Mean occupancy', 
     pch = occsize$pch, col = occsize$col, cex = 3, ylim = c(0.2, 1.05), xlim = c(.8,5))
text(log10(occsize$N) + occsize$offset.x, occsize$meanocc + .08*occsize$offset.y, occsize$Organism, col = occsize$col, cex = 2)
text(log10(occsize$N), occsize$meanocc + .03*occsize$offset.y, occsize$scale, col = occsize$col, cex = 1.5)
mtext("(b)", 3, outer = T, adj = 0.5, cex = 3)
dev.off()


# Mean number of individuals at each scale
MD.1stop = aggregate(fiftyMD1$Stop1, by = list(fiftyMD1$stateroute, fiftyMD1$year), sum)
meanN.1stop = mean(MD.1stop$x)
fiftyMD10 = subset(md.counts, stateroute %in% unique(md10.rt.occ$stateroute) & Year > 1995 & Year < 2011 & Count10!=0, 
                  select = c('stateroute','Year','Aou','Count10'))
MD.10stop = aggregate(fiftyMD10$Count10, by = list(fiftyMD10$stateroute, fiftyMD10$Year), sum)
meanN.10stop = mean(MD.10stop$x)
fiftyMD50 = subset(md.counts, stateroute %in% unique(md10.rt.occ$stateroute) & Year > 1995 & Year < 2011 & Count10!=0, 
                   select = c('stateroute','Year','Aou','SpeciesTotal'))
MD.50stop = aggregate(fiftyMD50$SpeciesTotal, by = list(fiftyMD50$stateroute, fiftyMD50$Year), sum)
meanN.50stop = mean(MD.50stop$x)
MD.27rtes = aggregate(md.counts$SpeciesTotal, by = list(md.counts$Year), sum)
meanN.27rtes = mean(MD.27rtes$x)