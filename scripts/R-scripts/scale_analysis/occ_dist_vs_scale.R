#Load Jes Coyle's analysis workspace for the Coyle et al. 2013 paper (doi: 10.1086/669903).
#Calculate temporal occupancy distributions at 3 different spatial scales for
#grant proposal preliminary analysis: 1) scale of 10 BBS stops, 
# 2) BBS route, 3) aggregate of 27 BBS routes within state of MD.
#pull in 50 stop data from ecoretriever
setwd("//bioark.ad.unc.edu/hurlbertlab/Databases/BBS/FiftyStopData")
bbs50 = read.csv("fiftystop_thru2010_goodspp_goodrtes.csv", header = TRUE)
#disregard ecodataretriever for now bc very buggy and BBS files incomplete 
#setwd("C:/Program Files (x86)/EcoDataRetriever")
#library(ecoretriever)
#ecoretriever::datasets()
#bbs50 = ecoretriever::fetch("BBS50")
#names(bbs50)
#bbs = bbs50$species
#bbsrts= bbs50$routes #Year columns missing from both datasets?
names(bbs50)
setwd("C:/git/core-transient")
#library(raster)
counts5 = read.csv('data/raw_datasets/dataset_1RAW/dataset_1_full.csv', header=T) #1996-2010 #in groups of ten #is this the full bbs dataset broken by 10 stops? 
#want to merge counts5 with bbs 50 stop data? why does counts5 have year data and not the others? 
occupancy.matrix = as.matrix(
  read.csv('scripts/R-scripts/scale_analysis/occ_matrix_BBS.csv', header=T, row.names = 1))
routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')
routes$stateroute = 1000*routes$statenum + routes$Route
names(routes)
names(counts5)

# Jes' All-BBS scale (50 pt count scale):
uniqSpYr = unique(counts5[, c('Year', 'Aou')])
BBS.occ = data.frame(table(uniqSpYr$Aou)/15)

# All-BBS scale Molly prototype (50 pt count scale):
bbs.occ.mat = occupancy.matrix[floor(as.numeric(row.names(occupancy.matrix))/1000),] #round down so no longer decimals 
bbs.uniq = unique(bbs50[,c('year','AOU')]) #AOU capitalized in 50 stop data, year columns not present in 50 stop data 
#can't get past this point with the bbs species data in the first place because missing year data 
#do I have to merge routes and spp before moving forward too?
bbs.occ = data.frame(table(bbs.uniq$AOU)/15)

###So for the whole dataset, 10 pt count stops: #we are only getting one out of five chunks along 
#want to estimate occupancy across each one, as of now only estimating for count 10 column 
#modify the below function 
#add a scale argument rather than hard coding the scale column 
#confirm whether or not raw data (50 stop original) differs in aou vs Aou vs AOU 
#figure out then how to group aggregating over multiple columns 
#fifty pt count data and then taking pts 1-5 and collapsing them all together 
occ_counts = function(countData, countColumn) {
  bbsu = unique(countData[countData[, countColumn]!= 0, c("stateroute", "year", "AOU")]) #problems start here
  #unique is calling unique rows in the dataframe for a given specified column that has any value that is not 0, 
  #and organizing the output generated from those rows by unique combo of stateroute, year, and AOU code
  #"unique" is also a command in the raster package, so rerunning code without raster package at beginning
  bbsu.rt.occ = data.frame(table(bbsu[,c("stateroute", "AOU")])/15) #do I change the 15 to reflect # years of bbs? 
  #1966-2010? 44 years
  bbsu.rt.occ2 = bbsu.rt.occ[bbsu.rt.occ$Freq!=0,]
  names(bbsu.rt.occ2)[3] = "occupancy"
  bbsu.rt.occ2$scale = 10
  bbsu.rt.occ2$subrouteID = countColumn
  bbsu.rt.occ2 = bbsu.rt.occ2[, c("stateroute", "scale", "subrouteID", "AOU", "occupancy")]
  return(bbsu.rt.occ2)
}
#state route stop, scale at which (1 or 10 stops etc), sub-route ID (if scale is 10 stops, it's count20), species, occupany
bbs1 = occ_counts(bbs50, "Stop10") 
#alternatively, http://stackoverflow.com/questions/2641653/pass-a-data-frame-column-name-to-a-function
#^correct solution; needed quotes around column name when executing function, function itself worked fine

#########
#trying to solve hard coding vs soft coding "scale" column issue
occ_counts = function(countData, countColumn, v) {
  bbsu = unique(countData[countData[, countColumn]!= 0, c("stateroute", "year", "AOU")])
  bbsu.rt.occ = data.frame(table(bbsu[,c("stateroute", "AOU")])/15)
  bbsu.rt.occ2 = bbsu.rt.occ[bbsu.rt.occ$Freq!=0,]
  names(bbsu.rt.occ2)[3] = "occupancy"
  bbsu.rt.occ2$subrouteID = countColumn
  bbsu.rt.occ2$scale = v 
  bbsu.rt.occ2 = bbsu.rt.occ2[, c("stateroute", "scale", "subrouteID", "AOU", "occupancy")]
  return(bbsu.rt.occ2)
}
#if subrouteID = Count10, Count20, etc == 10; if = Count25, == 25; if = Count1, == 1, but no need for "ifs"   
# modeled after: BBS_Core_agreement = 
  #sum(temp.site$occupancy>0.67 & temp.site$status=="breeder", na.rm = T)/sum(temp.site$occupancy>0.67, na.rm = T)
bbs1 = occ_counts(bbs50, "Stop10", 10) 
bbs2 = occ_counts(bbs50, "Stop25", 25)
#It works!!!!!!!!!!!!!!!!!!!!!!!!!


head(bbs1)
#scale of 1 pt count 
bbs1 = subset(fifty, stateroute %in% unique(bbs10.rt.occ$stateroute) & year > 1995 & year < 2011 & Stop1!=0, 
              select = c('stateroute','year','AOU','Stop1'))
bbs1.rt.occ = data.frame(table(bbs1[,c('stateroute','AOU')])/15)
bbs1.rt.occ2 = bbs1.rt.occ[bbs1.rt.occ$Freq!=0,]

#scale of 5 pt counts -> how to group? 
bbs5 = 
  unique(bbs1[bbs1$Count5!=0, c('stateroute', 'Year', 'Aou')])
bbs5.rt.occ = 
  data.frame(table(bbs5[,c('stateroute', 'Aou')])/15)
bbs5.rt.occ2 = bbs5.rt.occ[bbs10.rt.occ$Freq!=0,] 

#scale of 25 pt counts 
#Just halving the fifty stop data 

fiftybbs = subset(fifty, stateroute %in% unique(bbs1.rt.occ$stateroute) & year > 1995 & year < 2011 & Stop25!=0, 
                  select = c('stateroute','year','AOU','Stop1'))
bbs25.rt.occ = data.frame(table(fiftybbs[,c('stateroute','AOU')])/15)
bbs25.rt.occ2 = bbs25.rt.occ[bbs25.rt.occ$Freq!=0,]



###################################################################################
# MD BBS data
md.counts = subset(counts5, statenum==46) #sub to MD
md.occ.mat = occupancy.matrix[floor(as.numeric(row.names(occupancy.matrix))/1000)==46,]
md.uniq = unique(md.counts[,c('Year','Aou')])
# MD statewide temporal occupancy (27 routes)
md.occ = data.frame(table(md.uniq$Aou)/15)

#Scale of 10 BBS point count stops (specifically stops 1-10)
md10 = unique(md.counts[md.counts$Count10!=0,c('stateroute','Year','Aou')])
md10.rt.occ = data.frame(table(md10[,c('stateroute','Aou')])/15)
md10.rt.occ2 = md10.rt.occ[md10.rt.occ$Freq!=0,]

# Scale of 1 BBS point count stop #use to group in clumps of 5
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
summ2 = merge(summ, taxcolors, by = 'taxa', all.x = T)
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







