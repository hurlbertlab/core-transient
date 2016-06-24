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
bbs1 = occ_counts(bbs50, "Stop1", 1)
bbs2 = occ_counts(bbs50, "Stop2", 1) 
bbs3 = occ_counts(bbs50, "Stop3", 1)
bbs4 = occ_counts(bbs50, "Stop4", 1) 
bbs5 = occ_counts(bbs50, "Stop5", 1)
bbs6 = occ_counts(bbs50, "Stop6", 1) 
bbs7 = occ_counts(bbs50, "Stop7", 1)
bbs8 = occ_counts(bbs50, "Stop8", 1) 
bbs9 = occ_counts(bbs50, "Stop9", 1)
bbs10 = occ_counts(bbs50, "Stop10", 1) 
bbs11 = occ_counts(bbs50, "Stop11", 1)
bbs12 = occ_counts(bbs50, "Stop12", 1) 
bbs13 = occ_counts(bbs50, "Stop13", 1)
bbs14 = occ_counts(bbs50, "Stop14", 1) 
bbs15 = occ_counts(bbs50, "Stop15", 1)
bbs16 = occ_counts(bbs50, "Stop16", 1) 
bbs17 = occ_counts(bbs50, "Stop17", 1)
bbs18 = occ_counts(bbs50, "Stop18", 1) 
bbs19 = occ_counts(bbs50, "Stop19", 1)
bbs20 = occ_counts(bbs50, "Stop20", 1) 
bbs21 = occ_counts(bbs50, "Stop21", 1)
bbs22 = occ_counts(bbs50, "Stop22", 1) 
bbs23 = occ_counts(bbs50, "Stop23", 1)
bbs24 = occ_counts(bbs50, "Stop24", 1) 
bbs25 = occ_counts(bbs50, "Stop25", 1)
bbs26 = occ_counts(bbs50, "Stop26", 1) 
bbs27 = occ_counts(bbs50, "Stop27", 1)
bbs28 = occ_counts(bbs50, "Stop28", 1) 
bbs29 = occ_counts(bbs50, "Stop29", 1)
bbs30 = occ_counts(bbs50, "Stop30", 1) 
bbs31 = occ_counts(bbs50, "Stop31", 1)
bbs32 = occ_counts(bbs50, "Stop32", 1)
bbs33 = occ_counts(bbs50, "Stop33", 1) 
bbs34 = occ_counts(bbs50, "Stop34", 1)
bbs35 = occ_counts(bbs50, "Stop35", 1)
bbs36 = occ_counts(bbs50, "Stop36", 1) 
bbs37 = occ_counts(bbs50, "Stop37", 1)
bbs38 = occ_counts(bbs50, "Stop38", 1)
bbs39 = occ_counts(bbs50, "Stop39", 1) 
bbs40 = occ_counts(bbs50, "Stop40", 1)
bbs41 = occ_counts(bbs50, "Stop41", 1)
bbs42 = occ_counts(bbs50, "Stop42", 1)
bbs43 = occ_counts(bbs50, "Stop43", 1)
bbs44 = occ_counts(bbs50, "Stop44", 1)
bbs45 = occ_counts(bbs50, "Stop45", 1)
bbs46 = occ_counts(bbs50, "Stop46", 1)
bbs47 = occ_counts(bbs50, "Stop47", 1)
bbs48 = occ_counts(bbs50, "Stop48", 1)
bbs49 = occ_counts(bbs50, "Stop49", 1)
bbs50 = occ_counts(bbs50, "Stop50", 1)

#stitch all of these together into one dataframe 
bbsbound<- rbind(bbs1, bbs2, bbs3, bbs4, bbs5, bbs6, bbs7, bbs8, bbs9, bbs10, 
                 bbs11, bbs12, bbs13, bbs14, bbs15, bbs16, bbs17, bbs18, bbs19, bbs20,
                 bbs21, bbs22, bbs23, bbs24, bbs25, bbs26, bbs27, bbs28, bbs29, bbs30, 
                 bbs31, bbs32, bbs33, bbs34, bbs35, bbs36, bbs37, bbs38, bbs39, 
                 bbs40, bbs41, bbs42, bbs43, bbs44, bbs45, bbs46, bbs47, bbs48, bbs49, bbs50)

?rbind

#It works!!!!!!!!!!!!!!!!!!!!!!!!!
##clustering - do for all 50 stops 1 by one, scale = 1 
#then for every five, added together and grouped by fives, scale = 5 
#then for every 10, which are the fives doubled, scale = 10 
#then for every 25, which is just the fifty halved, scale = 25 
#we have already done the analysis for all 50, that was what we first started with, but we can re-run for clarity
bbs5bound_1<-rbind(bbs1, bbs2, bbs3, bbs4, bbs5) 
bbs5bound_1$subrouteID<-"1-5"
bbs5bound_1$scale<-"5"
bbs5bound_2<-rbind(bbs6, bbs7, bbs8, bbs9, bbs10)
bbs5bound_2$subrouteID<-"6-10"
bbs5bound_2$scale<-"5"
bbs5bound_3<-rbind(bbs11, bbs12, bbs13, bbs14, bbs15)
bbs5bound_3$subrouteID<-"11-15"
bbs5bound_3$scale<-"5"
bbs5bound_4<-rbind(bbs16, bbs17, bbs18, bbs19, bbs20)
bbs5bound_4$subrouteID<-"16-20"
bbs5bound_4$scale<-"5"
bbs5bound_5<-rbind(bbs21, bbs22, bbs23, bbs24, bbs25)
bbs5bound_5$subrouteID<-"21-25"
bbs5bound_5$scale<-"5"
bbs5bound_6<-rbind(bbs26, bbs27, bbs28, bbs29, bbs30)
bbs5bound_6$subrouteID<-"26-30"
bbs5bound_6$scale<-"5"
bbs5bound_7<-rbind(bbs31, bbs32, bbs33, bbs34, bbs35)
bbs5bound_7$subrouteID<-"31-35"
bbs5bound_7$scale<-"5"
bbs5bound_8<-rbind(bbs36, bbs37, bbs38, bbs39, bbs40)
bbs5bound_8$subrouteID<-"36-40"
bbs5bound_8$scale<-"5"
bbs5bound_9<-rbind(bbs41, bbs42, bbs43, bbs44, bbs45)
bbs5bound_9$subrouteID<-"41-45"
bbs5bound_9$scale<-"5"
bbs5bound_10<-rbind(bbs46, bbs47, bbs48, bbs49, bbs50)
bbs5bound_10$subrouteID<-"46-50"
bbs5bound_10$scale<-"5"
#now collapse the data and merge by....route # & AOU code? so that sub ID is Stops 1-5
#need to add occupancy data from duplicate AOU codes and state routes while this is merging
#instead of clustering like the above, maybe I have to merge each dataset forward again and again,
#can I write a forloop to do this? or can I just run ddply on the clusters I have, using the function "sum" 
#and applying sum to occupancy values, grouping by AOU and route # 
#sum on the value of "occupancy" for each group, broken down by AOU and stateroute 
library(plyr)
#^^incorporate in function loop where instead of bbs5bound_1 I have "data"
bbs_cluster = function(countData) {
  bdata = ddply(countData, c("stateroute", "AOU"), summarise,
        N = sum(length(occupancy)),
        occupancy = (sum(occupancy))/(length(occupancy))) #occupancy tricky, need to be aggregating it in diff way
  bdata = bdata[, c("stateroute", "AOU", "N", "occupancy")] #how to preserve the subrouteIDs ?
  return(bdata) 
}
#but occupancy calculated over amt of time spent at a site, # years, but year data missing 
#scale 5; naming convention for files = b(scale)_(part # in series)
b5_1 = bbs_cluster(bbs5bound_1)
b5_2 = bbs_cluster(bbs5bound_2)
b5_3 = bbs_cluster(bbs5bound_3)
b5_4 = bbs_cluster(bbs5bound_4)
b5_5 = bbs_cluster(bbs5bound_5)
b5_6 = bbs_cluster(bbs5bound_6)
b5_7 = bbs_cluster(bbs5bound_7)
b5_8 = bbs_cluster(bbs5bound_8)
b5_9 = bbs_cluster(bbs5bound_9)
b5_10 = bbs_cluster(bbs5bound_10)

#pair 1&2, 3&4, etc for scale 10 
bbs10_bound1 = rbind(b1, b2)
bbs10_bound2 = rbind(b3, b4)
bbs10_bound3 = rbind(b5, b6)
bbs10_bound4 = rbind(b7, b8)
bbs10_bound5 = rbind(b9, b10)

b10_1 = bbs_cluster(bbs10_bound1)
b10_2 = bbs_cluster(bbs10_bound2)
b10_3 = bbs_cluster(bbs10_bound3)
b10_4 = bbs_cluster(bbs10_bound4)
b10_5 = bbs_cluster(bbs10_bound5)

#pairings for scale 25 
bbs25_bound1 = rbind(b1, b2, b3, b4, b5)
bbs25_bound2 = rbind(b6, b7, b8, b9, b10)

b25_1 = bbs_cluster(bbs25_bound1)
b25_2 = bbs_cluster(bbs25_bound2)
######### do I have to add N's for the second run, too? 

## do 5 spp at one route at whatever resolution and compare occupancy from what it is vs what it should be 
#specify out stateroute and 4 AOU codes associated at each level, check to see if compounded accurately 
#occupancy is just a frequency, so it SHOULD be able to be added from multiple sites, 
#but maybe divided by the N of sites, so we would just have a mean occupancy, because it can't be greater than 1

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







