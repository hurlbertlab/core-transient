##BIOL465 Project Code 
##Molly Gartland
##11/16/2015
##A Comparitive Analysis of BBC and BBS Core-Transient Data
##----PRE-ANALYSIS:----
#----Set working directory----
setwd("//bioark.ad.unc.edu/hurlbertlab/Gartland/Final folder")
#----Read in/open necessary Data----
newbbsdata<-read.csv("site_sp_occupancy_matrix.csv", header=TRUE)
head(newbbsdata)
#----Create two further subsetted and tidied dataframes from BBS data----
#one with headers "BBS Route", "Lat", and "Long", one with BBS route, AOU codes, and occupancy values---- 
library(tidyr)
tidybirds <- gather(data = newbbsdata, 
                    key = AOU, 
                    value = occupancy, 
                    X2881:X22860,
                    na.rm=TRUE)
head(tidybirds)
names(tidybirds)[1]<-"BBS_rte"
tidy_counts<-data.frame(tidybirds)
write.csv(tidy_counts, file = "tidy_counts.csv")
env_data<-read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/Final folder/env_data.csv", header=TRUE)
head(env_data)
BBSlatlon<-subset(env_data, select=c("stateroute", "Longi", "Lati"))
head(BBSlatlon)
#BBS data done 
#----Read in BBC data for both counts and sites (currently not seperated by year) (see Ethan White pdf-transformation code)----
bbc_counts<-read.csv("bbc_counts.csv", header=TRUE)
head(bbc_counts)
bbc_sites<-read.csv("bbc_sites.csv", header=TRUE)
head(bbc_sites)
#----For BBC site data: prepare for site-pairing by subsetting assigning headers "BBC Site", "Lat", "Long"----
bbc_lat_long<-unique(subset(bbc_sites, select=c("siteID", "latitude", "longitude")))
bbc_lat_long$longitude= -bbc_lat_long$longitude
head(bbc_lat_long)
length(table(bbc_lat_long$siteID)) #should be 360
#----Write for_loop to calculate distances between every BBS and BBC site combination to find sites and routes that correspond best----
#store minimum value for each iteration of in output table
require(fields)
#calculate distances using Great Circle Distance equation
output=c()
for(bbc in bbc_lat_long$siteID){
  temp.lat=bbc_lat_long$latitude[bbc_lat_long$siteID==bbc]
  temp.lon= bbc_lat_long$longitude[bbc_lat_long$siteID==bbc] 
  distances = rdist.earth(matrix(c(BBSlatlon$Longi,BBSlatlon$Lati), ncol=2),matrix(c(temp.lon,temp.lat), ncol=2),miles=FALSE, R=6371)
  minDist= min(distances)
  closestBBS=BBSlatlon$stateroute[distances==minDist]
  output=rbind(output, c(bbc, closestBBS, minDist))
  }
output = as.data.frame(output)
colnames(output)<-c("bbcsiteID", "bbsrouteID", "minDist")
head(output)
summary(output)
#~median 49km
#----Create a new csv file dataframe of these corresponding distance; sort out all combos < ~40 mi/km distances---- 
require(dplyr)
pair_dist<-filter(output, minDist < 40)
summary(pair_dist)
aoucodes<-read.csv("aou_speciesnames.csv", header = TRUE)
tidybirds$AOU<-gsub("X", "", tidybirds$AOU)
head(pair_dist) #paired sites (?) just the selection mechanism for filtering 
head(bbc_counts) # bbc species and occupancy
head(tidybirds) #= bbs species and occupancy
head(aoucodes) #common names and aou codes (bbs)
#----Match up AOU codes for species names from BBS data with Common Names from BBC data----
###each SPECIES is a row and governs the rows, not the paired sites alone!
tidy_aou<-merge(tidybirds, aoucodes[, c('PRIMARY_COM_NAME', 'AOU_OUT')], by.x="AOU", by.y="AOU_OUT")
head(tidy_aou)
bbc92 = subset(bbc_counts, year==1992, select = c('siteID', 'species', 'count', 'status'))
pair_dist2<-merge(pair_dist, bbc92, by.x="bbcsiteID", by.y="siteID")
head(pair_dist)
names(pair_dist2)[4]<-"species"
names(pair_dist2)[5]<-"count"
names(pair_dist2)[6]<-"status"
head(pair_dist2)
#subset pair_dist2 and limit to year 1992 
#then tidy_aou_pairs to pair_dist2
unique_bbs_rts<-unique(pair_dist2$bbsrouteID)
mtchd_bbs<-tidy_aou[tidy_aou$BBS_rte %in% unique_bbs_rts, ]
head(mtchd_bbs)
names(mtchd_bbs)[4]<-"species"
names(mtchd_bbs)[2]<-"bbsrouteID"
tidy_whole<-merge(pair_dist2, mtchd_bbs, by= c("species", "bbsrouteID"), all=T) #can look at complete spp list 
head(tidy_whole)
tidy_whole$status<-as.character(tidy_whole$status)
#assign "missing" character status to missing species associated with BBC observations
tidy_whole$status[is.na(tidy_whole$status)]<-"missing"
#----ANALYSIS:----
#----Part I: How well does BBS occupancy correspond to BBC status in general?----
fig_one<-boxplot(occupancy~status, data=tidy_whole, xlab="BBC Status", ylab= "BBS Occupancy")
fig_one$stats
#----Part II: Site-by-site basis from a BBS perspective: do the BBC and BBS designations agree or disagree?----
output2=c()
uniqueBBSrts<-unique(tidy_whole$bbsrouteID) 
for (bbs in uniqueBBSrts) { 
  temp.site=subset(tidy_whole, bbsrouteID == bbs) #alternatively have tried temp.site=tidy_whole$bbsrouteID[tidy_whole$bbsrouteID==bbs]
  BBS_Core_agreement = sum(temp.site$occupancy>0.67 & temp.site$status=="breeder", na.rm = T)/sum(temp.site$occupancy>0.67, na.rm = T)
  BBS_Trans_agreement = sum(temp.site$occupancy<0.33 & (temp.site$status=="visitor" | temp.site$status=="missing"), na.rm = T)/sum(temp.site$occupancy<0.33, na.rm = T)
  BBS_Core_disagreement = sum(temp.site$occupancy>0.67 & (temp.site$status=="visitor" | temp.site$status=="missing"), na.rm = T)/sum(temp.site$occupancy>0.67, na.rm = T)
  BBS_Trans_disagreement = sum(temp.site$occupancy<0.33 & temp.site$status=="breeder", na.rm = T)/sum(temp.site$occupancy<0.33, na.rm = T)
  output2=rbind(output2, c(bbs, BBS_Core_agreement, BBS_Core_disagreement, BBS_Trans_agreement, BBS_Trans_disagreement))
}
output2 = as.data.frame(output2)
head(output2)
names(output2)<-c("BBSrouteID", "Core_Core",  "Core_Trans", "Trans_Trans", "Trans_Core")
output2
fig_two<-boxplot(output2[, 2:5], xlab="Dataset Agreement", ylab= "Frequency")
fig_two$stats
#plot agreement vs distance apart
#merge minDist to output2 by bbsrouteID; do I want to merge by independent distance values? 
#should I clump agreements together? how do I want to categorize it? 
head(output2)

distance_agreement<-merge(output, output2,  by.x ="bbsrouteID", by.y="BBSrouteID")
head(distance_agreement)
fig_three<-boxplot(distance_agreement[4, 9:12], xlab="Dataset Agreement", ylab="minDist")
#calculate frequency averages for agreement pairings
frequency_avgs<-colMeans(output2)
frequency_avgs
#----Part III: Linear models exploring association of environmental variables with core/trans designations, occupancy----
#----Bring back in environmental data associated with BBS routes for upcoming linear models----
head(env_data)
mod_data<-merge(output2, env_data, by.x="BBSrouteID", by.y="stateroute")
mod_data2<-merge(tidy_whole, env_data, by.x="bbsrouteID", by.y="stateroute")
head(mod_data2)
#----Models:----
core_agreement_mod<-lm(Core_Core~sum.NDVI.mean, data=mod_data)
core_disagreement_mod<-lm(Core_Trans~sum.NDVI.mean, data=mod_data)
trans_agreement_mod<-lm(Trans_Trans~sum.NDVI.mean, data=mod_data)
trans_disagreement_mod<-lm(Trans_Core~sum.NDVI.mean, data=mod_data)
mod1<-lm(occupancy~sum.NDVI.mean, data=mod_data2)
summary(mod1)
mod2<-lm(occupancy~elev.mean, data=mod_data2)
summary(core_agreement_mod)
summary(core_disagreement_mod)
summary(trans_agreement_mod)
summary(trans_disagreement_mod)
#----Future intentions: Plot models; create additional models comparing regional factors----
#and other environmental variables in addition to NDVI? 
#Also examine additional years of BBC data. 
p1<-boxplot(occupancy~sum.NDVI.mean, mod_data2)
#-----
#download a raster file .bil; data<- raster () command takes it 
  #plot command plots raster data! 
  #if have lat and long data can extract from raster layer corresponding temps for those points 
    #Dr. Hurlbert sending code example
  #30 second res 
  #r package for getting modus data, talk to Tracie or GIS lab folder 
  #summer ndvi in modis folder 
  #overlay BBC points; extract lat/long associated ndvis and sync with BBC points 
  #try for r package of elevation for points, but also available in lab folder )
