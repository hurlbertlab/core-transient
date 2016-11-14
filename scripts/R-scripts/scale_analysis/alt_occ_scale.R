#Variation in occupancy at multiple scales WITHIN & ABOVE BBS sites
#REVISED ANALYSIS - Alternate: 6 region nested loop with nearest rtes paired with focal rtes
#Molly F. Jenkins 
#11/11/2016

#Summary: ID six regions 
#for (region in six regions) 
#   for (scale in 2:66) (minimum common # of rtes in each grid)
#random sampling for every possible number of rtes between 2:66 instead of relying on a magic number sample for each grain 
#create a distance matrix 
#calculate great circle distance between routes 
#subset distance matrix to focal routes
#for (i in 1:length(a) 
  #for j in (i +1):length(a) <- keeps redundant pairings so can rule out later as needed 

#when focal route in loop is "2", want to find all of the rows in a where i OR j is the focal route 
#then, for five focal routes, rank by distance, and take just the top five 



#Set working directory to core-transient folder on github i.e. setwd("C:/git/core-transient/")


## Please download and install the following packages:
# maps, sp, rgdal, raster, maptools, rgeos
library(raster)
library(maps)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)

####Bringing in BBS50 stop data and prepping it for sub-route scale partitioning####

#bbs50 = ecoretriever::fetch('BBS50')
#bbs50 = bbs50$counts
#bbs50$stateroute = bbs50$statenum*1000 + bbs50$Route
#bbs50$stateroute = as.integer(bbs50$stateroute)
#^derivation of data from ecoretriever; still too large to host on github so save and pull from BioArk

bbs50 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs50.csv", header = TRUE)

# Get subset of BBS routes (just routes) btw 1996-2010 surveyed in EVERY year

require(dplyr)
#from Sara's code
good_rtes = bbs50 %>% 
  filter(year >= 2000, year <= 2014) %>% #shifted 15 year window up because missing 1996 data, and 2015 data available
  select(year, stateroute) %>%
  unique() %>%    
  group_by(stateroute) %>%  
  count(stateroute) %>% 
  filter(n == 15) #now getting 1005 routes with consecutive data :^)
#write.csv(good_rtes, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes.csv", row.names = FALSE) 

#compare # of routes and route numbers themselves to old version of bbs50 stored in BioArk 
require(dplyr)
# Subset the full BBS dataset to the routes above but including associated data
fifty_allyears = bbs50 %>% 
  filter(year >= 2000, year <= 2014) %>% 
  filter(stateroute %in% good_rtes$stateroute)

#finally works because needed $ specification, 
#can probably collapse into one line 
#write.csv(fifty_allyears, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/fifty_allyears.csv", row.names = FALSE)

#wrote to file just in case 

# merge lat longs from routes file to the list of "good" routes (2000-2014 present all years)
require(dplyr)
good_rtes2 = good_rtes %>% 
  left_join(routes, good_rtes, by = "stateroute") %>%
  dplyr::select(stateroute, Lati, Longi)
#write.csv(good_rtes2, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes2.csv", row.names = FALSE)


#########


#good_rtes2 = routes 2000-2014, and with lats + lons 
#fifty_allyears = data and routes 2000-2014, no lat lons yet tho so need to combine first

#actually, just need good_rtes2 info to get pairings

good_rtes2 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes2.csv", header = TRUE)
fifty_allyears = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/fifty_allyears.csv", header = TRUE)


bbs50_goodrtes = fifty_allyears %>% 
  inner_join(stateroute %in% good_rtes2$stateroute)

#modify below code to reflect BBS data and pairing routes within BBS by min dist 


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



bbc_lat_long$longitude = good_rtes2$Longi 
bbc_lat_long$latitude = good_rtes2$Lati
#----Write for_loop to calculate distances between every BBS and BBC site combination to find sites and routes that correspond best----
#store minimum value for each iteration of in output table
require(fields)
#calculate distances using Great Circle Distance equation
output=c()
for(focal_bbs in good_rtes2$stateroute){
  temp.lat=good_rtes2$Lati[good_rtes2$stateroute==focal_bbs]
  temp.lon= good_rtes2$Longi[good_rtes2$stateroute==focal_bbs] 
  distances = rdist.earth(matrix(c(good_rtes2$Longi,good_rtes2$Lati), ncol=2),matrix(c(temp.lon,temp.lat), ncol=2),miles=FALSE, R=6371)
  minDist = min(distances)
  closest_bbs = good_rtes2$stateroute[distances==minDist]
  output=rbind(output, c(focal_bbs, closest_bbs, minDist))
}
output = as.data.frame(output)
colnames(output)<-c("bbcsiteID", "bbsrouteID", "minDist")
head(output)
summary(output)
#~median 49km
