#Variation in occupancy at multiple scales WITHIN BBS sites
#Molly F. Jenkins 
#07/27/2016

#until dplyr masking issues resolved: require dplyr right before every time it's used

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

#compare # of routes and route numbers themselves to old version of bbs50 stored in BioArk 
require(dplyr)
# Subset the full BBS dataset to the routes above but including associated data
fifty_allyears = bbs50 %>% 
  filter(year >= 2000, year <= 2014) %>% 
  filter(stateroute %in% good_rtes$stateroute)

#finally works because needed $ specification, 
#can probably collapse into one line 

write.csv(fifty_allyears, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/filteredrtes.csv")
#wrote to file just in case 


###So for the whole dataset, 10 pt count stops: #we are only getting one out of five chunks along 
#want to estimate occupancy across each one, as of now only estimating for count 10 column 
#fifty pt count data and then taking pts 1-5 and collapsing them all together 
#########
occ_counts = function(countData, countColumns, scale) {
  bbssub = countData[, c("stateroute", "year", "AOU", countColumns)]
  bbssub$groupCount = rowSums(bbssub[, countColumns])
  bbsu = unique(bbssub[bbssub[, "groupCount"]!= 0, c("stateroute", "year", "AOU")]) #because this gets rid of 0's...
  bbsu.rt.occ = data.frame(table(bbsu[,c("stateroute", "AOU")])/15)
  bbsu.rt.occ2 = bbsu.rt.occ[bbsu.rt.occ$Freq!=0,] #and this also gets rid of occupancy values of 0 total 
  names(bbsu.rt.occ2)[3] = "occupancy"
  bbsu.rt.occ2$subrouteID = countColumns[1] #subrouteID refers to first stop in a grouped sequence, occ refers to the occ for the # of combined stops
  bbsu.rt.occ2$scale = scale 
  bbsu.rt.occ2 = bbsu.rt.occ2[, c("stateroute", "scale", "subrouteID", "AOU", "occupancy")]
  return(bbsu.rt.occ2)
}

# Generic calculation of occupancy for a specified scale

scales = c(5, 10, 25, 50)


output = c()
for (scale in scales) {
  numGroups = floor(50/scale)
  for (g in 1:numGroups) {
    groupedCols = paste("Stop", ((g-1)*scale + 1):(g*scale), sep = "")
    temp = occ_counts(fifty_allyears, groupedCols, scale)
    output = rbind(output, temp)
  }
  
}

bbs_scalesorted2<-output
# -----------------------------------------------------------

####Calculating occupancy at scales greater than a single route####

good_rtes2 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes2.csv", header = TRUE)

bbs_allyears = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_allyears.csv", header = TRUE)

# bring in bbs routes file 
routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')
routes$stateroute = 1000*routes$statenum + routes$Route


# merge lat longs from routes file to the list of "good" routes
require(dplyr)
good_rtes3 = good_rtes2 %>% 
  left_join(routes, good_rtes2, by = "stateroute") %>%
  select(stateroute, Lati, Longi)


# map these routes
# need North American map base first -> modified from "dataset_map.R" script as reference

par(mfrow=c(1,1), mar=c(0,0,0,0))
cex.terr = 1.3

map('world',xlim=c(-165,-55),ylim=c(25,70), bg='black', fill=T, col='white')
map('state',add=T)

sites<-data.frame(longitude = good_rtes3$Longi, latitude = good_rtes3$Lati)
points(sites$longitude, sites$latitude, col= "red", pch=16)


# figure out how many routes are present in grid cells of varying size

# count how many there are per grid cell at different scales

# e.g. doing this for both lat & long


#------------------------------------------------------------------------------------------

####prototype forloop for generating scaled-up samples for calculating occupancy####

#bring in pared down version of fifty_allyears (good data associated with good routes for continuous 15yr span)
#honestly should pare it down further back in script and then just use it here as-is


#reworked sequel to occ_counts function, but for scales above a single bbs route 
#instead of count columns, just using stop totals (hard code to StopTotal?)

#creating grain size and reps vectors 
grains = c(2) 
reps = c(100) #100? 50?

#nested forloops defining grid cells (i.e. latitudinal + longitudinal "bins"), 
#filtering routes sampled to those that fall within a given bin 
#and calculating occupancy for routes randomly sampled from those grouped within a bin  

output = data.frame(grain = NULL, lat = NULL, lon = NULL, rep = NULL, AOU = NULL, occ = NULL)
for (grain in grains) {
  temproutes = good_rtes3
  temproutes$latbin = floor(temproutes$Lati/grain)*grain + grain/2
  temproutes$longbin = floor(temproutes$Longi/grain)*grain + grain/2
  uniqLatBins = unique(temproutes$latbin)
  uniqLonBins = unique(temproutes$longbin)
  for (lat in uniqLatBins) {
    for (lon in uniqLonBins) {
      bin_rtes = filter(temproutes, latbin == lat, longbin == lon)
      
      #if(X > binroutes, skip to the next lon in uniqLonBins) 
        #need to  specify that magic number X of sites sampled can't be larger than 
        # of routes available to pool from in a given bin
      
      for (i in 1:reps) {
        # sample X routes at random from bin
        # where X = our magic number of routes that can adequately 
        #  estimate occupancy for each grain; CHANGES with grain
        sampled_rtes = sample_n(bin_rtes, 4)  #pull "4" from pre-defined table where paired with associated grain
        bbssub = filter(bbs_allyears, stateroute %in% sampled_rtes$stateroute)
        bbsuniq = unique(bbssub[, c('Aou', 'Year')])
        occs = bbsuniq %>% count(Aou) %>% mutate(occ = n/15)
        
        temp = data.frame(grain = grain, 
                          lat = lat, 
                          lon = lon, 
                          rep = i,
                          Aou = occs$Aou,
                          occ = occs$occ)
        
        output = rbind(output, temp)
        
      } #end of the rep loop
      
    } #end of the lon loop
    
  } #end of the lat loop
  
} #end of the grain loop



bbs_scaledup = output

#-----------------------------------------------------------------------------------------

####Determining ideal magic number "X" assigned to each grain; creating this file to draw from to use in below
#in lieu of hardcoding grain and sample_n portions prior to loops 


grain = 10
temproutes$latbin = floor(temproutes$Lati/grain)*grain + grain/2
temproutes$longbin = floor(temproutes$Longi/grain)*grain + grain/2
temproutes$latbin = floor(temproutes$Lati/grain)*grain + grain/2
temproutes$longbin = floor(temproutes$Longi/grain)*grain + grain/2

ct = temproutes %>% count(latbin, longbin)

map('state')
points(ct$longbin, ct$latbin, cex = 3*ct$n/max(ct$n), pch = 16)



hist(ct$n)
median(ct$n)

quantile(ct$n, 0.6)
