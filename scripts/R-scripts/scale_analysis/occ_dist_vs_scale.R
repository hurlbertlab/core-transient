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


#fifty = ecoretriever::fetch('BBS50')
#bbs50 = fifty
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
  filter(year >= 1996, year <= 2010) %>% 
  filter(stateroute %in% good_rtes$stateroute) #finally works because needed $ specification, 
#can probably collapse into one line 
 
#write.csv(fifty_allyears, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/filteredrtes.csv")
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

# bring in bbs routes file 
routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')
routes$stateroute = 1000*routes$statenum + routes$Route


# merge lat longs from routes file to the list of "good" routes
require(dplyr)
good_rtes2 = good_rtes %>% 
  left_join(routes, good_rtes, by = "stateroute") %>%
  select(stateroute, Lati, Longi)


# map these routes
# need North American map base first -> modified from "dataset_map.R" script as reference

par(mfrow=c(1,1), mar=c(0,0,0,0))
cex.terr = 1.3

map('world',xlim=c(-165,-55),ylim=c(25,70), bg='black', fill=T, col='white')
map('state',add=T)

sites<-data.frame(longitude = good_rtes2$Longi, latitude = good_rtes2$Lati)
points(sites$longitude, sites$latitude, col= "red", pch=16)


# figure out how many routes are present in grid cells of varying size

# count how many there are per grid cell at different scales

# e.g. doing this for both lat & long




#do I want to join or do I want to pair by minimum difference? can I do that given the 1005 vs 50 item problem?

#use scale selection to sample for routes that occur at the above random latitudes 

#preferential over a fully random sample because the above step stratifies the sample for us geographically (right?)

#I want to merge data for lats with my "data-data" while also subsetting to leave only routes that match w/random lats

require(dplyr)
upscaled_sample_sites = good_rtes2 %>% 
  select(good_rtes2$Lati, good_rtes2$stateroute) %>% 
  good_rtes2$Lati = floor(good_rtes2$Lati/grain)*grain + grain/2 %>% 
  semi_join(good_rtes2, scale_selection) #keep getting errors about not being able to filter OR join 
#because object is "of class "c('double', 'numeric') 


#or instead of using dummy/ex variable "lats" do I get to use my own lats but use random to select # of them? ^






#------------------------------------------------------------------------------------------

####prototype forloop for generating scaled-up samples for calculating occupancy####



#need to mod occ_counts for up-scale data first?


occ_counts2 = function(countData, stateroutes) {
  subdata = filter(countData, stateroute %in% stateroutes)
  
  
  
  
  
  
    bbssub = countData[, c("stateroute", "year", "AOU", countColumns)]
  bbssub$groupCount = rowSums(bbssub[, countColumns])
  bbsu = unique(bbssub[bbssub[, "groupCount"]!= 0, c("stateroute", "year", "AOU")]) #because this gets rid of 0's...
  bbsu.rt.occ = data.frame(table(bbsu[,c("stateroute", "AOU")])/15)
  bbsu.rt.occ2 = bbsu.rt.occ[bbsu.rt.occ$Freq!=0,] #and this also gets rid of occupancy values of 0 total 
  names(bbsu.rt.occ2)[3] = "occupancy"
  bbsu.rt.occ2$aboverouteID = countColumns[1] #subrouteID refers to first route in a grouped sequence, occ refers to the occ for the # of combined routes
  bbsu.rt.occ2$grain = grain 
  bbsu.rt.occ2 = bbsu.rt.occ2[, c("stateroute", "grain", "aboverouteID", "AOU", "occupancy")]
  return(bbsu.rt.occ2)
}

grains = c(1, 2, 10)
reps = ???

output = c()
for (grain in grains) {
  temproutes = good_rtes2
  temproutes$latbin = floor(temproutes$Lati/grain)*grain + grain/2
  temproutes$longbin = floor(temproutes$Longi/grain)*grain + grain/2
  
  uniqLatBins = unique(temproutes$latbin)
  uniqLonBins = unique(temproutes$longbin)
    for (lat in uniqLatBins) {
      for (lon in uniqLonBins) {
        bin_rtes = filter(temproutes, latbin == lat, longbin == lon)
        
        for (i in 1:reps) {
          # sample X routes at random from bin
          sampled_rtes = sample_n(bin_rtes, X)
          
        }
        
        
        groupedCols = paste("Rt_group", floor(lats/grain)*grain + grain/2, sep = "")
    temp = occ_counts2(fifty_allyears, groupedCols, grain)
    output = rbind(output, temp)
  }
  
}


bbs_scaledup = output
