#Variation in occupancy at multiple scales WITHIN BBS sites
#Molly F. Jenkins 
#07/27/2016



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

bbs_scalesorted<-output

bbs_scalesorted = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scalesorted.csv", header = TRUE)

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
  dplyr::select(stateroute, Lati, Longi)


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

#creating grain, "magic number" sample size for each grain, and reps vectors 
grain_sample = data.frame(c(1, 2, 4, 8), c(1, 4, 10, 25)) #figure out why stopping at grain 2 
#- bc need if statement to know how to proceed? will finishing if statement help loops continue?
names(grain_sample) = c("grain", "magic_num")

reps = c(100) #100? 50?

#nested forloops defining grid cells (i.e. latitudinal + longitudinal "bins"), 
#filtering routes sampled to those that fall within a given bin 
#and calculating occupancy for routes randomly sampled from those grouped within a bin  

output = data.frame(grain = NULL, lat = NULL, lon = NULL, rep = NULL, AOU = NULL, occ = NULL)
for (grain in grain_sample$grain) {
  sampling_lvl = grain_sample$magic_num[grain_sample$grain == grain]
  temproutes = good_rtes3
  temproutes$latbin = floor(temproutes$Lati/grain)*grain + grain/2
  temproutes$longbin = floor(temproutes$Longi/grain)*grain + grain/2
  uniqLatBins = unique(temproutes$latbin)
  uniqLonBins = unique(temproutes$longbin)
  for (lat in uniqLatBins) {
    for (lon in uniqLonBins) {
      bin_rtes = filter(temproutes, latbin == lat, longbin == lon)
      
      
      if(sampling_lvl < length(bin_rtes$stateroute)) {
         for (i in 1:reps) {
          # sample X routes at random from bin
          # where X = our magic number of routes that can adequately 
          #  estimate occupancy for each grain; CHANGES with grain
          # so need to make table first containing both grains and X's, and change "grain in grains" to "grain in 'table'"
          sampled_rtes = sample_n(bin_rtes, sampling_lvl, replace = TRUE) 
          #pull "sample" from grain_sample row where grain in outer loop corresponds to grain in table
          #-> how do I make the row correspond to the current grain in the outermost loop? 
          #currently when I hardcode grain =4, it pulls out correct corresponding sample size (10)
          #but when I don't, and the loop runs through the first grain in the set, it fails to execute
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
          print(paste("Grain", grain, ", Lat:", lat, ", Lon:", lon))
        } #end of the rep loop
         } 
        
             #need to  specify that magic number X of sites sampled can't be larger than 
        # of routes available to pool from in a given bin
      
      
      
    } #end of the lon loop
    
  } #end of the lat loop
  
} #end of the grain loop



bbs_scaledup = output    #wrote to file in case

#write.csv(bbs_scaledup, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scaledup.csv", row.names = FALSE)


####Product of loops with occ by grain, 100 reps per grid cell/bin####

bbs_scaledup = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scaledup.csv")


#for each unique combination of grain and lat and long across reps, what is the avg occ? 

# modify to take means of mean of each rep (in order of lat, lon, grain, and rep so as not to incorrectly avg values
occ_avgs = bbs_scaledup %>% group_by(lat, lon, grain, rep) %>% #adding rep to grouping
  summarize(mean = mean(occ)) %>% #summarize occ across Aou's for each rep 
  group_by(lat, lon, grain) %>% #group again, this time just by lat, lon, and grain
  summarize(mean = mean(mean)) # summarize mean occ across reps for each unique combo of lat, lon, and grain


# Adding grid8lat, grid8lon, grid8id column 

occ_avgs$grid8lat = floor(occ_avgs$lat/8)*8 + 8/2
occ_avgs$grid8lon = floor(occ_avgs$lon/8)*8 + 8/2


#-----------------------------------------------------------------------------------------
####Combining sub and above-route scale analyses outputs for comparison####
##Pre-combining formatting of datasets:

#Pasting latlongs of bin centerpoints together from above-route scale to create character label analagous to "stateroute" label 
bbs_scaledup$gridcenter = paste(bbs_scaledup$lat, bbs_scaledup$lon, sep = "")


#locating and ID-ing stateroutes of routes contained within a given grid cell 

good_rtes2 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes2.csv", header = TRUE)

routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')
routes$stateroute = 1000*routes$statenum + routes$Route

require(dplyr)
temproutes = good_rtes2 %>% 
  filter(routes, stateroute %in% good_rtes2$stateroute) %>% #filter, don't join bc extraneous and unnecessary info 
  dplyr::select(stateroute, Lati, Longi) #just want stateroutes and associated lat/longs


grain = 8

#everything below this line needs to be combined into a forloop for multiple grains and rerun 
#-----------------------------------------------------------------------------------------------

temproutes$latbin = floor(temproutes$Lati/grain)*grain + grain/2 
temproutes$longbin = floor(temproutes$Longi/grain)*grain + grain/2

temproutes$gridcenter = paste(temproutes$latbin, temproutes$longbin, sep = "")

#filtering data by mutual $gridcenter columns to acquire stateroutes in each grid 

require(dplyr) 
grid_rte_totals = temproutes %>% count(gridcenter) %>% arrange(desc(n))


#rework and automate code in a loop for multiple grains 
