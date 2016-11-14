#Variation in occupancy at multiple scales WITHIN & ABOVE BBS sites
#REVISED ANALYSIS
#Molly F. Jenkins 
#11/11/2016



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
write.csv(good_rtes, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes.csv", row.names = FALSE) 

#compare # of routes and route numbers themselves to old version of bbs50 stored in BioArk 
require(dplyr)
# Subset the full BBS dataset to the routes above but including associated data
fifty_allyears = bbs50 %>% 
  filter(year >= 2000, year <= 2014) %>% 
  filter(stateroute %in% good_rtes$stateroute)

#finally works because needed $ specification, 
#can probably collapse into one line 
write.csv(fifty_allyears, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/fifty_allyears.csv", row.names = FALSE)
fifty_allyears = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/fifty_allyears.csv", header = TRUE)
#wrote to file just in case 

# merge lat longs from routes file to the list of "good" routes (2000-2014 present all years)
require(dplyr)
good_rtes2 = good_rtes %>% 
  left_join(routes, good_rtes, by = "stateroute") %>%
  dplyr::select(stateroute, Lati, Longi)
#write.csv(good_rtes2, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes2.csv", row.names = FALSE)

####occ_counts function for below-route scale####

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

####do I want to keep lat-lons? yes 
bbs_scalesorted = inner_join(bbs_scalesorted, good_rtes2, by = c("stateroute" = "stateroute")) 

write.csv(bbs_scalesorted, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scalesorted.csv", row.names = FALSE)

bbs_scalesorted = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scalesorted.csv", header = TRUE)



subrte_occ_avgs = bbs_scalesorted %>% 
  group_by(scale, stateroute, Lati, Longi) %>% #adding stateroute as proxy for rep to grouping
  summarize(mean = mean(occupancy)) %>% #calc across all AOUs for each stateroute (bc stateroutes ARE reps)
  group_by(scale, Lati, Longi) %>% #calc across all stateroutes for each scale 
  summarize(mean = mean(mean))




#####################################################################

####grid sampling justification####

# grain of (8, 4, 2, 1) corresponds to samples of 66, 19, 10, 5 rtes in each sample 

####Calculating occupancy at scales greater than a single route####

bbs_allyears = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_allyears.csv", header = TRUE)
good_rtes2 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes2.csv", header = TRUE)


#creating grain, "magic number" sample size for each grain, and reps vectors 
grain_sample = data.frame(c(1, 2, 4, 8), c(5, 10, 19, 66)) 
names(grain_sample) = c("grain", "magic_num")


#based on the grain, each grain in occ_avgs will have the SAME area!
grain_sample$area_calc = grain_sample$magic_num*50*(pi*(0.4^2)) #(in sq km)
#area in km by # of routes * 50 stops * area of a stop (for above-route scale later)
#write.csv(grain_sample, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/grain_sample.csv", row.names = FALSE) 
#wrote to file for later use in cross-scale merge


reps = c(100) #100? 50?

#nested forloops defining grid cells (i.e. latitudinal + longitudinal "bins"), 
#filtering routes sampled to those that fall within a given bin 
#and calculating occupancy for routes randomly sampled from those grouped within a bin  

output = data.frame(grain = NULL, lat = NULL, lon = NULL, rep = NULL, AOU = NULL, occ = NULL)
for (grain in grain_sample$grain) {
  sampling_lvl = grain_sample$magic_num[grain_sample$grain == grain]
  temproutes = good_rtes2
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
write.csv(bbs_scaledup, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scaledup.csv", row.names = FALSE)


####Mean of means for above-route scales####
bbs_scaledup = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scaledup.csv")


#for each unique combination of grain and lat and long across reps, what is the avg occ? 
# modify to take means of mean of each rep (in order of lat, lon, grain, and rep so as not to incorrectly avg values
occ_avgs = bbs_scaledup %>% group_by(lat, lon, grain, rep) %>% #adding rep to grouping
  summarize(mean = mean(occ)) %>% #summarize occ across Aou's for each rep 
  group_by(lat, lon, grain) %>% #group again, this time just by lat, lon, and grain
  summarize(mean = mean(mean)) # summarize mean occ across reps for each unique combo of lat, lon, and grain

#occ avgs for each grain scale across reps (FINAL)
occ_avgs$grid8ID = paste(floor(occ_avgs$lat/8)*8 + 8/2, floor(occ_avgs$lon/8)*8 + 8/2, sep = "")
occ_avgs$grid8ID = as.character(occ_avgs$grid8ID)

write.csv(occ_avgs, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/occ_avgs.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------
####Combining sub and above-route scale analyses outputs for comparison####
#this is the part that got messy in the original .R file 
#use grid8ID specified in grid_rtes_best to subset occ_avgs for only those that match grid8ID
#then can compare across increasing grain size, across area 

####lower scale analyses output prep####

bbs_scalesorted = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scalesorted.csv", header = TRUE)
#scale corresponds to # of stops in a segment 
#add area ID BEFORE merging, same with above-route dataset 
bbs_scalesorted$area = (bbs_scalesorted$scale)*(pi*(0.4^2)) # area in km by area of a BBS segment based on # of stops in that segment (for now)


bbs_scalesorted$scale = paste("seg", bbs_scalesorted$scale, sep = "")
bbs_scalesorted$scale = as.character(bbs_scalesorted)
bbs_scalesorted$lat = bbs_scalesorted$Lati
bbs_scalesorted$lon = bbs_scalesorted$Longi


#before joining datasets OR getting rid of variables -> calc mean of means for bbs_bigsmall 
#determining the mean of means across reps and then across scales for below a bbs route 

subrte_occ_avgs = bbs_bigsmall %>% group_by(lat, lon, scale, stateroute) %>% #adding stateroute as proxy for rep to grouping
  summarize(mean = mean(occupancy)) %>% #calc across all AOUs for each stateroute (bc stateroutes ARE reps)
  group_by(lat, lon, scale) %>% #calc across all stateroutes for each scale 
  summarize(mean = mean(mean))

#pull back in following variables:
#grid8ID, scaleID, lat, lon, area <-get thru grid8ID bc linked
#for later cross-scale join
bbs_prejoin = bbs_bigsmall %>%
  dplyr::select(lat, lon, scaleID, grid8ID, area) %>% 
  filter(grid8ID %in% grid_rtes_best$grid8ID) #fixed, added 

test_join = inner_join(subrte_occ_avgs, bbs_prejoin)
unique(test_join$grid8ID)

#write.csv(test_join, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/test_join.csv", row.names = FALSE)
#I think this worked? mean of means across stateroutes nested within those grid cells 
#but with occ calc'd BELOW route level before lumped together by cell 
#area = area of stop segment based on scaleID and lat lon of original stateroute
#even tho stateroutes no longer needed areas of segments preserved 

####upper scale analyses output prep####

#bringing back in routes present from 2000-2014 in every year, with grains, # rtes samples, and areas
grain_sample = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/grain_sample.csv", header = TRUE)
occ_avgs = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/occ_avgs.csv")

#merge by grain 
area_occ_avgs = occ_avgs %>% 
  inner_join(grain %in% grain_sample$grain) %>%
  dplyr::select(lat, lon, grain, mean, grid8ID, area)

#check with unique to make sure 6 cells correct ====> it's correct => checktest = unique(sub_occ_avgs$grid8ID)

sub_occ_avgs$grain = paste("0.0", sub_occ_avgs$grain, sep = "")
sub_occ_avgs$scaleID = sub_occ_avgs$grain


####Bringing upper and lower scale analyses together####
####Stitch lower scale analyses in using stateroute_latlon file to designate lower scales within their bins####
##below a bbs route: bbs_scalesorted
#paring down datasets to only relevant 7 corresponding variables 
test_join = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/test_join.csv", header = TRUE)


bbs_test_join = test_join %>% 
  dplyr::select(mean, grid8ID, scaleID, lat, lon, area)
unique(bbs_test_join$grid8ID) # this is where grid8ID is getting messed up, backtrack to formation

sub_occ_avgs = sub_occ_avgs %>% 
  dplyr::select(mean, grid8ID, scaleID, lat, lon, area)

sub_occ_avgs$grid8ID = as.character(sub_occ_avgs$grid8ID)
bbs_test_join$grid8ID = as.character(bbs_test_join$grid8ID)
bbs_test_join$scaleID = as.character(bbs_test_join$scaleID)



#joining datasets -> bbs_bigsmall with 491323 rows, occ_avgs with 205 rows, should add up to 491528
bbs_cross_scales = full_join(bbs_test_join, sub_occ_avgs)







    
    
    
  #intentionally allowing "X" column to be created for ease of selection of top 6 cells 
  write.csv(grid_rte_totals, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/grid_rte_totals.csv")
grid_rte_totals = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/grid_rte_totals.csv")
grid_rtes_best = grid_rte_totals %>% 
  filter(grid_rte_totals$X < 7) #taking top six grids in grid 8 ID 
grid_rtes_best$gridID = as.character(grid_rtes_best$gridID)
grid_rtes_best #min n value = 66 
#this is our sample number. 


#how select other cell numbers?

#rerun for ALL grain sizes, use this to inform and re-form "grain_sample" table. 
#rerun lines 109:130 for grains: 8, 4, 2, 1. Connect outputs into new table!











#grain_sample = data.frame(c(1, 2, 4, 8), c(1, 4, 10, 25)) <- old sample sizes for comparison

####NOW subset grid8ID cells to contain same # of routes per cell (aka # of routes in 6th cell aka LCD)####






#adding area data for each scale of each stateroute
grid_rtes_best$area = grid_rtes_best$n*50*(pi*(0.4^2)) 
#area in km by # of routes * 50 stops * area of a stop (for above-route scale later)
#write.csv(grid_rtes_best, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/grid_rtes_best.csv") 
#wrote to file for later use in cross-scale merge




