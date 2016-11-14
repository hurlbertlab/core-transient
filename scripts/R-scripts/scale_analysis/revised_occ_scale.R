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

write.csv(bbs_scalesorted, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scalesorted.csv", row.names = FALSE)

bbs_scalesorted = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scalesorted.csv", header = TRUE)

#####################################################################

####grid sampling justification####

#bringing in lat lon associated with each route (so can determine routes present in grid cell)
good_rtes = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes.csv", header = TRUE)
routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')
routes$stateroute = 1000*routes$statenum + routes$Route


#putting these files together to get routes present in all years 2000-2014 
#AND their associated lat-lon data 
require(dplyr)
stateroute_latlon = routes %>% 
  filter( routes$stateroute %in% good_rtes$stateroute) %>%  
  dplyr::select(stateroute, Lati, Longi) 
#write.csv(stateroute_latlon, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/stateroute_latlon.csv", row.names = FALSE)


#setting grain to largest grid cell size 
sampledgrains = data.frame(c(1, 2, 4, 8))
names(sampledgrains) = c("grain")
output = c()
for (grain in sampledgrains$grain) {
  #binning stateroutes according to latlon in grid8ID cells
  stateroute_latlon$latbin = floor(stateroute_latlon$Lati/grain)*grain + grain/2 
  stateroute_latlon$longbin = floor(stateroute_latlon$Longi/grain)*grain + grain/2
  stateroute_latlon$gridID = paste(stateroute_latlon$latbin, stateroute_latlon$longbin, sep = "")
  stateroute_latlon$grain = grain
  output = rbind(output, stateroute_latlon)
  
  }

sample_sizes = output 
unique(sample_sizes$grain)
write.csv(sample_sizes, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/sample_sizes.csv", row.names = TRUE)
#cool, developed forloop appropriate, now can calc grid rte totals FOR EACH GRAIN 
#and take top 6 for each, find min of top six and use as "magic number" 

  #count # of stateroutes in each cell, take top 6 cells (for both sub and above-route occupancy)
  require(dplyr) 
  grid_rte_totals_1 = sample_sizes %>% 
    filter(grain == 1)  %>%
    count(gridID) %>% 
    arrange(desc(n))
    
    grid_rte_totals_2 = sample_sizes %>% 
      filter(grain == 2)  %>%
    count(gridID) %>% 
      arrange(desc(n))
    
    grid_rte_totals_4 = sample_sizes %>% 
      filter(grain == 4)  %>%
    count(gridID) %>% 
      arrange(desc(n))
    
    grid_rte_totals_8 = sample_sizes %>% 
      filter(grain == 8) %>%
    count(gridID) %>% 
      arrange(desc(n))
  
    
#it works! now find how many cells can include when in each grain set     
  
head(grid_rte_totals_1) #take head of EACH    
    
# grain of (8, 4, 2, 1) corresponds to samples of 66, 19, 10, 5 rtes in each sample 


####Calculating occupancy at scales greater than a single route####

bbs_allyears = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_allyears.csv", header = TRUE)
routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')
routes$stateroute = 1000*routes$statenum + routes$Route
good_rtes = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes.csv", header = TRUE)

# merge lat longs from routes file to the list of "good" routes (2000-2014 present all years)
require(dplyr)
good_rtes2 = good_rtes %>% 
  left_join(routes, good_rtes, by = "stateroute") %>%
  dplyr::select(stateroute, Lati, Longi)

#creating grain, "magic number" sample size for each grain, and reps vectors 
grain_sample = data.frame(c(1, 2, 4, 8), c(5, 10, 19, 66)) #figure out why stopping at grain 2 
#- bc need if statement to know how to proceed? will finishing if statement help loops continue?
names(grain_sample) = c("grain", "magic_num")

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











grain_sample = data.frame(c(1, 2, 4, 8), c(1, 4, 10, 25))

####NOW subset grid8ID cells to contain same # of routes per cell (aka # of routes in 6th cell aka LCD)####






#adding area data for each scale of each stateroute
grid_rtes_best$area = grid_rtes_best$n*50*(pi*(0.4^2)) 
#area in km by # of routes * 50 stops * area of a stop (for above-route scale later)
#write.csv(grid_rtes_best, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/grid_rtes_best.csv") 
#wrote to file for later use in cross-scale merge




