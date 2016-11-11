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

#compare # of routes and route numbers themselves to old version of bbs50 stored in BioArk 
require(dplyr)
# Subset the full BBS dataset to the routes above but including associated data
fifty_allyears = bbs50 %>% 
  filter(year >= 2000, year <= 2014) %>% 
  filter(stateroute %in% good_rtes$stateroute)

#finally works because needed $ specification, 
#can probably collapse into one line 

fifty_allyears = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/filteredrtes.csv", header = TRUE)
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

#write.csv(bbs_scalesorted, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scalesorted.csv", row.names = FALSE)

bbs_scalesorted = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scalesorted.csv", header = TRUE)

#####################################################################

####Selecting stateroutes present in each grid8ID cell, in each appropriate "magic number" sample####

#bringing back in routes present from 2000-2014 in every year
good_rtes2 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes2.csv", header = TRUE)
#bringing in lat lon associated with each route (so can determine routes present in grid cell)
routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')
routes$stateroute = 1000*routes$statenum + routes$Route


#putting these files together to get routes present in all years 2000-2014 
#AND their associated lat-lon data 
require(dplyr)
stateroute_latlon = routes %>% 
  filter( routes$stateroute %in% good_rtes2$stateroute) %>%  
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

test_sample = output 
unique(test_sample$grain)

#cool, developed forloop appropriate, now can calc grid rte totals FOR EACH GRAIN 
#and take top 6 for each, find min of top six and use as "magic number" 



  #count # of stateroutes in each cell, take top 6 cells (for both sub and above-route occupancy)
  require(dplyr) 
  grid_rte_totals_1 = test_sample %>% 
    filter(grain == 1)  %>%
    count(gridID) %>% 
    arrange(desc(n))
    
    grid_rte_totals_2 = test_sample %>% 
      filter(grain == 2)  %>%
    count(gridID) %>% 
      arrange(desc(n))
    
    grid_rte_totals_4 = test_sample %>% 
      filter(grain == 4)  %>%
    count(gridID) %>% 
      arrange(desc(n))
    
    grid_rte_totals_8 = test_sample %>% 
      filter(grain == 8) %>%
    count(gridID) %>% 
      arrange(desc(n))
  
    
#it works! now find how many cells can include when  in each grain set     
  
head(grid_rte_totals_1)    
    



# grain of (8, 4, 2, 1) corresponds to samples of 66, 31, 14, 6 rtes in each sample 



grid_rte_totals_1_sorted = grid_rte_totals_1 %>% 
  filter(n >= 66) #1x1grid cell is like, stupid small 

grid_rte_totals_2_sorted = grid_rte_totals_2 %>% 
  filter(n >= 66)

    
    
    
    
    
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

