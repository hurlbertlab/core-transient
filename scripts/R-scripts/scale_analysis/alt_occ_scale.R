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
# maps, sp, rgdal, raster, maptools, rgeos, dplyr, fields
library(raster)
library(maps)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)
library(fields)

####Bringing in BBS50 stop data and prepping it for sub-route scale partitioning####

bbs50 = ecoretriever::fetch('BBS50')
bbs50 = bbs50$counts
bbs50$stateroute = bbs50$statenum*1000 + bbs50$Route
bbs50$stateroute = as.integer(bbs50$stateroute)
#^derivation of data from ecoretriever; still too large to host on github so save and pull from BioArk

bbs50 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs50.csv", header = TRUE)

# Get subset of BBS routes (just routes) btw 1996-2010 surveyed in EVERY year

require(dplyr)
#from Sara's code
good_rtes = bbs50 %>% 
  filter(year >= 2001, year <= 2015) %>% #shifted 15 year window up because missing 1996 data, and 2015 data available
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

good_rtes2 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes2.csv", header = TRUE)
fifty_allyears = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/fifty_allyears.csv", header = TRUE)

require(dplyr)
bbs50_goodrtes = inner_join(fifty_allyears, good_rtes2, by = "stateroute")
#ID and subset to routes within top six regions id'd in grid_sampling_justification 
#assing "reg" label 
bbs50_goodrtes$grid8ID = paste(floor(bbs50_goodrtes$Lati/8)*8 + 8/2, floor(bbs50_goodrtes$Longi/8)*8 + 8/2, sep = "")
bbs50_goodrtes$grid8ID = as.character(bbs50_goodrtes$grid8ID)


#bring in top 6 grids for max scale (8 degree) from grid_sampling_justification.R script, 66 rte cutoff 
top6_grid8 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/top6_grid8.csv", header = TRUE)


#filter the 50 stop data to just those routes present within those 6 grid cell regions of interest 
fifty_top6 = bbs50_goodrtes %>% 
  filter(grid8ID %in% top6_grid8$x) %>%
  dplyr::select(7:62)#about halves the bbs50_goodrtes set of usable routes, and no redundant columns
#write.csv(fifty_top6, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/fifty_top6.csv", row.names = FALSE)


#######

fifty_top6 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/fifty_top6.csv", header = TRUE)

#----Write for_loop to calculate distances between every BBS site combination to find focal and associated routes that correspond best----
#store minimum value for each iteration of combos in output table


require(fields)
# Distance calculation between all combination of 
distances = rdist.earth(matrix(c(good_rtes2$Longi, good_rtes2$Lati), ncol=2),
                        matrix(c(good_rtes2$Longi, good_rtes2$Lati), ncol=2),
                        miles=FALSE, R=6371)

dist.df = data.frame(rte1 = rep(good_rtes2$stateroute, each = nrow(good_rtes2)),
                     rte2 = rep(good_rtes2$stateroute, times = nrow(good_rtes2)),
                     dist = as.vector(distances))

# inside loop, e.g., filter(dist.df, rte1 == 2001, rte2 != 2001)
dist.df2 = filter(dist.df, rte1 != rte2)

uniqrtes = unique(dist.df2$rte1)
####Aggregating loop#### #don't need a rep loop right?

bbs_allyears = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_allyears.csv", header = TRUE)
numrtes = 1:66
output = data.frame(r = NULL, nu = NULL, AOU = NULL, occ = NULL)
for (r in uniqrtes) {
  for (nu in numrtes) {
  tmp = filter(dist.df2, rte1 == r) %>%
    arrange(dist)
  tmprtes = tmp$rte2[1:nu]   #selects rtes to aggregate under focal route by dist from focal route, based on nu in numrtes range
  # Aggregate those routes together, calc occupancy, etc
  
  bbssub = filter(bbs_allyears, stateroute %in% tmprtes)
  bbsuniq = unique(bbssub[, c('Aou', 'Year')])
  occs = bbsuniq %>% count(Aou) %>% mutate(occ = n/15)
            
            temp = data.frame(r = r,
                              nu = nu,
                              Aou = occs$Aou,
                              occ = occs$occ)   #can add lat/lons in later, and grids based on the r right? 
            
            output = rbind(output, temp)
            print(paste("Focal rte", r, "# rtes sampled", nu))
        } #n loop
        
       } #r loop
    
##Problem: right now r is each focal route, 
#but that focal route is NOT included in occ calcs, just the secondary routes associated with it are


bbs_focal_occs = as.data.frame(output)
write.csv(bbs_focal_occs, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_focal_occs.csv", row.names = FALSE)
head(output)  
  

#########
####Calc mean of means####

#for each unique combination of focal route and number of routes and Aou, what is the avg occ? -> already calc'd
# what is the avg across Aou's for each unique combo of focal route and number of aggregated routes? 

occ_avgs = bbs_focal_occs %>% group_by(r, nu) %>% 
  summarize(mean = mean(occ))  #summarize occ across Aou's for each rep 

####Calc area####

occ_avgs$log_area = log(occ_avgs$nu*50*(pi*(0.4^2))) #in km 
# number of routes * fifty stops * area in sq km of a stop 

####Occupancy vs area####

plot(occ_avgs$log_area, occ_avgs$mean, xlab = "log(area)", ylab = "mean occupancy")


#still just at above route scale tho - now need to stitch above and below together again 

####Find lat/lons of focal routes and subsequently their grid8IDs####


