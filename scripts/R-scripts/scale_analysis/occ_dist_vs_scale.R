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

#occ avgs for each grain scale across reps (FINAL)


#subset to only stateroutes across each grain that fall into grain 8 bin as well (allowing us to compare across scale)

# adding grid8id column to ID which rows have lat/lons that match bin at grain 8 
# remember: don't need stateroutes at this point, because lumped together 

occ_avgs$grid8ID = paste(floor(occ_avgs$lat/8)*8 + 8/2, floor(occ_avgs$lon/8)*8 + 8/2, sep = "")


#can use grain size and scale 8 grid info -> each panel is based on unique grid8ID
#do need to select top 6 categories based on count tho before plotting


#write.csv(occ_avgs, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/occ_avgs.csv", row.names = FALSE)

#-----------------------------------------------------------------------------------------

####Combining sub and above-route scale analyses outputs for comparison####
##Pre-combining formatting of datasets:


##ocating and ID-ing stateroutes contained within a given grid cell (since disappear in process of deriving occ)##

#bringing back in routes present from 2000-2014 in every year
good_rtes2 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes2.csv", header = TRUE)


#bringing in lat lon associated with each route (so can determine routes present in grid cell)
routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')
routes$stateroute = 1000*routes$statenum + routes$Route


#putting these files together to get good routes AND their associated lat-lon data 
#(so can be matched in corresponding grid cells)


require(dplyr)
stateroute_latlon = routes %>% 
  filter( routes$stateroute %in% good_rtes2$stateroute) %>% #filter, don't join bc extraneous and unnecessary info 
  dplyr::select(stateroute, Lati, Longi) #just getting lats and longs of stateroutes in general 

#setting grain to highest cell size 
grain = 8

#binning stateroutes according to latlon in grain 8 cells
stateroute_latlon$latbin = floor(stateroute_latlon$Lati/grain)*grain + grain/2 
stateroute_latlon$longbin = floor(stateroute_latlon$Longi/grain)*grain + grain/2

stateroute_latlon$grid8ID = paste(stateroute_latlon$latbin, stateroute_latlon$longbin, sep = "")


#count # of stateroutes in each cell, take top 6
require(dplyr) 
grid_rte_totals = stateroute_latlon %>% count(grid8ID) %>% arrange(desc(n)) 


#write.csv(grid_rte_totals, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/grid_rte_totals.csv")

grid_rte_totals = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/grid_rte_totals.csv")
#intentionally allowing "X" column to be created for ease of selection of top 6 cells 
grid_rtes_best = grid_rte_totals %>% 
  filter(grid_rte_totals$X < 7) #taking top six grids only for state routes to dictate sample


#-------------------------------------------------------------------
#use grid8ID specified in grid_rtes_best to subset occ_avgs for only those that match grid8ID
#then can compare across increasing grain size 
occ_avgs = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/occ_avgs.csv")

sub_occ_avgs = occ_avgs %>% 
  filter(grid8ID %in% grid_rtes_best$grid8ID)

#check with unique to make sure 6 cells correct ====> it's correct 
#checktest = unique(sub_occ_avgs$grid8ID)



####Map occ ~ grain at each of the six sample collections#### 

#super gross right now but it works 
#par(mfrow = c(2, 3))
#plot(sub_occ_avgs$grain[sub_occ_avgs$grid8ID == "44-76"], sub_occ_avgs$mean[sub_occ_avgs$grid8ID == "44-76"], xlab = "grain", ylab = "mean occ", main = "Grid 44-76")
#plot(sub_occ_avgs$grain[sub_occ_avgs$grid8ID == "36-84"], sub_occ_avgs$mean[sub_occ_avgs$grid8ID == "36-84"], xlab = "grain", ylab = "mean occ", main = "Grid 36-84")
#plot(sub_occ_avgs$grain[sub_occ_avgs$grid8ID == "44-92"], sub_occ_avgs$mean[sub_occ_avgs$grid8ID == "44-92"], xlab = "grain", ylab = "mean occ", main = "Grid 44-92")
#plot(sub_occ_avgs$grain[sub_occ_avgs$grid8ID == "36-92"], sub_occ_avgs$mean[sub_occ_avgs$grid8ID == "36-92"], xlab = "grain", ylab = "mean occ", main = "Grid 36-92")
#plot(sub_occ_avgs$grain[sub_occ_avgs$grid8ID == "36-76"], sub_occ_avgs$mean[sub_occ_avgs$grid8ID == "36-76"], xlab = "grain", ylab = "mean occ", main = "Grid 36-76")
#plot(sub_occ_avgs$grain[sub_occ_avgs$grid8ID == "36-108"], sub_occ_avgs$mean[sub_occ_avgs$grid8ID == "36-108"], xlab = "grain", ylab = "mean occ", main = "Grid 36-108")


####Stitch lower scale analyses in using stateroute_latlon file to designate lower scales within their bins####
##below a bbs route: bbs_scalesorted

bbs_scalesorted = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scalesorted.csv", header = TRUE)

#I DO want to join this time because I want the stateroute lat-long info, 
#so I know which bins the stateroutes can be paired up in 

bbs_bigsmall = inner_join(bbs_scalesorted, stateroute_latlon, by = c("stateroute" = "stateroute")) 
  
#now I know how the sub-route data is nested in the grid 8 data 
#and I can match the occ avg data for grain 8 in
#by pulling in sub occ avgs? at the diff grains? based on grid8ID 

#or do I want to not add it this way, but instead get rid of the AOU column and just use 
#grain as scale and mean as occupancyand subroute ID as grid8ID corresponding variables 

#so I actually need to rename some things first 
#rename both "subrouteID" and "grain" to "routeID"
#paste in a prefix on both first so I know whether above or below route -> 0 for grain, 00 for scale 
#to ensure scales ultimately still follow correct direction and ranking in relation to each other 



#preparing columns for large merge (renaming analagous columns and 
#ensuring data still corresponds with appropriate scales and unique ID's)

sub_occ_avgs$grain = paste("0", sub_occ_avgs$grain, sep = "")
bbs_bigsmall$scale = paste("00", bbs_bigsmall$scale, sep = "")

sub_occ_avgs$scaleID = sub_occ_avgs$grain
bbs_bigsmall$scaleID = bbs_bigsmall$scale
#scale ID NEEDS to be chr or num, not integer, otherwise 0's will be removed 
#altho it seems to look ok 
#hmmm says chr currently, not sure why switched during the join 
#try designating as num to see if fixed, or putting a . in front otherwise 


#in sub_occ for the larger scales, instead of stateroute I can have the unrounded lat_lon paired and rename it siteID? 

sub_occ_avgs$siteID = paste(sub_occ_avgs$lat, sub_occ_avgs$lon, sep = "")
bbs_bigsmall$siteID = bbs_bigsmall$stateroute

sub_occ_avgs$occupancy = sub_occ_avgs$mean    #renaming occ at above route scale to correspond with occ 

#also keep lat lon info for bbs_bigsmall, don't select it out -> or can I get rid of lat lon in sub occ 
#since now have unique ID
#R won't let me, so I guess I'm keeping lat lons in bbs_bigsmall because easier 
#just make sure columns are still "lat", "lon" 




#and sub_supr_rteID corresponds to the grid8ID, so need it copied into a second column - one used for nesting**
#and one used purely for labeling the above route ID -> confirm idea?

sub_occ_avgs$sub_supr_rteID = sub_occ_avgs$grid8ID
bbs_bigsmall$sub_supr_rteID = bbs_bigsmall$subrouteID
bbs_bigsmall$lat = bbs_bigsmall$Lati
bbs_bigsmall$lon = bbs_bigsmall$Longi

bbs_bigsmall = bbs_bigsmall %>% 
  dplyr::select(siteID, sub_supr_rteID, occupancy, grid8ID, scaleID, lat, lon)

sub_occ_avgs = sub_occ_avgs %>% 
  dplyr::select(siteID, sub_supr_rteID, occupancy, grid8ID, scaleID, lat, lon)

#both datasets should have 7 corresponding variables 

#make sure variables joining by match in class type! so ID's should be chr vectors 

bbs_bigsmall$siteID = as.character(bbs_bigsmall$siteID)
sub_occ_avgs$grid8ID = as.character(sub_occ_avgs$grid8ID)

#joining datasets -> bbs_bigsmall with 866748 rows, occ_avgs with 205 rows, should add up to 866953

bbs_cross_scales = full_join(bbs_bigsmall, sub_occ_avgs)

#error message BUT adds up to 866953! Hooray! 

#write.csv(bbs_cross_scales, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_cross_scales.csv", row.names = FALSE)


bbs_cross_scales = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_cross_scales.csv")

#is 54KB small enough to fit on git? can I back it up there? 

unique(bbs_cross_scales$scaleID)  #seems to look ok - ok definitely not ok for plotting though
#need to go back and fix the scaleID because influences the ordering by which R reads the scale 

par(mfrow = c(2, 3))
plot(bbs_cross_scales$scaleID[bbs_cross_scales$grid8ID == "44-76"], bbs_cross_scales$occupancy[bbs_cross_scales$grid8ID == "44-76"], xlab = "grain", ylab = "mean occ", main = "Grid 44-76")
plot(bbs_cross_scales$scaleID[bbs_cross_scales$grid8ID == "36-84"], bbs_cross_scales$occupancy[bbs_cross_scales$grid8ID == "36-84"], xlab = "grain", ylab = "mean occ", main = "Grid 36-84")
plot(bbs_cross_scales$scaleID[bbs_cross_scales$grid8ID == "44-92"], bbs_cross_scales$occupancy[bbs_cross_scales$grid8ID == "44-92"], xlab = "grain", ylab = "mean occ", main = "Grid 44-92")
plot(bbs_cross_scales$scaleID[bbs_cross_scales$grid8ID == "36-92"], bbs_cross_scales$occupancy[bbs_cross_scales$grid8ID == "36-92"], xlab = "grain", ylab = "mean occ", main = "Grid 36-92")
plot(bbs_cross_scales$scaleID[bbs_cross_scales$grid8ID == "36-76"], bbs_cross_scales$occupancy[bbs_cross_scales$grid8ID == "36-76"], xlab = "grain", ylab = "mean occ", main = "Grid 36-76")
plot(bbs_cross_scales$scaleID[bbs_cross_scales$grid8ID == "36-108"], bbs_cross_scales$occupancy[bbs_cross_scales$grid8ID == "36-108"], xlab = "grain", ylab = "mean occ", main = "Grid 36-108")
