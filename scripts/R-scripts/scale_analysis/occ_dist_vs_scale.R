#Variation in occupancy at multiple scales WITHIN & ABOVE BBS sites
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

fifty_allyears = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/filteredrtes.csv", header = TRUE)
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

#write.csv(bbs_scalesorted, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scalesorted.csv", row.names = FALSE)

bbs_scalesorted = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scalesorted.csv", header = TRUE)


##locating and ID-ing stateroutes contained within a given grid cell (since disappear in process of deriving occ)##

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
#write.csv(stateroute_latlon, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/stateroute_latlon.csv", row.names = FALSE)

#count # of stateroutes in each cell, take top 6 (for both sub and above-route occupancy)
require(dplyr) 
grid_rte_totals = stateroute_latlon %>% 
  count(grid8ID) %>% 
  arrange(desc(n)) 
#intentionally allowing "X" column to be created for ease of selection of top 6 cells 
#write.csv(grid_rte_totals, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/grid_rte_totals.csv")
grid_rte_totals = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/grid_rte_totals.csv")
grid_rtes_best = grid_rte_totals %>% 
  filter(grid_rte_totals$X < 7) #taking top six grids 
grid_rtes_best$grid8ID = as.character(grid_rtes_best$grid8ID)

grid_rtes_best$area = grid_rtes_best$n*50*(pi*(0.4^2)) 
#area in km by # of routes * 50 stops * area of a stop (for above-route scale later)
#write.csv(grid_rtes_best, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/grid_rtes_best.csv") 
#wrote to file for later use in cross-scale merge


bbs_scalesorted = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scalesorted.csv", header = TRUE)
#scale corresponds to # of stops in a segment 
#add area ID BEFORE merging, same with above-route dataset 
bbs_scalesorted$area = (bbs_scalesorted$scale)*(pi*(0.4^2)) # area in km by area of a BBS segment based on # of stops in that segment (for now)


#I DO want to join this time because I want the stateroute lat-long info, 
#so I know which bins the stateroutes can be paired up in 

bbs_bigsmall = inner_join(bbs_scalesorted, stateroute_latlon, by = c("stateroute" = "stateroute")) 
bbs_bigsmall$scale = paste("0.00", bbs_bigsmall$scale, sep = "")
bbs_bigsmall$scaleID = bbs_bigsmall$scale
bbs_bigsmall$lat = bbs_bigsmall$Lati
bbs_bigsmall$lon = bbs_bigsmall$Longi


#before joining datasets OR getting rid of variables -> calc mean of means for bbs_bigsmall 
#determining the mean of means across reps and then across scales for below a bbs route 

subrte_occ_avgs = bbs_bigsmall %>% group_by(lat, lon, scaleID, stateroute) %>% #adding stateroute as proxy for rep to grouping
  summarize(mean = mean(occupancy)) %>% #calc across all AOUs for each stateroute (bc stateroutes ARE reps)
  group_by(lat, lon, scaleID) %>% #calc across all stateroutes for each scale 
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

#------------------------------------------------------------------------------------------

####prototype forloop for generating scaled-up samples for calculating occupancy####

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

#-----------------------------------------------------------------------------------------
####Combining sub and above-route scale analyses outputs for comparison####
#write.csv(occ_avgs, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/occ_avgs.csv", row.names = FALSE)
#use grid8ID specified in grid_rtes_best to subset occ_avgs for only those that match grid8ID
#then can compare across increasing grain size, across area 
occ_avgs = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/occ_avgs.csv")
grid_rtes_best = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/grid_rtes_best.csv")


pre_sub_occ_avgs = occ_avgs %>% 
  filter(grid8ID %in% grid_rtes_best$grid8ID) 
   #want to retain area column from grid_rtes_best 
  #based on # of stateroutes in each grid, based on grid center....so I think I want to join and select tbh 


sub_occ_avgs = inner_join(pre_sub_occ_avgs, grid_rtes_best, by = "grid8ID") %>%
  dplyr::select(lat, lon, grain, mean, grid8ID, area)

#check with unique to make sure 6 cells correct ====> it's correct => checktest = unique(sub_occ_avgs$grid8ID)

sub_occ_avgs$grain = paste("0.0", sub_occ_avgs$grain, sep = "")
sub_occ_avgs$scaleID = sub_occ_avgs$grain

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


####Map occ ~ grain at each of the six sample collections#### 
#write.csv(bbs_cross_scales, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_cross_scales.csv", row.names = FALSE)
bbs_cross_scales = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_cross_scales.csv")
#is 54KB small enough to fit on git? can I back it up there? 


bbs_cross_scales$log_area = log(bbs_cross_scales$area)

par(mfrow = c(2, 3))
plot(bbs_cross_scales$log_area[bbs_cross_scales$grid8ID == "44-76"], bbs_cross_scales$mean[bbs_cross_scales$grid8ID == "44-76"], xlab = "log(area)", ylab = "mean occ", main = "Grid 44-76")
plot(bbs_cross_scales$log_area[bbs_cross_scales$grid8ID == "36-84"], bbs_cross_scales$mean[bbs_cross_scales$grid8ID == "36-84"], xlab = "log(area)", ylab = "mean occ", main = "Grid 36-84")
plot(bbs_cross_scales$log_area[bbs_cross_scales$grid8ID == "44-92"], bbs_cross_scales$mean[bbs_cross_scales$grid8ID == "44-92"], xlab = "log(area)", ylab = "mean occ", main = "Grid 44-92")
plot(bbs_cross_scales$log_area[bbs_cross_scales$grid8ID == "36-92"], bbs_cross_scales$mean[bbs_cross_scales$grid8ID == "36-92"], xlab = "log(area)", ylab = "mean occ", main = "Grid 36-92")
plot(bbs_cross_scales$log_area[bbs_cross_scales$grid8ID == "36-76"], bbs_cross_scales$mean[bbs_cross_scales$grid8ID == "36-76"], xlab = "log(area)", ylab = "mean occ", main = "Grid 36-76")
plot(bbs_cross_scales$log_area[bbs_cross_scales$grid8ID == "36-108"], bbs_cross_scales$mean[bbs_cross_scales$grid8ID == "36-108"], xlab = "log(area)", ylab = "mean occ", main = "Grid 36-108")


#simple linear model exploring this relationship 
mod = lm(log_area~mean, data = bbs_cross_scales)
summary(mod) #explains ~1/2 of the variation, what happens when we intro NDVI? 

#where are these grid cells on a map of the US? which ones are which? 

bbs_cross_scales$grid8ID = as.character(bbs_cross_scales$grid8ID)


####Quickly bringing in proto NDVI data to cross-scale model####

#just need stateroute_latlon file filtered to relevant 6 grids 

grid_rtes_best = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/grid_rtes_best.csv", header = TRUE)
grid_rtes_best$grid8ID = as.character(grid_rtes_best$grid8ID)
stateroute_latlon = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/stateroute_latlon.csv", header = TRUE)
stateroute_latlon$grid8ID = as.character(stateroute_latlon$grid8ID)

raster_cropdims = stateroute_latlon %>%
  filter(grid8ID %in% grid_rtes_best$grid8ID) %>% 
  dplyr::select(latbin, longbin, grid8ID)

#calculating edge lats and lons of grid from center -> shift 4 degrees on either side

raster_cropdims$xmin = raster_cropdims$latbin - 4
raster_cropdims$xmax = raster_cropdims$latbin + 4
raster_cropdims$ymin = raster_cropdims$longbin - 4
raster_cropdims$ymax = raster_cropdims$longbin + 4

#bringing in ndvi and trimming using raster and crop 
#do I HAVE to for each grid cell, one at a time? I can't just reference the x and y columns I made? 

unique(raster_cropdims$grid8ID, raster_cropdims$xmin, raster_cropdims$xmax)

ndvimean<-raster("//bioark.ad.unc.edu/HurlbertLab/GIS/MODIS NDVI/Vegetation_Indices_may-aug_2000-2010.gri")
e1 = extent(32, -88, 40, -96) #x1, y1, x2, y2 based on + or - 4 degrees from grid8ID center point
ndvi_e1 = crop(ndvimean, e1)
#Error in validityMethod(object) : invalid extent: xmin >= xmax

#####mapping occ avgs across US####
#make a map where grid centers size is dictated by avg occs (do areas further out west have lower avg occs?)
#label with grid cell numbers for comparison with graphs 


map('state')
points(bbs_cross_scales$lon, bbs_cross_scales$lat, 
       cex = 3*bbs_cross_scales$mean, pch = 16)


#leg_benchmarks = c(2, max(ct$n)/2, max(ct$n))
#legend("bottomright", legend = c(2, (log10(bbs_cross_scales$mean))/2, log10(bbs_cross_scales$mean)), pch = 16)
#pt.cex = log10(leg_benchmarks))

#might have to correct lat + lon within a projection? 
#how to lump across grid8 cells? 


####Find dimensions of grids based on center points and grid sizes####

unique(bbs_cross_scales$grid8ID) 
#crop raster to dims to set extent within each grid 
#(as opposed to extracting ndvi for center lat and lon POINTS as above)


