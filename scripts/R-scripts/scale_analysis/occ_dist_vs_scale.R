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


#count # of stateroutes in each cell, take top 6
require(dplyr) 
grid_rte_totals = stateroute_latlon %>% count(grid8ID) %>% arrange(desc(n)) 


#write.csv(grid_rte_totals, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/grid_rte_totals.csv")

grid_rte_totals = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/grid_rte_totals.csv")
#intentionally allowing "X" column to be created for ease of selection of top 6 cells 
grid_rtes_best = grid_rte_totals %>% 
  filter(grid_rte_totals$X < 7) #taking top six grids only for state routes to dictate sample

grid_rtes_best$area = grid_rtes_best$n*50*(pi*(0.4^2)) #area in km by # of routes * 50 stops * area of a stop (for later)


bbs_scalesorted = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scalesorted.csv", header = TRUE)
#scale corresponds to # of stops in a segment 
#add area ID BEFORE merging, same with above-route dataset 
bbs_scalesorted$area = (bbs_scalesorted$scale)*(pi*(0.4^2)) # area in km by area of a BBS segment based on # of stops in that segment (for now)


#I DO want to join this time because I want the stateroute lat-long info, 
#so I know which bins the stateroutes can be paired up in 

bbs_bigsmall = inner_join(bbs_scalesorted, stateroute_latlon, by = c("stateroute" = "stateroute")) 
bbs_bigsmall$scale = paste("0.00", bbs_bigsmall$scale, sep = "")
bbs_bigsmall$scaleID = bbs_bigsmall$scale
bbs_bigsmall$siteID = bbs_bigsmall$stateroute 


bbs_bigsmall$sub_supr_rteID = bbs_bigsmall$subrouteID
bbs_bigsmall$lat = bbs_bigsmall$Lati
bbs_bigsmall$lon = bbs_bigsmall$Longi



#before joining datasets OR getting rid of variables -> calc mean of means for bbs_bigsmall 
#determining the mean of means across reps and then across scales for below a bbs route 

subrte_occ_avgs = bbs_bigsmall %>% group_by(lat, lon, scaleID, stateroute) %>% #adding stateroute as proxy for rep to grouping
  summarize(mean = mean(occupancy)) %>% #calc across all AOUs (leaving out Aou as grouping variable)
  group_by(lat, lon, scaleID) %>% #calc across all stateroutes for each scale (because stateroutes ARE the reps in this case...?)
  summarize(mean = mean(mean))

#do I need to take mean of means here? or do I already have one mean and just need to take one more 
#instead of two here, and thus preserve stateroute? 
#bc when calc occupancy in original function, derive it across years for all AOU's.... 
#so don't I just need to take mean across AOU's and then scales for each stateroute? 


##### get Sara's feedback and help 
#not sure what to do about "rep" since did not run separate reps for beneath-route calcs? 
#should I reconstruct WITH a rep and rerun? NO!!!!
#reps were for pulling different combinations of stateroutes within each grid cell 
#because we weren't going to use ALL of the stateroutes in each grid cell 
#because that # varied by cell! wanted to hold constant 
#so sampled 
#below route scale didn't need that 
#so we calc mean by stateroute in lieu of rep and scaleID, I think that's reasonable 
#pull addition of lat lon and scaleID variables earlier on in script 


#pull back in following variables:
#siteID<-necessary?, sub_supr_rteID <-necessary?, RENAME MEAN -> occupancy, grid8ID <- get thru lat + lon, scaleID, lat, lon, area <-get thru grid8ID 
#for later cross-scale join

#so re-merge based on bbs_bigsmall area, lat, lon, and scaleID to get areas of diff segments 
#(will have multiple same areas for diff occs, lats, lons, and same scaleIDs)

#pare bbs_bigmsall down to just important variables for next join 

bbs_prejoin = bbs_bigsmall %>%
  dplyr::select(lat, lon, scaleID, grid8ID, area)

test_join = inner_join(subrte_occ_avgs, bbs_prejoin)
#I think this worked? mean of means across stateroutes nested within those grid cells 
#but with occ calc'd BELOW route level before lumped together by cell 
#area = area of stop segment based on scaleID and lat lon of original stateroute


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

#-------------------------------------------------------------------
#use grid8ID specified in grid_rtes_best to subset occ_avgs for only those that match grid8ID
#then can compare across increasing grain size 
occ_avgs = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/occ_avgs.csv")

pre_sub_occ_avgs = occ_avgs %>% 
  filter(grid8ID %in% grid_rtes_best$grid8ID) 
   #want to retain area column from grid_rtes_best 
  #based on # of stateroutes in each grid, based on grid center....so I think I want to join and select tbh 


sub_occ_avgs = inner_join(pre_sub_occ_avgs, grid_rtes_best, by = "grid8ID") %>%
  dplyr::select(lat, lon, grain, mean, grid8ID, area)

#check with unique to make sure 6 cells correct ====> it's correct 
#checktest = unique(sub_occ_avgs$grid8ID)





####Stitch lower scale analyses in using stateroute_latlon file to designate lower scales within their bins####
##below a bbs route: bbs_scalesorted

 


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

sub_occ_avgs$grain = paste("0.0", sub_occ_avgs$grain, sep = "")

sub_occ_avgs$scaleID = sub_occ_avgs$grain

#scale ID NEEDS to be chr or num, not integer, otherwise 0's will be removed 
#altho it seems to look ok 
#hmmm says chr currently, not sure why switched during the join 
#try putting a . in front otherwise 


#in sub_occ for the larger scales, instead of stateroute I can have the unrounded lat_lon paired and rename it siteID? 

#sub_occ_avgs$siteID = paste(sub_occ_avgs$lat, sub_occ_avgs$lon, sep = "")


#sub_occ_avgs$occupancy = sub_occ_avgs$mean    #renaming occ at above route scale to correspond with occ 

#also keep lat lon info for bbs_bigsmall, don't select it out -> or can I get rid of lat lon in sub occ 
#since now have unique ID
#R won't let me, so I guess I'm keeping lat lons in bbs_bigsmall because easier 
#just make sure columns are still "lat", "lon" 




#and sub_supr_rteID corresponds to the grid8ID, so need it copied into a second column - one used for nesting**
#and one used purely for labeling the above route ID -> confirm idea?

#sub_occ_avgs$sub_supr_rteID = sub_occ_avgs$grid8ID



#paring down datasets to only relevant corresponding variables 

bbs_test_join = test_join %>% 
  dplyr::select(mean, grid8ID, scaleID, lat, lon, area)

sub_occ_avgs = sub_occ_avgs %>% 
  dplyr::select(mean, grid8ID, scaleID, lat, lon, area)

#both datasets should have 7 corresponding variables 

#make sure variables joining by match in class type! so ID's should be chr vectors 

sub_occ_avgs$grid8ID = as.character(sub_occ_avgs$grid8ID)


#joining datasets -> bbs_bigsmall with 866748 rows, occ_avgs with 205 rows, should add up to 866953

bbs_cross_scales = full_join(bbs_test_join, sub_occ_avgs)

#error message BUT adds up to 866953! Hooray! 

#write.csv(bbs_cross_scales, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_cross_scales.csv", row.names = FALSE)


bbs_cross_scales = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_cross_scales.csv")

#is 54KB small enough to fit on git? can I back it up there? 

#need to fix or add-on area column that is based on scaleID 

#bbs_cross_scales$scale_Area = c(0)

#need to make a table that takes stop segment and grid cell size and matches it with scaleID and a scale area

scale_table = data.frame("scaleID" = unique(bbs_cross_scales$scaleID))

####Map occ ~ grain at each of the six sample collections#### 

par(mfrow = c(2, 3))
plot(bbs_cross_scales$log_area[bbs_cross_scales$grid8ID == "44-76"], bbs_cross_scales$mean[bbs_cross_scales$grid8ID == "44-76"], xlab = "log(area)", ylab = "mean occ", main = "Grid 44-76")
plot(bbs_cross_scales$log_area[bbs_cross_scales$grid8ID == "36-84"], bbs_cross_scales$mean[bbs_cross_scales$grid8ID == "36-84"], xlab = "log(area)", ylab = "mean occ", main = "Grid 36-84")
plot(bbs_cross_scales$log_area[bbs_cross_scales$grid8ID == "44-92"], bbs_cross_scales$mean[bbs_cross_scales$grid8ID == "44-92"], xlab = "log(area)", ylab = "mean occ", main = "Grid 44-92")
plot(bbs_cross_scales$log_area[bbs_cross_scales$grid8ID == "36-92"], bbs_cross_scales$mean[bbs_cross_scales$grid8ID == "36-92"], xlab = "log(area)", ylab = "mean occ", main = "Grid 36-92")
plot(bbs_cross_scales$log_area[bbs_cross_scales$grid8ID == "36-76"], bbs_cross_scales$mean[bbs_cross_scales$grid8ID == "36-76"], xlab = "log(area)", ylab = "mean occ", main = "Grid 36-76")
plot(bbs_cross_scales$log_area[bbs_cross_scales$grid8ID == "36-108"], bbs_cross_scales$mean[bbs_cross_scales$grid8ID == "36-108"], xlab = "log(area)", ylab = "mean occ", main = "Grid 36-108")

#need to log transform area but SO FAR SO GOOD :~D 

bbs_cross_scales$log_area = log(bbs_cross_scales$area)

#make a map where grid centers size is dictated by avg occs (do areas further out west have lower avg occs?)
#label with grid cell number's for comparison with graphs 

#mapping occ avgs across US
map('state')
points(bbs_cross_scales$lon, bbs_cross_scales$lat, 
       cex = log10(bbs_cross_scales$mean), pch = 16)
#leg_benchmarks = c(2, max(ct$n)/2, max(ct$n))
#legend("bottomright", legend = c(2, (log10(bbs_cross_scales$mean))/2, log10(bbs_cross_scales$mean)), pch = 16)
#pt.cex = log10(leg_benchmarks))

#might have to correct lat + lon within a projection? 

