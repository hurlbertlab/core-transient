#Variation in occupancy at multiple scales WITHIN & ABOVE BBS sites
#REVISED ANALYSIS
#Molly F. Jenkins 
#11/11/2016

#skip to line 236 for current revision stopping point


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


bbs50 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs50.csv", header = TRUE)
# merge lat longs from routes file to the list of "good" routes (2000-2014 present all years)
require(dplyr)
good_rtes2 = good_rtes %>% 
  left_join(routes, good_rtes, by = "stateroute") %>%
  dplyr::select(stateroute, Lati, Longi)
#write.csv(good_rtes2, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes2.csv", row.names = FALSE)

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
          occs = bbsuniq %>% dplyr::count(Aou) %>% mutate(occ = n/15)
          occs = data.frame(occs)
          
          bbs_abun = bbssub %>%
            group_by(Aou, Year) %>%
            dplyr::summarize(sum(SpeciesTotal))
          bbs_abun = data.frame(bbs_abun)  
          
          # to get unique list of stateroutes and aous for bbs abundance
          bbs_abun = bbs_abun[row.names(unique(bbs_abun[,c('Aou', 'Year')])),]
        
          all_info = merge(bbs_abun, occs, by = "Aou")
          # Calc community size from bbssub, then join to occs below
          temp = data.frame(grain = grain, 
                            lat = lat, 
                            lon = lon, 
                            rep = i,
                            Aou = all_info$Aou,
                            occ = all_info$occ,
                            abun = all_info$sum.SpeciesTotal.)
          
          output = rbind(output, temp)
          print(paste("Grain", grain, ", Lat:", lat, ", Lon:", lon))
        } #end of the rep loop
      } 
    } #end of the lon loop
    
  } #end of the lat loop
  
} #end of the grain loop



bbs_scaledup = output    #wrote to file in case
# write.csv(bbs_scaledup, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scaledup.csv", row.names = FALSE)
totalspp = bbs_scaledup %>% 
  group_by(Aou, grain) %>%
  tally(abun)
for(i in bbs_abun$AOU){
  sum(bbs_abun$occupancy <= 1/3)/(totalspp$n)
}
bbs_scaledup$spptally = 1 
pctTrans = sum(bbs_scaledup$occ <= 1/3)/sum(bbs_scaledup$spptally)

mod3 = lm(bbs_scaledup$occ ~ log10(bbs_scaledup$abun))
xnew = range(log10(bbs_scaledup$abun))
xhat <- predict(mod3, newdata = data.frame((xnew)))
xhats = range(xhat)
print(xhats)


####Mean of means for above-route scales####
bbs_scaledup = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_scaledup.csv")

#-----------------------------------------------------------------------------------------
####Combining sub and above-route scale analyses outputs for comparison####
#this is the part that got messy in the original .R file 
#use grid8ID specified in grid_rtes_best to subset occ_avgs for only those that match grid8ID
#then can compare across increasing grain size, across area 

#scale corresponds to # of stops in a segment 
#add area ID BEFORE merging, same with above-route dataset 
bbs_scalesorted$area = (bbs_scalesorted$scale)*(pi*(0.4^2)) # area in km by area of a BBS segment based on # of stops in that segment (for now)
bbs_scalesorted$scale = paste("seg", bbs_scalesorted$scale, sep = "")
bbs_scalesorted$scale = as.character(bbs_scalesorted$scale)
bbs_scalesorted$lat = bbs_scalesorted$Lati
bbs_scalesorted$lon = bbs_scalesorted$Longi
bbs_scalesorted$grid8ID = paste(floor(bbs_scalesorted$lat/8)*8 + 8/2, floor(bbs_scalesorted$lon/8)*8 + 8/2, sep = "")
bbs_scalesorted$grid8ID = as.character(bbs_scalesorted$grid8ID)

#subset to six grid cells creates by grain_sample aka those existing in occ_avgs already
#totally unnecessary if areas and scales standardized, don't need to nest within "top" grids, 
#right? 

bbs_scalesorted = bbs_scalesorted %>% 
  filter(grid8ID %in% occ_avgs$grid8ID)

#before joining datasets OR getting rid of variables -> calc mean of means for bbs_bigsmall 
#determining the mean of means across reps and then across scales for below a bbs route 

subrte_occ_avgs = bbs_scalesorted %>% 
  group_by(lat, lon, scale, stateroute) %>% #adding stateroute as proxy for rep to grouping
  summarize(mean = mean(occupancy)) %>% #calc across all AOUs for each stateroute (bc stateroutes ARE reps)
  group_by(lat, lon, scale) %>% #calc across all stateroutes for each scale 
  summarize(mean = mean(mean))

#pull back in following variables:
#grid8ID, scaleID, lat, lon, area <-get thru grid8ID bc linked
#for later cross-scale join
bbs_prejoin = bbs_scalesorted %>%
  dplyr::select(lat, lon, scale, grid8ID, area) %>% 
  filter(grid8ID %in% sample_sizes$gridID) #fixed, subsetted to top 6 grid cells only 

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







    
    
 #grain_sample = data.frame(c(1, 2, 4, 8), c(1, 4, 10, 25)) <- old sample sizes for comparison



