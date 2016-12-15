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
#exclude AOU species codes <=2880 [waterbirds, shorebirds, etc], (>=3650 & <=3810) [owls],
#(>=3900 &  <=3910) [kingfishers], (>=4160 & <=4210) [nightjars], 7010 [dipper]
#^best practices 
bbs_bestAous = bbs_allyears %>% 
  filter(Aou > 2880 | Aou < 3650 | Aou > 3810 | Aou < 3900 | Aou > 3910 | Aou < 4160 | Aou > 4210 | Aou != 7010) 

#I think this may have already been done on the bbs data in the ecoretriever cache? 

numrtes = 1:65 #
output = data.frame(r = NULL, nu = NULL, AOU = NULL, occ = NULL)
for (r in uniqrtes) {
  for (nu in numrtes) {
  tmp = filter(dist.df2, rte1 == r) %>%
    arrange(dist)
  tmprtes = tmp$rte2[1:nu]   #selects rtes to aggregate under focal route by dist from focal route, based on nu in numrtes range
  # Aggregate those routes together, calc occupancy, etc
  
  bbssub = filter(bbs_bestAous, stateroute %in% c(r, tmprtes))
  bbsuniq = unique(bbssub[, c('Aou', 'Year')])
  occs = bbsuniq %>% dplyr::count(Aou) %>% dplyr::mutate(occ = n/15)
  
  temp = data.frame(focalrte = r,
                    numrtes = nu+1,                           #total # routes being aggregated
                    meanOcc = mean(occs$occ, na.rm =T),       #mean occupancy
                    pctCore = sum(occs$occ > 2/3)/nrow(occs), #fraction of species that are core
                    pctTran = sum(occs$occ <= 1/3)/nrow(occs),#fraction of species that are transient
                    totalAbun = sum(bbssub$SpeciesTotal)/15,  #total community size (per year)
                    maxRadius = tmp$dist[nu])                 #radius including rtes aggregated
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
####Calc area####

bbs_focal_occs$area = bbs_focal_occs$numrtes*50*(pi*(0.4^2)) #in km 
# number of routes * fifty stops * area in sq km of a stop 

####Occupancy vs area/# rtes####

plot(bbs_focal_occs$numrtes, bbs_focal_occs$meanOcc, xlab = "# routes", ylab = "mean occupancy")
par(mfrow = c(2, 1))
plot(bbs_focal_occs$numrtes, bbs_focal_occs$pctTran, xlab = "# routes", ylab = "% Trans")
plot(bbs_focal_occs$numrtes, bbs_focal_occs$pctCore, xlab = "# routes", ylab = "% Core")

#still just at above route scale tho - now need to stitch above and below together again 


####Find lat/lons of focal routes, add env data, color code points####
routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')
routes$stateroute = 1000*routes$statenum + routes$Route




####rerun sub-route occ analysis####

fifty_allyears = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/fifty_allyears.csv", header = TRUE)

fifty_allyears2 = fifty_allyears %>% 
  filter(AOU > 2880 & !(AOU >= 3650 & AOU <= 3810) & !(AOU >= 3900 & AOU <= 3910) & 
           !(AOU >= 4160 & AOU <= 4210) & AOU != 7010) 
###So for the whole dataset, 10 pt count stops: #we are only getting one out of five chunks along 
#want to estimate occupancy across each one, as of now only estimating for count 10 column 
#fifty pt count data and then taking pts 1-5 and collapsing them all together 
#########
occ_counts = function(countData, countColumns, scale) {
  bbssub = countData[, c("stateroute", "year", "AOU", countColumns)]
  bbssub$groupCount = rowSums(bbssub[, countColumns])
  bbsu = unique(bbssub[bbssub[, "groupCount"]!= 0, c("stateroute", "year", "AOU")]) #because this gets rid of 0's...
  
  occ.df = bbsu %>%
    count(stateroute, AOU) %>%
    mutate(occ = n/15, scale = scale, subrouteID = countColumns[1])
    
  occ.summ = occ.df %>%
    group_by(stateroute) %>%
    summarize(meanOcc = mean(occ), 
              pctCore = sum(occ > 2/3)/length(occ),
              pctTran = sum(occ <= 1/3)/length(occ))
  
  abun = bbssub %>% 
    group_by(stateroute, year) %>%  
    summarize(totalN = sum(groupCount)) %>%
    group_by(stateroute) %>%
    summarize(aveN = mean(totalN))
  
    
      
    
  return(list(occ = occ.summ, abun = abun))
}
    bbsu.rt.occ = data.frame(table(bbsu[,c("stateroute", "AOU")])/15)
    bbsu.rt.occ2 = bbsu.rt.occ[bbsu.rt.occ$Freq!=0,] #and this also gets rid of occupancy values of 0 total 
    names(bbsu.rt.occ2)[3] = "occupancy"
    # avg abun for each AOU for each year @ each stateroute (diff than presence absence!)
    bbsu.rt.occ2$subrouteID = countColumns[1] #subrouteID refers to first stop in a grouped sequence, occ refers to the occ for the # of combined stops
    bbsu.rt.occ2$scale = scale 
    bbsu.rt.occ2$abun = (abun$n/15)
    #bbsu.rt.occ2$AOU = AOU #is it going to know to match up the AOU values from both occ and abun?
    bbsu.rt.occ2 = bbsu.rt.occ2[, c("stateroute", "scale", "subrouteID", "AOU", "occupancy", "abun")]
    return(bbsu.rt.occ2)
  }
  
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
    temp = occ_counts(fifty_allyears2, groupedCols, scale, calcAbund = TRUE)
    output = rbind(output, temp)
  }
  
}

bbs_scalesorted<-output

#calc mean occ, abundance, % core and % trans across stateroute, AOU, and subroute ID cluster for each scale 

test_meanocc = bbs_scalesorted %>% 
  group_by(scale, stateroute, subrouteID) %>% #occ across all AOU's, for each unique combo of rte, scale(segment length), and starting segment
  summarize(mean = mean(occupancy)) %>% 
  group_by(scale, stateroute) %>%
  summarize(mean_occ = mean(mean)) 


test_meanabun = bbs_scalesorted %>% 
  group_by(scale, stateroute, subrouteID) %>%
  summarize(abun = mean(abun)) %>%
  group_by(scale, stateroute) %>%
  summarize(mean_ab = mean(abun)) 


pctCore = sum(test_meanocc$mean > .67)/nrow(test_meanocc) #fraction of species that are core
pctTran = sum(test_meanocc$mean <= .33)/nrow(test_meanocc)

#should do ^ for each scale

#how to accumulate "reps" or "numrtes" equiv in below-rte scale accordingly? 

bbs_focal_occs = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_focal_occs.csv", header = TRUE)
