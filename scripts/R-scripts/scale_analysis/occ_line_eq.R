#Occupancy-scale analysis

##feat. alternative to curve-fitting parameters: slope, intercept, and x value @ scale of 3 aggregated routes.
# author: Molly F. Jenkins
# date: 06/27/2017

# setwd("C:/git/core-transient")
#'#' Please download and install the following packages:
library(raster)
library(maps)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)
library(fields)
library(tidyr)
library(ggplot2)
library(nlme)
library(gridExtra)
library(wesanderson)
library(stats)


# To run this script, you need temperature, precip, etc data, 
# which are currently stored in the following directories off of github: 

# Data directories
tempdatadir = '//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/BIOCLIM_meanTemp/'
precipdata = '//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/2-25-2011/prec/'
ndvidata = "//bioark.ad.unc.edu/HurlbertLab/GIS/MODIS NDVI/"
BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/'


####Below-route occupancy calculations####
#need to happen first so can use the 50-scale occ 
#calculated for each route as the base to aggregate for the above-scale calcs

fifty_allyears = read.csv(paste(BBS, "fifty_allyears.csv", sep = ""), header = TRUE)
fifty_bestAous = fifty_allyears %>% 
  filter(AOU > 2880 & !(AOU >= 3650 & AOU <= 3810) & !(AOU >= 3900 & AOU <= 3910) & 
           !(AOU >= 4160 & AOU <= 4210) & AOU != 7010) #leaving out owls, waterbirds as less reliable data

#occ_counts function for calculating occupancy at any scale
occ_counts = function(countData, countColumns, scale) {
  bbssub = countData[, c("stateroute", "year", "AOU", countColumns)]
  bbssub$groupCount = rowSums(bbssub[, countColumns])
  bbsu = unique(bbssub[bbssub[, "groupCount"]!= 0, c("stateroute", "year", "AOU")]) 
  
  abun.summ = bbssub %>% #abundance
    group_by(stateroute, year) %>%  
    summarize(totalN = sum(groupCount)) %>%
    group_by(stateroute) %>%
    summarize(aveN = mean(totalN))
  
  occ.summ = bbsu %>% #occupancy
    count(stateroute, AOU) %>%
    mutate(occ = n/15, scale = scale, subrouteID = countColumns[1]) %>%
    group_by(stateroute) %>%
    summarize(meanOcc = mean(occ), 
              pctCore = sum(occ > 2/3)/length(occ),
              pctTran = sum(occ <= 1/3)/length(occ)) %>%
    #spRichTrans33  
    # spRichTrans25 = sum(occ <= 1/4)/length(occ),
    # spRichTrans10 = sum(occ <= 0.1)/length(occ)) %>%
    mutate(scale = paste(scale, g, sep = "-")) %>%
    left_join(abun.summ, by = 'stateroute')
  return(occ.summ)
}


# Generic calculation of occupancy for a specified scale
b_scales = c(5, 10, 25, 50)

output = c()
for (scale in b_scales) {
  numGroups = floor(50/scale)
  for (g in 1:numGroups) {
    groupedCols = paste("Stop", ((g-1)*scale + 1):(g*scale), sep = "")
    temp = occ_counts(fifty_bestAous, groupedCols, scale)
    output = rbind(output, temp) 
  }
}
bbs_below<-data.frame(output)
#write.csv(bbs_below, paste(BBS, "bbs_below.csv", sep = ""), row.names = FALSE) #updated 06/30, on BioArk
#should be able to use the 50 stop info (1 rte) from this output to aggregate routes AFTER below scale

####Calculations for Occupancy above the scale of a BBS route####
#Revised calcs workspace 
#sort out bbs_below to ONLY those routes at 50-stop scale (occ calc'd for a single route)

#use to aggregate 
good_rtes2 = read.csv(paste(BBS, "good_rtes2.csv", sep = ""), header = TRUE) 
require(fields)
#Distance calculation between all combination of routes to pair them by min dist for aggregation
distances = rdist.earth(matrix(c(good_rtes2$Longi, good_rtes2$Lati), ncol=2),
                        matrix(c(good_rtes2$Longi, good_rtes2$Lati), ncol=2),
                        miles=FALSE, R=6371)
dist.df = data.frame(rte1 = rep(good_rtes2$stateroute, each = nrow(good_rtes2)),
                     rte2 = rep(good_rtes2$stateroute, times = nrow(good_rtes2)),
                     dist = as.vector(distances))
#write.csv(dist.df, "C:/git/core-transient/scripts/R-scripts/scale_analysis/dist_df.csv", row.names = FALSE) for later calcs

bbs_fullrte = bbs_below %>%
  filter(scale == "50-1") #953 routes at scale of a single route

#go one step at a time, logically -> don't rush thru recreating the loop 

#need to make sure NOT running thru 66 times on the same site and scale 
uniqrtes = unique(bbs_fullrte$stateroute) #all routes present are unique, still 953 which is great
numrtes = 1:65 # based on min common number in top 6 grid cells, see grid_sampling_justification script 
output = data.frame(focalrte2 = NULL,
                    numrtes2 = NULL, 
                    meanOcc2 = NULL,       
                    pctCore2 = NULL,  
                    pctTran2 = NULL, 
                    totalAbun2 = NULL,  
                    maxRadius2 = NULL)


for (r in uniqrtes) { #for each focal route
  for (nu in numrtes) { #for each level of scale aggregated to each focal route
    
    tmp_rte_group = dist.df %>% 
      filter(rte1 == r) %>% 
      top_n(66, desc(dist)) %>% #fixed ordering by including arrange parm
      arrange(dist)
    
    nu_group = tmp_rte_group %>% 
      top_n(nu, desc(dist)) %>% #narrow to how many routes to aggregate occ across
      select(rte2) %>% as.vector()
    
    bbssub = bbs_fullrte %>%
      filter(stateroute %in% nu_group$rte2) #stateroute = rte2 group in nu_group (routes to agg across!!!) should be nu rows 
    
    
    #bbsuniq = unique(bbssub[, c('Aou', 'Year')])
    #occs = bbssub %>% dplyr::count(Aou) %>% dplyr::mutate(occ = n/15) #already have occs! just need to accumulate and avg them 
    
    #adding 2 to end since using an input df with all of the exact same column names -> can change back b4 merging, after loop
    temp = data.frame(focalrte2 = r,
                      numrtes2 = nu, #total # routes being aggregated -> do I really need the +1 if it's already inclusive of the 1st?
                      meanOcc2 = mean(bbssub$meanOcc, na.rm =T),       #mean occupancy
                      pctCore2 = mean(bbssub$pctCore, na.rm = T), #how do I want to do this? avg of routes aggregated, or recalc? 
                      pctTran2 = mean(bbssub$pctTran, na.rm = T), #fraction of species that are transient
                      totalAbun2 = sum(bbssub$aveN),  #total community size (per year) already calc'd per route....so just add across routes?
                      maxRadius2 = tmp_rte_group$dist[nu])   
    
    output = rbind(output, temp)
    print(paste("Focal rte", r, "#' rtes sampled", nu)) #for viewing progress
    
  } #n loop
  
} #r loop

bbs_above_v2 = as.data.frame(output)
#Calc area for above route scale
bbs_above_v2$area = bbs_above_v2$numrtes*50*(pi*(0.4^2)) #number of routes * fifty stops * area in sq km of a stop 
#write.csv(bbs_above_v2, paste(BBS, "bbs_above_v2.csv", sep = ""), row.names = FALSE)
#updated 06/30 evening locally and on BioArk; not sure why data folder rejected bc not THAT big but not on github

####scale-joining####
bbs_above = read.csv(paste(BBS, "bbs_above_v2.csv", sep = ""), header = TRUE)
bbs_below = read.csv(paste(BBS, "bbs_below.csv", sep = ""), header = TRUE)

#adding maxRadius column to bbs_below w/NA's + renaming and rearranging columns accordingly, creating area cols
bbs_below = bbs_below %>% 
  mutate(maxRadius = c("NA")) %>%
  dplyr::rename(focalrte = stateroute) %>%
  select(focalrte, scale, everything()) %>%
  mutate(area = (as.integer(lapply(strsplit(as.character(bbs_below$scale), 
                                            split="-"), "[", 1)))*(pi*(0.4^2))) 
#modify and split scale so that it's just the # of stops in each seg; not the seg order # preceded by a "-"


bbs_above = bbs_above %>% 
  dplyr::rename(scale = numrtes2, aveN = totalAbun2, focalrte = focalrte2, meanOcc = meanOcc2, 
                pctCore = pctCore2, pctTran = pctTran2, maxRadius = maxRadius2) #%>%
#this already done above  
#mutate(area = scale*50*(pi*(0.4^2))) #area in km by # of routes * 50 stops in each rte * area of a stop (for above-route scale later)
bbs_above$scale = as.factor(bbs_above$scale)


bbs_allscales = rbind(bbs_below, bbs_above) #rbind ok since all share column names
#write.csv(bbs_allscales, "C:/git/core-transient/scripts/R-scripts/scale_analysis/bbs_allscales.csv", row.names = FALSE)
#updated 07/02/2017

####Occ-scale analysis####