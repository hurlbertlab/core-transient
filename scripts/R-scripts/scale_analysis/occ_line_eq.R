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
#write.csv(bbs_below, "data/BBS/bbs_below.csv", row.names = FALSE) #updated 06/30
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
output = data.frame(r = NULL, nu = NULL, AOU = NULL, occ = NULL)


for (r in uniqrtes) { #for each focal route
  for (nu in numrtes) { #for each level of scale aggregated to each focal route
    
    tmp_rte_group = dist.df %>% 
      filter(rte1 == r) %>% 
      top_n(66, desc(dist)) %>% #fixed ordering by including arrange parm
      arrange(dist)
    
    
