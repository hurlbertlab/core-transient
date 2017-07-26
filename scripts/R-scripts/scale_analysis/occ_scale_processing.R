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

fifty_allyears = read.csv(paste(BBS, "fifty_allyears.csv", sep = ""), header = TRUE) #using updated version, 50 stop data, 07/12
fifty_bestAous = fifty_allyears %>% 
  filter(AOU > 2880 & !(AOU >= 3650 & AOU <= 3810) & !(AOU >= 3900 & AOU <= 3910) & 
           !(AOU >= 4160 & AOU <= 4210) & AOU != 7010) #leaving out owls, waterbirds as less reliable data

#occ_counts function for calculating occupancy at any scale
occ_counts = function(countData, countColumns, scale) {
  bbssub = countData[, c("stateroute", "year", "AOU", countColumns)] #these are our grouping vars
  bbssub$groupCount = rowSums(bbssub[, countColumns]) 
  bbsu = unique(bbssub[bbssub[, "groupCount"]!= 0, c("stateroute", "year", "AOU")]) 
  
  abun.summ = bbssub %>% #abundance
    group_by(stateroute, year) %>%  
    summarize(totalN = sum(groupCount)) %>%
    group_by(stateroute) %>%
    summarize(aveN = mean(totalN)) #we want to go further and summarize across focal + secondary rtes tho
  
  occ.summ = bbsu %>% #occupancy
    count(stateroute, AOU) %>%
    mutate(occ = n/15, scale = scale, subrouteID = countColumns[1]) %>%
    group_by(stateroute) %>%
    summarize(meanOcc = mean(occ), 
              pctCore = sum(occ > 2/3)/length(occ),
              pctTran = sum(occ <= 1/3)/length(occ)) %>%
    mutate(scale = paste(scale, g, sep = "-")) %>%
    left_join(abun.summ, by = 'stateroute')
  return(occ.summ)
}


# Generic calculation of occupancy for a specified scale
#fix to run all at once, so no sep run for above-scale, USE occ-counts for both 

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
write.csv(bbs_below, paste(BBS, "bbs_below.csv", sep = ""), row.names = FALSE) #updated 06/30, on BioArk
#should be able to use the 50 stop info (1 rte) from this output to aggregate routes AFTER below scale
write.csv(bbs_below, "data/BBS/bbs_below.csv", row.names = FALSE)

#at scale of a single route (e.g. "50-1", no communities)


####Calculations for Occupancy above the scale of a BBS route####
#Revised calcs workspace 
#sort out bbs_below to ONLY those routes at 50-stop scale (occ calc'd for a single route)
#use to aggregate 
good_rtes2 = read.csv(paste(BBS, "good_rtes2.csv", sep = ""), header = TRUE) #using updated version, 07/12
require(fields)
#Distance calculation between all combination of routes to pair them by min dist for aggregation
distances = rdist.earth(matrix(c(good_rtes2$Longi, good_rtes2$Lati), ncol=2),
                        matrix(c(good_rtes2$Longi, good_rtes2$Lati), ncol=2),
                        miles=FALSE, R=6371)
dist.df = data.frame(rte1 = rep(good_rtes2$stateroute, each = nrow(good_rtes2)),
                     rte2 = rep(good_rtes2$stateroute, times = nrow(good_rtes2)),
                     dist = as.vector(distances))
write.csv(dist.df, "scripts/R-scripts/scale_analysis/dist_df.csv", row.names = FALSE) #for later calcs


####Rte loop####
dist.df = read.csv("scripts/R-scripts/scale_analysis/dist_df.csv", header = TRUE)
bbs_below = read.csv(paste(BBS, "bbs_below.csv", sep = ""), header = TRUE)
bbs_fullrte = bbs_below %>%
  filter(scale == "50-1") %>%
  select(stateroute, meanOcc, aveN) #953 routes at scale of a single route

#go one step at a time, logically -> don't rush thru recreating the loop 

#need to make sure NOT running thru 66 times on the same site and scale 
uniqrtes = unique(bbs_fullrte$stateroute) #all routes present are unique, still 953 which is great
numrtes = 2:66 # based on min common number in top 6 grid cells, see grid_sampling_justification script 
output = data.frame(focalrte = NULL,
                    scale = NULL, 
                    aveN = NULL,
                    meanOcc = NULL, 
                    pctCore = NULL,
                    pctTran = NULL,
                    maxRadius = NULL)


for (r in uniqrtes) { #for each focal route
  for (nu in numrtes) { #for each level of scale aggregated to each focal route
    
    tmp_rte_group = dist.df %>% 
      filter(rte1 == r) %>% 
      top_n(66, desc(dist)) %>% #fixed ordering by including arrange parm, 
      #remove/skip top row 
      arrange(dist) %>%
      slice(1:nu) %>% 
      select(everything()) %>% data.frame()
    
    focal_clustr = bbs_fullrte %>% 
      inner_join(tmp_rte_group, by = c("stateroute" = "rte2"))%>%
      arrange(dist)
      #(for a given focal rte, narrow input data to those 66 secondary routes in focal cluster)
    
    # Use below code to inform occ calc appropriately
    
    
    # occ_counts = function(countData, countColumns, scale) {
    #   bbssub = countData[, c("stateroute", "year", "AOU", countColumns)] #these are our grouping vars
    #   bbssub$groupCount = rowSums(bbssub[, countColumns]) 
    #   bbsu = unique(bbssub[bbssub[, "groupCount"]!= 0, c("stateroute", "year", "AOU")]) 
    #   
    #   abun.summ = bbssub %>% #abundance
    #     group_by(stateroute, year) %>%  
    #     summarize(totalN = sum(groupCount)) %>%
    #     group_by(stateroute) %>%
    #     summarize(aveN = mean(totalN)) #we want to go further and summarize across focal + secondary rtes tho
    #   
    #   occ.summ = bbsu %>% #occupancy
    #     count(stateroute, AOU) %>%
    #     mutate(occ = n/15, scale = scale, subrouteID = countColumns[1]) %>%
    #     group_by(stateroute) %>%
    #     summarize(meanOcc = mean(occ), 
    #               pctCore = sum(occ > 2/3)/length(occ),
    #               pctTran = sum(occ <= 1/3)/length(occ)) %>%
    #     mutate(scale = paste(scale, g, sep = "-")) %>%
    #     left_join(abun.summ, by = 'stateroute')
    #   return(occ.summ)
    
    
    
    occ.summ = focal_clustr %>% 
      summarize(aveN2 = sum(aveN), 
                meanOcc2 = mean(Occ), 
                pctCore2 = sum(Occ > 2/3)/length(Occ),
                pctTran2 = sum(Occ <= 1/3)/length(Occ), 
                maxRadius = max(dist)) 
    
    #now - how to cycle thru agg occ calcs from 2-66? w/in list? 
    #create scale variable corresponding to num rows/routes pulled into occ calc
    #tmp_rte_group is effectively our sub for "count columns" at the above-rte scale 
    #START with a community occ and abun already calc'd for each individual rte
        temp = data.frame(focalrte = r,
                          scale = nu, #total # routes being aggregated -> do I really need the +1 if it's already inclusive of the 1st?
                          meanOcc = occ.summ$meanOcc2, 
                          pctCore = occ.summ$pctCore2,
                          pctTran = occ.summ$pctTran2,
                          aveN = occ.summ$aveN2, 
                          maxRadius = occ.summ$maxRadius)   
        
        output = rbind(output, temp)
        print(paste("Focal rte", r, "#' rtes sampled", nu)) #for viewing progress
    
    #adding 2 to end since using an input df with all of the exact same column names -> can change back b4 merging, after loop
   
  } #n loop
  
} #r loop
# I can then feed the above into my occ_counts function 
# may need to transpose rows to columns 


bbs_above = as.data.frame(output)
#Calc area for above route scale
#bbs_above$area = bbs_above_v2$numrtes*50*(pi*(0.4^2)) #number of routes * fifty stops * area in sq km of a stop 
write.csv(bbs_above, paste(BBS, "bbs_above.csv", sep = ""), row.names = FALSE)
#updated 07/20 evening locally and on BioArk; not sure if data folder will reject on git
write.csv(bbs_above, "data/BBS/bbs_above.csv", row.names = FALSE)
#updated 07/20

####scale-joining####
bbs_above = read.csv(paste(BBS, "bbs_above.csv", sep = ""), header = TRUE)
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
  dplyr::mutate(area = scale*50*(pi*(0.4^2))) #area in km by # of routes * 50 stops in each rte * area of a stop (for above-route scale later)
bbs_above$scale = as.factor(bbs_above$scale)


bbs_allscales = rbind(bbs_below, bbs_above) #rbind ok since all share column names
write.csv(bbs_allscales, "data/BBS/bbs_allscales.csv", row.names = FALSE)
#updated 07/20/2017, also in BioArk since old copy ALSO there
write.csv(bbs_allscales, paste(BBS, "bbs_allscales.csv", sep = ""), row.names = FALSE)

####filter out stateroutes that are one-sided in scale####
#in terms of their representation of below vs above scale (should have both, not one alone)

bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
bbs_allscales$logA = log10(bbs_allscales$area)
bbs_allscales$logN = log10(bbs_allscales$aveN)
bbs_allscales$lnA = log(bbs_allscales$area) #log is the natural log 
bbs_allscales$lnN = log(bbs_allscales$aveN) #rerun plots with this?

#only want rtes w/all 83 scales rep'd, which at this point - there are! 
bbs_allscales2 = bbs_allscales %>% filter(meanOcc != 'NA') %>% 
  count(focalrte) %>% filter(n == 83) %>% data.frame() #fix error to exclude NAs
bbs_allscales3 = filter(bbs_allscales, focalrte %in% bbs_allscales2$focalrte)

#Order levels of scale factor post-join####
#fix duplication of scale (50-1, 1 -> check up)

bbs_allscales3$scale = factor(bbs_allscales3$scale, 
                             levels = c('5-1', '5-2', '5-3', '5-4', '5-5', '5-6', '5-7', '5-8', '5-9', '5-10',
                                        '10-1', '10-2', '10-3', '10-4', '10-5', '25-1', '25-2', '50-1', 
                                        '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12',
                                        '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24',
                                        '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36',
                                        '37', '38', '39', '40', '41', '42', '43', '44', '45', '46', '47', '48',
                                        '49', '50', '51', '52', '53', '54', '55', '56', '57', '58', '59', '60',
                                        '61', '62', '63', '64', '65', '66'), ordered=TRUE)

write.csv(bbs_allscales3, "data/BBS/bbs_allscales.csv", row.names = FALSE) #overwrote bbs all scales file 
#updated 07/24/2017 from 1003 to 1001 routes


####Occ-scale analysis####
####Cross-scale analysis and visualization####
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
levels(bbs_allscales$scale)
unique(bbs_allscales$scale)


mod1 = lm(meanOcc~logA, data = bbs_allscales) #expljkains ~50% of the variation in occ
mod2 = lm(meanOcc~logN, data = bbs_allscales)
summary(mod1)

plot(meanOcc~logA, data = bbs_allscales, xlab = "Log Area" , ylab = "Mean Temporal Occupancy")
plot(meanOcc~logN, data = bbs_allscales, xlab = "Average Abundance" , ylab = "Mean Temporal Occupancy")
#^^same pattern roughly; abundance describes ~same amt of variance as area so serves as a good proxy 


#ALL files updated 07/20 ~4pm 


