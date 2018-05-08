# Alternate cutoffs of core and transient species in a community: are distributions changed?
# author: Molly F. Jenkins
# updated: 05/03/2017

#The way the probability densities are calculated, they are just taking temporal occupancy as it is 
#and not looking at a specific cutoff - it's literally just the data. 
#You could draw the cutoffs at any point and would still have a bimodal distribution until it skewed unimodal on one extreme or the other 

#The following script demonstrates the cutoffs of 50% 
#This script explores cutoffs of: 
#2/4 & 2/4 (original occ-scale-processing and creation of bbs_allscales.csv data)
#The supplemental cutoff scripts can be viewed in allscales_coefs50.R and allscales_coefs80.R, original in core_scale_analyses.R

###################################################################################################################
####Original and working derivation 2/4 & 1/4 cutoffs####
# setwd("C:/git/core_scale")
#'#' Please download and install the following packages:
library(raster)
library(tidyverse)
library(fields)
library(maps)
library(sp)
library(rgdal)
library(maptools)
library(rgeos)
library(nlme)
library(gridExtra)
library(wesanderson)
library(stats)
library(viridis)

# To run this script, you need temperature, precip, etc data, 
# which are currently stored in the following directories off of github: 

# Data directories
BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/Intermediate scripts/BBS scaled/'


#modify below code to rerun for bbs below 

occ_counts2 = function(countData, countColumns, scale) {
  bbssub = countData[, c("stateroute", "year", "aou", countColumns)] #these are our grouping vars
  bbssub$groupCount = rowSums(bbssub[, countColumns]) 
  bbsu = unique(bbssub[bbssub[, "groupCount"]!= 0, c("stateroute", "year", "aou", "groupCount")])
  return(bbsu)
}


fifty_allyears = read.csv("intermed/fifty_allyears.csv", header = TRUE) #using updated version, 50 stop data, 07/12
fifty_bestAous = fifty_allyears %>% 
  filter(aou > 2880 & !(aou >= 3650 & aou <= 3810) & !(aou >= 3900 & aou <= 3910) & 
           !(aou >= 4160 & aou <= 4210) & aou != 7010) #leaving out owls, waterbirds as less reliable data

#should just return data for 50-1 scale, across all 50 stops 
c_scales = c(5, 10, 25, 50) #just doing for one for now -> need to fix and expand to full selection
output = c()
for (scale in c_scales) {
  numGroups = floor(50/scale)
  for (g in 1:numGroups) {
    groupedCols = paste("stop", ((g-1)*scale + 1):(g*scale), sep = "")
    temp = occ_counts2(fifty_bestAous, groupedCols, scale)
    temp$scale = scale
    temp$seg = g #added segment specifier to aid in recalc
    output = rbind(output, temp) 
  }
}

bbs_below_guide = data.frame(output)
write.csv(bbs_below_guide, "intermed/bbs_below_guide.csv", row.names = FALSE)

####Data prep for calculating occupancy above the scale of a BBS route####
#Revised calcs workspace 
#sort out bbs_below to ONLY those routes at 50-stop scale (occ calc'd for a single route)
#use to aggregate 
good_rtes2 = read.csv("intermed/good_rtes2.csv", header = TRUE) #using updated version, 05/03
require(fields)
#Distance calculation between all combination of routes to pair them by min dist for aggregation
distances = rdist.earth(matrix(c(good_rtes2$longitude, good_rtes2$latitude), ncol=2),
                        matrix(c(good_rtes2$longitude, good_rtes2$latitude), ncol=2),
                        miles=FALSE, R=6371)
dist.df = data.frame(rte1 = rep(good_rtes2$stateroute, each = nrow(good_rtes2)),
                     rte2 = rep(good_rtes2$stateroute, times = nrow(good_rtes2)),
                     dist = as.vector(distances))
write.csv(dist.df, "intermed/dist_df.csv", row.names = FALSE) #for later calcs

#occ_counts2
#important to not remove AOU and stateroute data by year, but to halt at that step 
#so can be guided thru original occ_counts, with secondary routes as "countColumns" 
#have to gen new function

occ_counts2 = function(countData, countColumns, scale) {
  bbssub = countData[, c("stateroute", "year", "aou", countColumns)] #these are our grouping vars
  bbssub$groupCount = rowSums(bbssub[, countColumns]) 
  bbsu = unique(bbssub[bbssub[, "groupCount"]!= 0, c("stateroute", "year", "aou", "groupCount")])
  return(bbsu)
}


fifty_allyears = read.csv("intermed/fifty_allyears.csv", header = TRUE) #using updated version, 50 stop data, 07/12
fifty_bestAous = fifty_allyears %>% 
  filter(aou > 2880 & !(aou >= 3650 & aou <= 3810) & !(aou >= 3900 & aou <= 3910) & 
           !(aou >= 4160 & aou <= 4210) & aou != 7010) #leaving out owls, waterbirds as less reliable data

#should just return data for 50-1 scale, across all 50 stops 
c_scales = c(50)
output = c()
for (scale in c_scales) {
  numGroups = floor(50/scale)
  for (g in 1:numGroups) {
    groupedCols = paste("stop", ((g-1)*scale + 1):(g*scale), sep = "")
    temp = occ_counts2(fifty_bestAous, groupedCols, scale)
    output = rbind(output, temp) 
  }
}

bbs_above_guide = data.frame(output)
write.csv(bbs_above_guide, "intermed/bbs_above_guide.csv", row.names = FALSE)

####Paring bbs_below down to 983 routes####
#updated 05/03/2017, along with all code preceding
dist.df = read.csv("intermed/dist_df.csv", header = TRUE)
bbs_below_guide = read.csv("intermed/bbs_below_guide.csv", header = TRUE)
#groupcounts for each AOU for each year at scale of ONE stateroute 

#filter out to only routes that are up to 1000km radius away from each other before analyses 
far = dist.df %>% arrange(rte1, dist) %>% group_by(rte1) %>% slice(66)
hist(far$dist)
far2 = far %>% filter(dist < 1000)

bbs_below_guide = bbs_below_guide %>% filter(stateroute %in% far2$rte1)

#tested, works 
test = bbs_below_guide %>% filter(scale == "25")
unique(test$seg)
#[1] 1 2; correct, 25 stop scale should only have two segments per route


#I can group by scale and segment and THEN take means of segments
require(tidyverse)
#need to make sure NOT running thru 66 times on the same site and scale 
uniqrtes = unique(bbs_below_guide$stateroute) #all routes present are unique, still 953 which is great
scale = unique(bbs_below_guide$scale)
rte_segments = unique(bbs_below_guide$seg)

output = data.frame(focalrte = NULL,
                    scale = NULL, 
                    meanOcc = NULL, 
                    pctCore = NULL,
                    pctTran = NULL,
                    aveN = NULL)

#test example route 2010 and nu at 57 routes -> large scale, should have high occ 
for (r in uniqrtes) { #for each focal route
  for (nu in scale) { #for each level of scale aggregated to each focal route
    #does rte segments need to change w/every scale? 
    focal_c = bbs_below_guide %>% 
      filter(stateroute == r & scale == nu)
    
    for (s in unique(focal_c$seg)) {
      
      focal_clustr = focal_c %>% 
        filter(seg == s) #tmp_rte_group already ordered by distance so don't need 2x
      #(for a given focal rte, narrow input data to those nu secondary routes in focal cluster)
      #across 57 routes
      
      abun.summ = focal_clustr %>% #abundance
        group_by(year) %>%  #not grouping by stateroute bc it stops mattering 
        summarize(totalN = sum(groupCount)) %>%
        summarize(aveN = mean(totalN), 
                  stateroute = r)
      
      occ.summ = focal_clustr %>% #occupancy -> focal clustr should GROW with scale, larger avg pool -> 
        #increased likelihood that AOU will be present -> OH! I don't want stateroute in here! it doesn't matter! 
        #it just matters that it shows up in the cluster at all, not just the stateroutes that go in
        #how many years does each AOU show up in the cluster 
        dplyr::select(year, aou) %>% #duplicates remnant of distinct secondary routes - finally ID'd bug
        distinct() %>% #removing duplicates 09/20
        count(aou) %>% #how many times does that AOU show up in that clustr that year 
        mutate(occ = n/15, scale = nu) %>% #, subrouteID = countColumns[1]) #%>% countColumns not needed bc already pared down
        # group_by(r) %>% #don't want to group by stateroute though! want to calc for whole clustr
        summarize(focalrte = r, 
                  scale = nu, 
                  meanOcc = mean(occ), #FIX 09/19 across vector of AOU occupancies for AOU mean
                  pctCore = sum(occ > 2/4)/length(occ),
                  pctTran = sum(occ <= 2/4)/length(occ)) 
      
      
      occ2 = occ.summ %>% 
        group_by(focalrte) %>% 
        summarize(scale = nu, 
                  meanOcc = mean(meanOcc), #mean of means, community mean  
                  pctCore = mean(pctCore), 
                  pctTran = mean(pctTran)) %>%
        left_join(abun.summ, by = c('focalrte' = 'stateroute'))
      
      output = rbind(output, occ2)
      print(paste("Focal rte", r, "#' rtes sampled", nu)) #for viewing progress
      
      #adding 2 to end since using an input df with all of the exact same column names -> can change back b4 merging, after loop
    } #segment loop 
  } #n loop
  
} #r loop
# I can then feed the above into my occ_counts function 
# may need to transpose rows to columns 


bbs_below50 = as.data.frame(output)
write.csv(bbs_below50, "intermed/bbs_below50.csv", row.names = FALSE)

#NOW I can average for unique scale-route combo (currently duplicates based on segments)

bbs_below_avgs50 = bbs_below50 %>% 
  group_by(focalrte, scale) %>% 
  summarize(meanOcc = mean(meanOcc), 
            pctCore = mean(pctCore), 
            pctTran = mean(pctTran), 
            aveN = mean(aveN))

write.csv(bbs_below_avgs50, "intermed/bbs_below_avgs50.csv", row.names = FALSE)
#below scale avgs updated 05/03/2018

####Above-scale and merging code####
####Calculating occupancy scales 2:66 loop####
dist.df = read.csv("intermed/dist_df.csv", header = TRUE)
bbs_above_guide = read.csv("intermed/bbs_above_guide.csv", header = TRUE)
#groupcounts for each AOU for each year at scale of ONE stateroute 

#filter out to only routes that are up to 1000km radius away from each other before analyses 
far = dist.df %>% arrange(rte1, dist) %>% group_by(rte1) %>% slice(66)
hist(far$dist)
far2 = far %>% filter(dist < 1000)

bbs_above_guide = bbs_above_guide %>% filter(stateroute %in% far2$rte1)

#go one step at a time, logically -> don't rush thru recreating the loop 

#need to make sure NOT running thru 66 times on the same site and scale 
uniqrtes = unique(bbs_above_guide$stateroute) #all routes present are unique, still 953 which is great
numrtes = 2:66 # based on min common number in top 6 grid cells, see grid_sampling_justification script 
output = data.frame(focalrte = NULL,
                    scale = NULL, 
                    meanOcc = NULL, 
                    pctCore = NULL,
                    pctTran = NULL,
                    maxdist = NULL,
                    aveN = NULL)

#test example route 2010 and nu at 57 routes -> large scale, should have high occ 
for (r in uniqrtes) { #for each focal route
  for (nu in numrtes) { #for each level of scale aggregated to each focal route
    
    #takes dist.df and generates a new list that changes based on which route in uniqrtes is being focused on 
    #and the length of the list varies with the scale or nu 
    
    tmp_rte_group = dist.df %>% #changes with size of nu but caps at 66
      filter(rte1 == r) %>% 
      top_n(66, desc(dist)) %>% #fixed ordering by including arrange parm, 
      #remove/skip top row 
      arrange(dist) %>%
      slice(1:nu) %>% 
      dplyr::select(everything()) %>% data.frame()
    
    #takes varying list from above and uses it to subset the bbs data so that occ can be calculated for the cluster 
    
    focal_clustr = bbs_above_guide %>% 
      filter(stateroute %in% tmp_rte_group$rte2) #tmp_rte_group already ordered by distance so don't need 2x
    #(for a given focal rte, narrow input data to those nu secondary routes in focal cluster)
    #across 57 routes
    
    abun.summ = focal_clustr %>% #abundance
      group_by(year) %>%  #not grouping by stateroute bc it stops mattering 
      summarize(totalN = sum(groupCount)) %>%
      summarize(aveN = mean(totalN), 
                stateroute = r)
    
    occ.summ = focal_clustr %>% #occupancy -> focal clustr should GROW with scale, larger avg pool -> 
      #increased likelihood that AOU will be present -> OH! I don't want stateroute in here! it doesn't matter! 
      #it just matters that it shows up in the cluster at all, not just the stateroutes that go in
      #how many years does each AOU show up in the cluster 
      dplyr::select(year, aou) %>% #duplicates remnant of distinct secondary routes - finally ID'd bug
      distinct() %>% #removing duplicates 09/20
      count(aou) %>% #how many times does that AOU show up in that clustr that year 
      mutate(occ = n/15, scale = nu) %>% #, subrouteID = countColumns[1]) #%>% countColumns not needed bc already pared down
      # group_by(r) %>% #don't want to group by stateroute though! want to calc for whole clustr
      summarize(focalrte = r, 
                scale = nu, 
                meanOcc = mean(occ), #FIX 09/19 across vector of AOU occupancies for AOU mean
                pctCore = sum(occ > 2/4)/length(occ),
                pctTran = sum(occ <= 2/4)/length(occ), 
                maxdist = max(tmp_rte_group$dist)) 
    
    
    
    occ2 = occ.summ %>% 
      group_by(focalrte) %>% 
      summarize(scale = nu, 
                meanOcc = mean(meanOcc), #mean of means, community mean  
                pctCore = mean(pctCore), 
                pctTran = mean(pctTran), 
                maxdist = mean(maxdist)) %>%
      left_join(abun.summ, by = c('focalrte' = 'stateroute'))
    
    output = rbind(output, occ2)
    print(paste("Focal rte", r, "#' rtes sampled", nu)) #for viewing progress
    
    #adding 2 to end since using an input df with all of the exact same column names -> can change back b4 merging, after loop
    
  } #n loop
  
} #r loop
# I can then feed the above into my occ_counts function 
# may need to transpose rows to columns 


bbs_above50 = as.data.frame(output)
#Calc area for above route scale
#bbs_above$area = bbs_above_v2$numrtes*50*(pi*(0.4^2)) #number of routes * fifty stops * area in sq km of a stop 
write.csv(bbs_above50, "intermed/bbs_above50.csv", row.names = FALSE)
#updated 05/03 with everything immediately preceding 

####Merging across scales####
bbs_above50 = read.csv("intermed/bbs_above50.csv", header = TRUE)
bbs_below50 = read.csv("intermed/bbs_below_avgs50.csv", header = TRUE)

#adding maxRadius column to bbs_below w/NA's + renaming and rearranging columns accordingly, creating area cols
bbs_below2 = bbs_below50 %>% 
  mutate(maxdist = c("NA")) %>%
  dplyr::select(focalrte, scale, everything()) %>%
  mutate(area = bbs_below50$scale*(pi*(0.4^2)), 
         scale = paste("seg", scale, sep = ""))

#modify and split scale so that it's just the # of stops in each seg; not the seg order # preceded by a "-"


bbs_above = bbs_above50 %>% 
  dplyr::mutate(area = scale*50*(pi*(0.4^2))) %>% #area in km by # of routes * 50 stops in each rte * area of a stop (for above-route scale later)
  dplyr::select(focalrte, scale, meanOcc, pctCore, pctTran, aveN, maxdist, area) #%>% 
# filter(scale == "2")

bbs_above$scale = as.factor(bbs_above$scale)


bbs_allscales = rbind(bbs_below2, bbs_above) #rbind ok since all share column names

bbs_allscales$logA = log10(bbs_allscales$area)
bbs_allscales$logN = log10(bbs_allscales$aveN)
bbs_allscales$lnA = log(bbs_allscales$area) #log is the natural log 
bbs_allscales$lnN = log(bbs_allscales$aveN)

write.csv(bbs_allscales, "intermed/bbs_allscales50.csv", row.names = FALSE) #saved 05/03

####Revamped coef extraction loop for comparing differences in value distributions between cutoff categories####
####Extract coefficients from scale-occupancy relationships for analysis####

####normal cutoff of 67%#### 

#read in data for processing
bbs_allscales = read.csv("intermed/bbs_allscales50.csv", header = TRUE)
levels(bbs_allscales$scale)
unique(bbs_allscales$scale)
length(unique(bbs_allscales$focalrte))
bbs_allscales = na.omit(bbs_allscales) #from 66792 to 66792 when maxdist left out so 
#oh we DO want to cut out the below-route stuff bc we can't do the env analyses on these period
length(unique(bbs_allscales$focalrte)) #983 rtes, 62920 obs
bbs_allscales2 = bbs_allscales %>% dplyr::select(-scale, -maxdist)


PCA.df = data.frame(stateroute = numeric(), PCA.min = numeric(), PCA.max = numeric(), 
                    PCA.slope = numeric(), 
                    PCA.mid = numeric(), 
                    PCA.curvature = numeric())
PCN.df = data.frame(stateroute = numeric(), PCN.min = numeric(), PCN.max = numeric(), 
                    PCN.slope = numeric(), 
                    PCN.mid = numeric(), 
                    PCN.curvature = numeric())


####coefs####
stateroutes = unique(bbs_allscales2$focalrte)

#do I even need a loop? can't I just group by stateroute and calc these ?

for(s in stateroutes){
  logsub = subset(bbs_allscales2, bbs_allscales2$focalrte == s)  
  #PCA 
  #PCApred_df = data.frame(preds = predict(PCAlog), scale = logsub$scale, logA = logsub$logA)  #get preds -> is predicting unique per scale, all clear
  #ACTUAL stats (for plotting data pts): 
  PCA.min = logsub$pctCore[logsub$logA == min(logsub$logA)]
  PCA.max = logsub$pctCore[logsub$logA == max(logsub$logA)]
  PCA.mid = min(logsub$logA[logsub$pctCore >= 0.5]) 
  PCA.slope = ((PCA.max - PCA.min)/(max(logsub$logA) - min(logsub$logA)))
  #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
  #save as an area, not a "scale" 
  
  PCA.obline = logsub$pctCore #vector for a given focal rte s, actual values along the pos decel curve
  
  b = PCA.min -(PCA.slope*min(logsub$logA)) # b = y1 - m*x1
  
  PCA.pline = PCA.slope*logsub$logA+b #the vector of y values/occs that lie between the min and max in a straight line
  
  PCA.curvature = sum(PCA.obline-PCA.pline) 
  #AUC proxy - taking diff between actual and predicted mid vals at EVERY scale and adding together
  
  PCAmodel = data.frame(stateroute = s, PCA.min, PCA.max, PCA.slope, 
                        PCA.mid, PCA.curvature)
  
  PCA.df = rbind(PCA.df, PCAmodel)
  #
  
  #PCN 
  #PCNpred_df = data.frame(preds = predict(PCNlog), scale = logsub$scale, logN = logsub$logN)  #get preds -> is predicting unique per scale, all clear
  #ACTUAL stats (for plotting data pts): 
  PCN.min = logsub$pctCore[logsub$logN == min(logsub$logN)]
  PCN.max = logsub$pctCore[logsub$logN == max(logsub$logN)]
  PCN.mid = min(logsub$logN[logsub$pctCore >= 0.5]) 
  PCN.slope = ((PCN.max - PCN.min)/(max(logsub$logN) - min(logsub$logN)))
  #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
  #save as an area, not a "scale" 
  
  PCN.obline = logsub$pctCore #vector for a given focal rte s, actual values along the pos decel curve
  
  b2 = PCN.min -(PCN.slope*min(logsub$logN)) # b = y1 - m*x1
  
  PCN.pline = PCN.slope*logsub$logN+b2 #the vector of y values/occs that lie between the min and max in a straight line
  
  PCN.curvature = sum(PCN.obline-PCN.pline) 
  #NUC proxy - taking diff between actual and predicted mid vals at EVERY scale and adding together
  
  PCNmodel = data.frame(stateroute = s, PCN.min, PCN.max, 
                        PCN.slope, PCN.mid, PCN.curvature)
  
  PCN.df = rbind(PCN.df, PCNmodel) #
  
}  

#join all together using inner_join by focal rte, not cbind 
core_coefs50 = PCA.df %>% 
  inner_join(PCN.df, PCA.df, by = "stateroute") %>% distinct()

write.csv(core_coefs50, "intermed/core_coefs50.csv", row.names = FALSE) 
#updated 05/03, removal of redundant coefs and inclusion of ON, revised curvature est

