###80% cutoff and bbs_allscales50 & core_coefs50 generation 

####bbs_allscales#### 
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
library(viridis)

# To run this script, you need temperature, precip, etc data, 
# which are currently stored in the following directories off of github: 

# Data directories
BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/Intermediate scripts/BBS scaled/'


#modify below code to rerun for bbs below 

occ_counts2 = function(countData, countColumns, scale) {
  bbssub = countData[, c("stateroute", "year", "AOU", countColumns)] #these are our grouping vars
  bbssub$groupCount = rowSums(bbssub[, countColumns]) 
  bbsu = unique(bbssub[bbssub[, "groupCount"]!= 0, c("stateroute", "year", "AOU", "groupCount")])
  return(bbsu)
}


fifty_allyears = read.csv(paste(BBS, "fifty_allyears.csv", sep = ""), header = TRUE) #using updated version, 50 stop data, 07/12
fifty_bestAous = fifty_allyears %>% 
  filter(AOU > 2880 & !(AOU >= 3650 & AOU <= 3810) & !(AOU >= 3900 & AOU <= 3910) & 
           !(AOU >= 4160 & AOU <= 4210) & AOU != 7010) #leaving out owls, waterbirds as less reliable data

#should just return data for 50-1 scale, across all 50 stops 
c_scales = c(5, 10, 25, 50) #just doing for one for now -> need to fix and expand to full selection
output = c()
for (scale in c_scales) {
  numGroups = floor(50/scale)
  for (g in 1:numGroups) {
    groupedCols = paste("Stop", ((g-1)*scale + 1):(g*scale), sep = "")
    temp = occ_counts2(fifty_bestAous, groupedCols, scale)
    temp$scale = scale
    temp$seg = g #added segment specifier to aid in recalc
    output = rbind(output, temp) 
  }
}

bbs_below_guide = data.frame(output)
write.csv(bbs_below_guide, paste(BBS, "bbs_below_guide.csv", sep = ""), row.names = FALSE)

####Paring down to 968 routes####
dist.df = read.csv("scripts/R-scripts/scale_analysis/intermed/dist_df.csv", header = TRUE)
bbs_below_guide = read.csv(paste(BBS, "bbs_below_guide.csv", sep = ""), header = TRUE)
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
        dplyr::select(year, AOU) %>% #duplicates remnant of distinct secondary routes - finally ID'd bug
        distinct() %>% #removing duplicates 09/20
        count(AOU) %>% #how many times does that AOU show up in that clustr that year 
        mutate(occ = n/15, scale = nu) %>% #, subrouteID = countColumns[1]) #%>% countColumns not needed bc already pared down
        # group_by(r) %>% #don't want to group by stateroute though! want to calc for whole clustr
        summarize(focalrte = r, 
                  scale = nu, 
                  meanOcc = mean(occ), #FIX 09/19 across vector of AOU occupancies for AOU mean
                  pctCore = sum(occ > 4/5)/length(occ),
                  pctTran = sum(occ <= 1/5)/length(occ)) 
      
      
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


bbs_below80 = as.data.frame(output)
#write.csv(bbs_below, paste(BBS, "bbs_below_new.csv", sep = ""), row.names = FALSE)

#NOW I can average for unique scale-route combo (currently duplicates based on segments)

bbs_below_avgs_80 = bbs_below80 %>% 
  group_by(focalrte, scale) %>% 
  summarize(meanOcc = mean(meanOcc), 
            pctCore = mean(pctCore), 
            pctTran = mean(pctTran), 
            aveN = mean(aveN))

write.csv(bbs_below_avgs_80, paste(BBS, "bbs_below_avgs_80.csv", sep = ""), row.names = FALSE)
# write.csv(bbs_below_avgs, "data/BBS/bbs_below_avgs.csv", row.names = FALSE)
# successfully stored both avgs and new in bioark and data folder since small enough


####Above-scale and merging code####
####Calculating occupancy scales 2:66 loop####
dist.df = read.csv("scripts/R-scripts/scale_analysis/intermed/dist_df.csv", header = TRUE)
bbs_above_guide = read.csv("scripts/R-scripts/scale_analysis/intermed/bbs_above_guide.csv", header = TRUE)
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
      dplyr::select(year, AOU) %>% #duplicates remnant of distinct secondary routes - finally ID'd bug
      distinct() %>% #removing duplicates 09/20
      count(AOU) %>% #how many times does that AOU show up in that clustr that year 
      mutate(occ = n/15, scale = nu) %>% #, subrouteID = countColumns[1]) #%>% countColumns not needed bc already pared down
      # group_by(r) %>% #don't want to group by stateroute though! want to calc for whole clustr
      summarize(focalrte = r, 
                scale = nu, 
                meanOcc = mean(occ), #FIX 09/19 across vector of AOU occupancies for AOU mean
                pctCore = sum(occ > 4/5)/length(occ),
                pctTran = sum(occ <= 1/5)/length(occ), 
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


bbs_above_80 = as.data.frame(output)
#Calc area for above route scale
#bbs_above$area = bbs_above_v2$numrtes*50*(pi*(0.4^2)) #number of routes * fifty stops * area in sq km of a stop 
write.csv(bbs_above_80, paste(BBS, "bbs_above_80.csv", sep = ""), row.names = FALSE)
#updated 09/20 evening locally and on BioArk; not sure if data folder will reject on git
#write.csv(bbs_above, "data/BBS/bbs_above.csv", row.names = FALSE)
#updated 09/20




####Merging across scales####
bbs_above = read.csv(paste(BBS, "bbs_above_80.csv", sep = ""), header = TRUE)
bbs_below = read.csv(paste(BBS, "bbs_below_avgs_80.csv", sep = ""), header = TRUE)

#adding maxRadius column to bbs_below w/NA's + renaming and rearranging columns accordingly, creating area cols
bbs_below2 = bbs_below %>% 
  mutate(maxdist = c("NA")) %>%
  dplyr::select(focalrte, scale, everything()) %>%
  mutate(area = bbs_below$scale*(pi*(0.4^2)), 
         scale = paste("seg", scale, sep = ""))

#modify and split scale so that it's just the # of stops in each seg; not the seg order # preceded by a "-"


bbs_above = bbs_above %>% 
  dplyr::mutate(area = scale*50*(pi*(0.4^2))) %>% #area in km by # of routes * 50 stops in each rte * area of a stop (for above-route scale later)
  dplyr::select(focalrte, scale, meanOcc, pctCore, pctTran, aveN, maxdist, area) #%>% 
# filter(scale == "2")

bbs_above$scale = as.factor(bbs_above$scale)


bbs_allscales80 = rbind(bbs_below2, bbs_above) #rbind ok since all share column names

bbs_allscales80$logA = log10(bbs_allscales80$area)
bbs_allscales80$logN = log10(bbs_allscales80$aveN)
bbs_allscales80$lnA = log(bbs_allscales80$area) #log is the natural log 
bbs_allscales80$lnN = log(bbs_allscales80$aveN)

write.csv(bbs_allscales80, paste(BBS, "bbs_allscales80.csv", sep = ""), row.names = FALSE) #saved 03/12
write.csv(bbs_allscales80, "data/BBS/bbs_allscales80.csv", row.names = FALSE)


####core_coefs80####

#read in data for processing
bbs_allscales = read.csv(paste(BBS, "bbs_allscales80.csv", sep = ""), header = TRUE)
levels(bbs_allscales$scale)
unique(bbs_allscales$scale)
length(unique(bbs_allscales$focalrte))
bbs_allscales = na.omit(bbs_allscales) #from 66792 to 66792 when maxdist left out so 
#oh we DO want to cut out the below-route stuff bc we can't do the env analyses on these period
length(unique(bbs_allscales$focalrte)) #968 rtes, 62920 obs


PCA.df = data.frame(stateroute = numeric(), PCA.min = numeric(), PCA.max = numeric(), 
                    PCA.slope = numeric(), 
                    PCA.mid = numeric(), 
                    PCA.curvature = numeric())
PCN.df = data.frame(stateroute = numeric(), PCN.min = numeric(), PCN.max = numeric(), 
                    PCN.slope = numeric(), 
                    PCN.mid = numeric(), 
                    PCN.curvature = numeric())


####coefs####
stateroutes = unique(bbs_allscales$focalrte)

#do I even need a loop? can't I just group by stateroute and calc these ?

for(s in stateroutes){
  logsub = subset(bbs_allscales, bbs_allscales$focalrte == s)  
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
core_coefs80 = PCA.df %>% 
  inner_join(PCN.df, PCA.df, by = "stateroute") %>% distinct()

write.csv(core_coefs80, "scripts/R-scripts/scale_analysis/core_coefs80.csv", row.names = FALSE) 
#updated 4/11, removal of redundant coefs and inclusion of ON, revised curvature est

