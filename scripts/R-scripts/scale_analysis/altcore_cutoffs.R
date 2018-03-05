# Alternate cutoffs of core and transient species in a community: are distributions changed?
# author: Molly F. Jenkins
# date: 03/05/2017

#The way the probability densities are calculated, they are just taking temporal occupancy as it is 
#and not looking at a specific cutoff - it's literally just the data. 
#You could draw the cutoffs at any point and would still have a bimodal distribution until it skewed unimodal on one extreme or the other 

#The following script demonstrates 3 alternatives to the original cutoffs of >2/3 and <1/3, which can be viewed in the pctcore_alt.R script. 
#This script explores cutoffs of: 
#2/3 $ 1/3 (original occ-scale-processing and creation of bbs_allscales.csv data)
#3/4 & 1/4
#3/5 & 2/5 
#4/5 & 1/5
###################################################################################################################
####Original and working derivation 2/3 & 1/3 cutoffs####
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

#tested, works 
test = bbs_below_guide %>% filter(scale == "25")
unique(test$seg)
#[1] 1 2; correct, 25 stop scale should only have two segments per route


#I can group by scale and segment and THEN take means of segments

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
        select(year, AOU) %>% #duplicates remnant of distinct secondary routes - finally ID'd bug
        distinct() %>% #removing duplicates 09/20
        count(AOU) %>% #how many times does that AOU show up in that clustr that year 
        mutate(occ = n/15, scale = nu) %>% #, subrouteID = countColumns[1]) #%>% countColumns not needed bc already pared down
        # group_by(r) %>% #don't want to group by stateroute though! want to calc for whole clustr
        summarize(focalrte = r, 
                  scale = nu, 
                  meanOcc = mean(occ), #FIX 09/19 across vector of AOU occupancies for AOU mean
                  pctCore = sum(occ > 2/3)/length(occ),
                  pctTran = sum(occ <= 1/3)/length(occ)) 
      
      
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


bbs_below = as.data.frame(output)
#write.csv(bbs_below, paste(BBS, "bbs_below_new.csv", sep = ""), row.names = FALSE)

#NOW I can average for unique scale-route combo (currently duplicates based on segments)

bbs_below_avgs = bbs_below %>% 
  group_by(focalrte, scale) %>% 
  summarize(meanOcc = mean(meanOcc), 
            pctCore = mean(pctCore), 
            pctTran = mean(pctTran), 
            aveN = mean(aveN))

# write.csv(bbs_below_avgs, paste(BBS, "bbs_below_avgs.csv", sep = ""), row.names = FALSE)
# write.csv(bbs_below_avgs, "data/BBS/bbs_below_avgs.csv", row.names = FALSE)
# successfully stored both avgs and new in bioark and data folder since small enough


####Above-scale and merging code####
####Calculating occupancy scales 2:66 loop####
dist.df = read.csv("scripts/R-scripts/scale_analysis/intermed/dist_df.csv", header = TRUE)
bbs_above_guide = read.csv("scripts/R-scripts/scale_analysis/intermed/bbs_above_guide.csv", header = TRUE)
#groupcounts for each AOU for each year at scale of ONE stateroute 


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
      select(everything()) %>% data.frame()
    
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
      select(year, AOU) %>% #duplicates remnant of distinct secondary routes - finally ID'd bug
      distinct() %>% #removing duplicates 09/20
      count(AOU) %>% #how many times does that AOU show up in that clustr that year 
      mutate(occ = n/15, scale = nu) %>% #, subrouteID = countColumns[1]) #%>% countColumns not needed bc already pared down
      # group_by(r) %>% #don't want to group by stateroute though! want to calc for whole clustr
      summarize(focalrte = r, 
                scale = nu, 
                meanOcc = mean(occ), #FIX 09/19 across vector of AOU occupancies for AOU mean
                pctCore = sum(occ > 2/3)/length(occ),
                pctTran = sum(occ <= 1/3)/length(occ), 
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


bbs_above = as.data.frame(output)
#Calc area for above route scale
#bbs_above$area = bbs_above_v2$numrtes*50*(pi*(0.4^2)) #number of routes * fifty stops * area in sq km of a stop 
write.csv(bbs_above, paste(BBS, "bbs_above.csv", sep = ""), row.names = FALSE)
#updated 09/20 evening locally and on BioArk; not sure if data folder will reject on git
write.csv(bbs_above, "data/BBS/bbs_above.csv", row.names = FALSE)
#updated 09/20




####Merging across scales####
bbs_above = read.csv(paste(BBS, "bbs_above.csv", sep = ""), header = TRUE)
bbs_below = read.csv(paste(BBS, "bbs_below_avgs.csv", sep = ""), header = TRUE)

#adding maxRadius column to bbs_below w/NA's + renaming and rearranging columns accordingly, creating area cols
bbs_below = bbs_below %>% 
  mutate(maxdist = c("NA")) %>%
  select(focalrte, scale, everything()) %>%
  mutate(area = bbs_below$scale*(pi*(0.4^2)), 
         scale = paste("seg", scale, sep = ""))

#modify and split scale so that it's just the # of stops in each seg; not the seg order # preceded by a "-"


bbs_above = bbs_above %>% 
  dplyr::mutate(area = scale*50*(pi*(0.4^2))) %>% #area in km by # of routes * 50 stops in each rte * area of a stop (for above-route scale later)
  dplyr::select(focalrte, scale, meanOcc, pctCore, pctTran, aveN, maxdist, area) #%>% 
# filter(scale == "2")

bbs_above$scale = as.factor(bbs_above$scale)


bbs_allscales = rbind(bbs_below, bbs_above) #rbind ok since all share column names



##################################################################################################################
####3/4 & 1/4####

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

#tested, works 
test = bbs_below_guide %>% filter(scale == "25")
unique(test$seg)
#[1] 1 2; correct, 25 stop scale should only have two segments per route


#I can group by scale and segment and THEN take means of segments

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
        select(year, AOU) %>% #duplicates remnant of distinct secondary routes - finally ID'd bug
        distinct() %>% #removing duplicates 09/20
        count(AOU) %>% #how many times does that AOU show up in that clustr that year 
        mutate(occ = n/15, scale = nu) %>% #, subrouteID = countColumns[1]) #%>% countColumns not needed bc already pared down
        # group_by(r) %>% #don't want to group by stateroute though! want to calc for whole clustr
        summarize(focalrte = r, 
                  scale = nu, 
                  meanOcc = mean(occ), #FIX 09/19 across vector of AOU occupancies for AOU mean
                  pctCore = sum(occ > 3/4)/length(occ),
                  pctTran = sum(occ <= 1/4)/length(occ)) 
      
      
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


bbs_below = as.data.frame(output)
#write.csv(bbs_below, paste(BBS, "bbs_below_new.csv", sep = ""), row.names = FALSE)

#NOW I can average for unique scale-route combo (currently duplicates based on segments)

bbs_below_avgs = bbs_below %>% 
  group_by(focalrte, scale) %>% 
  summarize(meanOcc = mean(meanOcc), 
            pctCore = mean(pctCore), 
            pctTran = mean(pctTran), 
            aveN = mean(aveN))

# write.csv(bbs_below_avgs, paste(BBS, "bbs_below_avgs.csv", sep = ""), row.names = FALSE)
# write.csv(bbs_below_avgs, "data/BBS/bbs_below_avgs.csv", row.names = FALSE)
# successfully stored both avgs and new in bioark and data folder since small enough


####Above-scale and merging code####
####Calculating occupancy scales 2:66 loop####
dist.df = read.csv("scripts/R-scripts/scale_analysis/intermed/dist_df.csv", header = TRUE)
bbs_above_guide = read.csv("scripts/R-scripts/scale_analysis/intermed/bbs_above_guide.csv", header = TRUE)
#groupcounts for each AOU for each year at scale of ONE stateroute 


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
      select(everything()) %>% data.frame()
    
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
      select(year, AOU) %>% #duplicates remnant of distinct secondary routes - finally ID'd bug
      distinct() %>% #removing duplicates 09/20
      count(AOU) %>% #how many times does that AOU show up in that clustr that year 
      mutate(occ = n/15, scale = nu) %>% #, subrouteID = countColumns[1]) #%>% countColumns not needed bc already pared down
      # group_by(r) %>% #don't want to group by stateroute though! want to calc for whole clustr
      summarize(focalrte = r, 
                scale = nu, 
                meanOcc = mean(occ), #FIX 09/19 across vector of AOU occupancies for AOU mean
                pctCore = sum(occ > 3/4)/length(occ),
                pctTran = sum(occ <= 1/4)/length(occ), 
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


bbs_above = as.data.frame(output)
#Calc area for above route scale
#bbs_above$area = bbs_above_v2$numrtes*50*(pi*(0.4^2)) #number of routes * fifty stops * area in sq km of a stop 
write.csv(bbs_above, paste(BBS, "bbs_above.csv", sep = ""), row.names = FALSE)
#updated 09/20 evening locally and on BioArk; not sure if data folder will reject on git
write.csv(bbs_above, "data/BBS/bbs_above.csv", row.names = FALSE)
#updated 09/20




####Merging across scales####
bbs_above = read.csv(paste(BBS, "bbs_above.csv", sep = ""), header = TRUE)
bbs_below = read.csv(paste(BBS, "bbs_below_avgs.csv", sep = ""), header = TRUE)

#adding maxRadius column to bbs_below w/NA's + renaming and rearranging columns accordingly, creating area cols
bbs_below = bbs_below %>% 
  mutate(maxdist = c("NA")) %>%
  select(focalrte, scale, everything()) %>%
  mutate(area = bbs_below$scale*(pi*(0.4^2)), 
         scale = paste("seg", scale, sep = ""))

#modify and split scale so that it's just the # of stops in each seg; not the seg order # preceded by a "-"


bbs_above = bbs_above %>% 
  dplyr::mutate(area = scale*50*(pi*(0.4^2))) %>% #area in km by # of routes * 50 stops in each rte * area of a stop (for above-route scale later)
  dplyr::select(focalrte, scale, meanOcc, pctCore, pctTran, aveN, maxdist, area) #%>% 
# filter(scale == "2")

bbs_above$scale = as.factor(bbs_above$scale)


bbs_allscales = rbind(bbs_below, bbs_above) #rbind ok since all share column names

######################################################################################################################

####4/5 & 1/5#### 

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

#tested, works 
test = bbs_below_guide %>% filter(scale == "25")
unique(test$seg)
#[1] 1 2; correct, 25 stop scale should only have two segments per route


#I can group by scale and segment and THEN take means of segments

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
        select(year, AOU) %>% #duplicates remnant of distinct secondary routes - finally ID'd bug
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


bbs_below = as.data.frame(output)
#write.csv(bbs_below, paste(BBS, "bbs_below_new.csv", sep = ""), row.names = FALSE)

#NOW I can average for unique scale-route combo (currently duplicates based on segments)

bbs_below_avgs = bbs_below %>% 
  group_by(focalrte, scale) %>% 
  summarize(meanOcc = mean(meanOcc), 
            pctCore = mean(pctCore), 
            pctTran = mean(pctTran), 
            aveN = mean(aveN))

# write.csv(bbs_below_avgs, paste(BBS, "bbs_below_avgs.csv", sep = ""), row.names = FALSE)
# write.csv(bbs_below_avgs, "data/BBS/bbs_below_avgs.csv", row.names = FALSE)
# successfully stored both avgs and new in bioark and data folder since small enough


####Above-scale and merging code####
####Calculating occupancy scales 2:66 loop####
dist.df = read.csv("scripts/R-scripts/scale_analysis/intermed/dist_df.csv", header = TRUE)
bbs_above_guide = read.csv("scripts/R-scripts/scale_analysis/intermed/bbs_above_guide.csv", header = TRUE)
#groupcounts for each AOU for each year at scale of ONE stateroute 


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
      select(everything()) %>% data.frame()
    
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
      select(year, AOU) %>% #duplicates remnant of distinct secondary routes - finally ID'd bug
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


bbs_above = as.data.frame(output)
#Calc area for above route scale
#bbs_above$area = bbs_above_v2$numrtes*50*(pi*(0.4^2)) #number of routes * fifty stops * area in sq km of a stop 
write.csv(bbs_above, paste(BBS, "bbs_above.csv", sep = ""), row.names = FALSE)
#updated 09/20 evening locally and on BioArk; not sure if data folder will reject on git
write.csv(bbs_above, "data/BBS/bbs_above.csv", row.names = FALSE)
#updated 09/20




####Merging across scales####
bbs_above = read.csv(paste(BBS, "bbs_above.csv", sep = ""), header = TRUE)
bbs_below = read.csv(paste(BBS, "bbs_below_avgs.csv", sep = ""), header = TRUE)

#adding maxRadius column to bbs_below w/NA's + renaming and rearranging columns accordingly, creating area cols
bbs_below = bbs_below %>% 
  mutate(maxdist = c("NA")) %>%
  select(focalrte, scale, everything()) %>%
  mutate(area = bbs_below$scale*(pi*(0.4^2)), 
         scale = paste("seg", scale, sep = ""))

#modify and split scale so that it's just the # of stops in each seg; not the seg order # preceded by a "-"


bbs_above = bbs_above %>% 
  dplyr::mutate(area = scale*50*(pi*(0.4^2))) %>% #area in km by # of routes * 50 stops in each rte * area of a stop (for above-route scale later)
  dplyr::select(focalrte, scale, meanOcc, pctCore, pctTran, aveN, maxdist, area) #%>% 
# filter(scale == "2")

bbs_above$scale = as.factor(bbs_above$scale)


bbs_allscales = rbind(bbs_below, bbs_above) #rbind ok since all share column names




######################################################################################################################

####3/5 vs 2/5####

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

#tested, works 
test = bbs_below_guide %>% filter(scale == "25")
unique(test$seg)
#[1] 1 2; correct, 25 stop scale should only have two segments per route


#I can group by scale and segment and THEN take means of segments

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
        select(year, AOU) %>% #duplicates remnant of distinct secondary routes - finally ID'd bug
        distinct() %>% #removing duplicates 09/20
        count(AOU) %>% #how many times does that AOU show up in that clustr that year 
        mutate(occ = n/15, scale = nu) %>% #, subrouteID = countColumns[1]) #%>% countColumns not needed bc already pared down
        # group_by(r) %>% #don't want to group by stateroute though! want to calc for whole clustr
        summarize(focalrte = r, 
                  scale = nu, 
                  meanOcc = mean(occ), #FIX 09/19 across vector of AOU occupancies for AOU mean
                  pctCore = sum(occ > 3/5)/length(occ),
                  pctTran = sum(occ <= 2/5)/length(occ)) 
      
      
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


bbs_below = as.data.frame(output)
#write.csv(bbs_below, paste(BBS, "bbs_below_new.csv", sep = ""), row.names = FALSE)

#NOW I can average for unique scale-route combo (currently duplicates based on segments)

bbs_below_avgs = bbs_below %>% 
  group_by(focalrte, scale) %>% 
  summarize(meanOcc = mean(meanOcc), 
            pctCore = mean(pctCore), 
            pctTran = mean(pctTran), 
            aveN = mean(aveN))

# write.csv(bbs_below_avgs, paste(BBS, "bbs_below_avgs.csv", sep = ""), row.names = FALSE)
# write.csv(bbs_below_avgs, "data/BBS/bbs_below_avgs.csv", row.names = FALSE)
# successfully stored both avgs and new in bioark and data folder since small enough


####Above-scale and merging code####
####Calculating occupancy scales 2:66 loop####
dist.df = read.csv("scripts/R-scripts/scale_analysis/intermed/dist_df.csv", header = TRUE)
bbs_above_guide = read.csv("scripts/R-scripts/scale_analysis/intermed/bbs_above_guide.csv", header = TRUE)
#groupcounts for each AOU for each year at scale of ONE stateroute 


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
      select(everything()) %>% data.frame()
    
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
      select(year, AOU) %>% #duplicates remnant of distinct secondary routes - finally ID'd bug
      distinct() %>% #removing duplicates 09/20
      count(AOU) %>% #how many times does that AOU show up in that clustr that year 
      mutate(occ = n/15, scale = nu) %>% #, subrouteID = countColumns[1]) #%>% countColumns not needed bc already pared down
      # group_by(r) %>% #don't want to group by stateroute though! want to calc for whole clustr
      summarize(focalrte = r, 
                scale = nu, 
                meanOcc = mean(occ), #FIX 09/19 across vector of AOU occupancies for AOU mean
                pctCore = sum(occ > 3/5)/length(occ),
                pctTran = sum(occ <= 2/5)/length(occ), 
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


bbs_above = as.data.frame(output)
#Calc area for above route scale
#bbs_above$area = bbs_above_v2$numrtes*50*(pi*(0.4^2)) #number of routes * fifty stops * area in sq km of a stop 
write.csv(bbs_above, paste(BBS, "bbs_above.csv", sep = ""), row.names = FALSE)
#updated 09/20 evening locally and on BioArk; not sure if data folder will reject on git
write.csv(bbs_above, "data/BBS/bbs_above.csv", row.names = FALSE)
#updated 09/20




####Merging across scales####
bbs_above = read.csv(paste(BBS, "bbs_above.csv", sep = ""), header = TRUE)
bbs_below = read.csv(paste(BBS, "bbs_below_avgs.csv", sep = ""), header = TRUE)

#adding maxRadius column to bbs_below w/NA's + renaming and rearranging columns accordingly, creating area cols
bbs_below = bbs_below %>% 
  mutate(maxdist = c("NA")) %>%
  select(focalrte, scale, everything()) %>%
  mutate(area = bbs_below$scale*(pi*(0.4^2)), 
         scale = paste("seg", scale, sep = ""))

#modify and split scale so that it's just the # of stops in each seg; not the seg order # preceded by a "-"


bbs_above = bbs_above %>% 
  dplyr::mutate(area = scale*50*(pi*(0.4^2))) %>% #area in km by # of routes * 50 stops in each rte * area of a stop (for above-route scale later)
  dplyr::select(focalrte, scale, meanOcc, pctCore, pctTran, aveN, maxdist, area) #%>% 
# filter(scale == "2")

bbs_above$scale = as.factor(bbs_above$scale)


bbs_allscales = rbind(bbs_below, bbs_above) #rbind ok since all share column names






#####################################################################################################################


####Distributions (cutoffs not part of calculations, just temporal occupancy)####

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

BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/Intermediate scripts/BBS scaled/'

fifty_allyears = read.csv(paste(BBS, "fifty_allyears.csv", sep = ""), header = TRUE) #using updated version, 50 stop data, 07/12
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)

fifty_bestAous = fifty_allyears %>% 
  filter(AOU > 2880 & !(AOU >= 3650 & AOU <= 3810) & !(AOU >= 3900 & AOU <= 3910) & 
           !(AOU >= 4160 & AOU <= 4210) & AOU != 7010) #leaving out owls, waterbirds as less reliable data

#occ_counts function for calculating occupancy at any scale
#countcolumns can refer to the stops in a stateroute OR 
#it can refer to the associated secondary routes to aggregate across 
#occ_counts function for calculating occupancy at any scale
#countcolumns can refer to the stops in a stateroute OR 
#it can refer to the associated secondary routes to aggregate across 
occ_counts = function(countData, countColumns, scale) {
  bbssub = countData[, c("stateroute", "year", "AOU", countColumns)] #these are our grouping vars
  bbssub$groupCount = rowSums(bbssub[, countColumns]) 
  bbsu = unique(bbssub[bbssub[, "groupCount"]!= 0, c("stateroute", "year", "AOU")]) 
  
  abun.summ = bbssub %>% #abundance
    group_by(stateroute, year) %>%  
    summarize(totalN = sum(groupCount))  #we want to go further and summarize across focal + secondary rtes tho
  
  occ.summ = bbsu %>% #occupancy
    count(stateroute, AOU) %>%
    mutate(occ = n/15, scale = scale) %>% #, #may want to get rid of, this is at the column-counting scale
    #scale = scale) %>%
    left_join(abun.summ, by = 'stateroute')
  return(occ.summ)
}


# Generic calculation of occupancy for a specified scale
#fix to run all at once, so no sep run for above-scale, USE occ-counts for both 
b_scales = c(5, 10, 25, 50)
output = c()
for (s in b_scales) {
  numGroups = floor(50/s)
  for (g in 1:numGroups) {
    groupedCols = paste("Stop", ((g-1)*s + 1):(g*s), sep = "")
    temp = occ_counts(fifty_bestAous, groupedCols, s) 
    output = rbind(output, temp) 
  } 
}

min_dist = output
#transformation into matrix unnecessary with ggplot version 
#write.csv(min_dist, "//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/min_dist.csv", row.names = FALSE)
min_dist = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/min_dist.csv", header = TRUE)


#filter to scale == 50, check
min_dist2 = min_dist %>% 
  filter(scale == "50")

fig1a = ggplot(min_dist2, aes(occ))+
  geom_density(bw = "bcv", kernel = "gaussian", n = 2000, na.rm = TRUE)+
  labs(x = "Proportion of time present at site", y = "Probability Density", title = "Single Route Scale")+ 
  theme_classic() #coord_cartesian(xlim = c(0, 1), ylim = c(0, 2.5))+
fig1a

#repeat for scale of 5 stop segment and scale of 66 routes 

####Figure 1b: at scale of 5 stop segments####
# Generic calculation of occupancy for a specified scale
#fix to run all at once, so no sep run for above-scale, USE occ-counts for both 

#scale of 5 segments (min) 
min_dist = min_dist[, -3]
#need to avg occs between unique stateroute-AOU pairs since 5 for every 1 
min_dist3 = min_dist %>% 
  group_by(AOU, stateroute, scale) %>% 
  summarise(occ = mean(occ)) %>% dplyr::select(everything()) 

min_out = as.data.frame(min_dist3)
#write.csv(min_out, "//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/min_out.csv", row.names = FALSE)

fig1b = ggplot(min_dist3, aes(occ, group = scale, color = scale))+
  geom_density(kernel = "gaussian", n = 2000, na.rm = TRUE)+
  labs(x = "Proportion of time present at site", y = "Probability Density", title = "Local Scales")+ 
  theme_classic() #coord_cartesian(xlim = c(0, 1), ylim = c(0, 2.5))+
fig1b 

####Fig 1c: distribution at the maximum scale####
dist.df = read.csv("scripts/R-scripts/scale_analysis/dist_df.csv", header = TRUE)
bbs_above_guide = read.csv("scripts/R-scripts/scale_analysis/bbs_above_guide.csv", header = TRUE)
#groupcounts for each AOU for each year at scale of ONE stateroute 

#occ_counts function for calculating occupancy at any scale
#countcolumns can refer to the stops in a stateroute OR 
#it can refer to the associated secondary routes to aggregate across 

uniqrtes = unique(bbs_above_guide$stateroute) #all routes present are unique, still 953 which is great
scales = c(2, 4, 8, 16, 32, 66) # based on min common number in top 6 grid cells, see grid_sampling_justification script 
max_out = c()

for (nu in scales){
  #test example route 2010 and nu at 57 routes -> large scale, should have high occ 
  for (r in uniqrtes) { #for each focal route
    tmp_rte_group = dist.df %>% #changes with size of nu but caps at 66
      filter(rte1 == r) %>% 
      top_n(66, desc(dist)) %>% #fixed ordering by including arrange parm, 
      #remove/skip top row 
      arrange(dist) %>%
      slice(1:nu) %>% 
      dplyr::select(everything()) %>% data.frame()
    
    
    focal_clustr = bbs_above_guide %>% 
      filter(stateroute %in% tmp_rte_group$rte2) 
    
    occ.summ = focal_clustr %>% 
      dplyr::select(year, AOU) %>% #duplicates remnant of distinct secondary routes - finally ID'd bug
      distinct() %>% #removing duplicates 09/20
      count(AOU) %>% #how many times does that AOU show up in that clustr that year 
      dplyr::mutate(occ = n/15, stateroute = r, scale = nu) 
    
    max_out = rbind(max_out, occ.summ)
    
  }
}

max_out = max_out[, -2]
max_out = as.data.frame(max_out)

fig1c = ggplot(max_out, aes(occ))+
  geom_density(bw = "bcv", kernel = "gaussian", n = 2000, na.rm = TRUE)+
  labs(x = "Proportion of time present at site", y = "Probability Density", title = "Maximum Scale")+theme_classic()
#so it was the limits giving me crap in the original 
fig1c

write.csv(max_out, "//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/max_out.csv", row.names = FALSE)

####Figure 4 all graphs overlay####
## merge output, min, and max into single df while adding new column delineating which 
## category: single, min, or max the data corresponds to so multiple lines can be 
## overlaid on single density plot 

#read in min and single route scale occ density data 
min_out = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/min_out.csv", header = TRUE)
#scales 2:66 agg routes 
max_out = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/max_out.csv", header = TRUE)
#updated 12/12 


#organize by scales; label and differentiate scales so that below-rtes are appropriately smaller
#do area calcs and color by area? 

min_out = min_out %>% 
  dplyr::select(stateroute, AOU, occ, scale) %>% 
  dplyr::mutate(area = scale*(pi*(0.4^2))) %>% #scale corresponds to the number of stops
  dplyr::select(stateroute, AOU, occ, area)


max_out = max_out %>% 
  dplyr::select(stateroute, AOU, occ, scale) %>% 
  dplyr::mutate(area = scale*50*(pi*(0.4^2))) %>% #scale corresponds to the number of agg routes; 50 stops per rte
  dplyr::select(stateroute, AOU, occ, area)



all_fig = rbind(max_out, min_out)
write.csv(all_fig, "//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/all_figoutput.csv", row.names = FALSE)
#stored in bioark folder 


####Plotting how distributions change across scale, using area####
all_fig = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/Intermediate scripts/BBS scaled/all_figoutput.csv", header = TRUE)
#all_fig$area = as.factor(all_fig$area)

all_figplot = ggplot(all_fig, aes(occ, group = factor(signif(area, digits = 2)), color = factor(signif(area, digits = 2))))+
  stat_density(geom = "path", position = "identity", bw = "bcv", kernel = "gaussian", n = 4000, na.rm = TRUE, size = 1.3)+
  labs(x = "Proportion of time present at site", y = "Probability Density")+theme_classic()+
  scale_color_viridis(discrete = TRUE, name = expression("Spatial Scale in km"^{2}))+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 16))+
  theme(legend.text = element_text(size = 16), legend.title = element_text(size = 16))+
  theme(legend.position = c(0.50, 0.50))
all_figplot
