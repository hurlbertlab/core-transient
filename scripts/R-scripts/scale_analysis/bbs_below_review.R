####BBS below troubleshooting####

setwd("C:/git/core-transient")
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
    output = rbind(output, temp) 
  }
}

bbs_below_guide = data.frame(output)
write.csv(bbs_below_guide, paste(BBS, "bbs_below_guide.csv", sep = ""), row.names = FALSE)

#bbs above guide derived from subset where scale == 50 



#need to make sure NOT running thru 66 times on the same site and scale 
uniqrtes = unique(bbs_below_guide$stateroute) #all routes present are unique, still 953 which is great
scale = unique(bbs_below_guide$scale)
output = data.frame(focalrte = NULL,
                    scale = NULL, 
                    meanOcc = NULL, 
                    pctCore = NULL,
                    pctTran = NULL,
                    aveN = NULL)

#test example route 2010 and nu at 57 routes -> large scale, should have high occ 
for (r in uniqrtes) { #for each focal route
  for (nu in scale) { #for each level of scale aggregated to each focal route
    
    
    
    #find parallel from occ_counts2 for appropriate scales, filter by correct scales 
    
    # tmp_rte_group = dist.df %>% #changes with size of nu but caps at 66
    #   filter(rte1 == r) %>% 
    #   top_n(66, desc(dist)) %>% #fixed ordering by including arrange parm, 
    #   #remove/skip top row 
    #   arrange(dist) %>%
    #   slice(1:nu) %>% 
    #   select(everything()) %>% data.frame()
    
    focal_clustr = bbs_below_guide %>% 
      filter(stateroute == r & scale == nu) #tmp_rte_group already ordered by distance so don't need 2x
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
    
  } #n loop
  
} #r loop
# I can then feed the above into my occ_counts function 
# may need to transpose rows to columns 


bbs_below = as.data.frame(output)
write.csv(bbs_below, paste(BBS, "bbs_below_new.csv", sep = ""), row.names = FALSE)

####Recomparing problem scale differences####
bbs_above = read.csv(paste(BBS, "bbs_above.csv", sep = ""), header = TRUE)
bbs_below = read.csv(paste(BBS, "bbs_below_new.csv", sep = ""), header = TRUE)

#adding maxRadius column to bbs_below w/NA's + renaming and rearranging columns accordingly, creating area cols
bbs_below = bbs_below %>% 
  mutate(maxdist = c("NA")) %>%
  select(focalrte, scale, everything()) %>%
  mutate(area = bbs_below$scale*(pi*(0.4^2)))
         
#modify and split scale so that it's just the # of stops in each seg; not the seg order # preceded by a "-"


bbs_above = bbs_above %>% 
  dplyr::mutate(area = scale*50*(pi*(0.4^2))) %>% #area in km by # of routes * 50 stops in each rte * area of a stop (for above-route scale later)
  dplyr::select(focalrte, scale, meanOcc, pctCore, pctTran, aveN, maxdist, area)%>% 
  filter(scale == "2")

bbs_above$scale = as.factor(bbs_above$scale)


bbs_allscales = rbind(bbs_below, bbs_above) #rbind ok since all share column names

####filter out stateroutes that are one-sided in scale####
#in terms of their representation of below vs above scale (should have both, not one alone)
bbs_allscales$logA = log10(bbs_allscales$area)
bbs_allscales$logN = log10(bbs_allscales$aveN)
bbs_allscales$lnA = log(bbs_allscales$area) #log is the natural log 
bbs_allscales$lnN = log(bbs_allscales$aveN) #rerun plots with this?

bbs_prob = bbs_allscales

####Closer look at scales 1 vs 2 where jump occurs####


ggplot(bbs_below, aes(x = log(area), y = meanOcc))+geom_line(aes(group = focalrte), color = "grey")+
  theme_classic() + #+geom_line(aes(y = preds), color = "red")+ #geom_smooth(model = lm, color = 'red')+
  labs(x = "Log Area", y = "Mean Community Occupancy")
