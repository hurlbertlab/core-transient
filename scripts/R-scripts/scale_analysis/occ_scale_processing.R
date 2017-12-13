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
    summarize(meanOcc = mean(occ), 
    mpctCore = sum(occ > 2/3)/length(occ),
    mpctTran = sum(occ <= 1/3)/length(occ)) %>%
    left_join(abun.summ, by = 'stateroute')
  
  
  
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
  
  
  
  
  
  
  return(occ.summ)
}


# Generic calculation of occupancy for a specified scale
#fix to run all at once, so no sep run for above-scale, USE occ-counts for both 
b_scales = c(5, 10, 25, 50)
output2 = c()
output = c()
for (s in b_scales) {
  numGroups = floor(50/s)
  for (g in 1:numGroups) {
    groupedCols = paste("Stop", ((g-1)*s + 1):(g*s), sep = "")
    temp = occ_counts(fifty_bestAous, groupedCols, s) 
    output = rbind(output, temp) 
  }
  
  occ.summ = output %>% 
    group_by(stateroute) %>%
    summarize(aveN = mean(totalN), 
              meanOcc = mean(occ), 
              mpctCore = sum(occ > 2/3)/length(occ),
              mpctTran = sum(occ <= 1/3)/length(occ),
              scale = as.factor(s)) 
  
  output2 = rbind(output2, occ.summ)
}


single_rte = output2 %>% 
  dplyr::filter(scale == "50")

ggplot(single_rte)+geom_point(aes(x = pctCore, y = pctTran))
ggplot(single_rte)+geom_point(aes(x = aveN, y = meanOcc))
#I REALLY don't know why the mean Occ is so low, or why it is so gap-y compared to 2 routes 
#I'm gonna check this out further down in bbs_allscales 


####Testing the above function for matching w/distribution plots####
#set g = 1 
#set s = 50 
output = c()
groupedCols = paste("Stop", ((g-1)*s + 1):(g*s), sep = "")
temp = occ_counts(fifty_bestAous, groupedCols, s) 
output = rbind(output, temp)

occ.summ = output %>% 
  group_by(stateroute) %>%
  summarize(aveN = mean(totalN), 
            meanOcc = mean(occ), 
            pctCore = sum(occ > 2/3)/length(occ),
            pctTran = sum(occ <= 1/3)/length(occ),
            scale = as.factor(s))


View(occ.summ)
ggplot(output, aes(occ)) + geom_density(bw = "bcv", kernel = "gaussian", n = 2000, na.rm = TRUE) + 
  labs(x = "Proportion of time present at site", y = "Probability Density", title = "Single Route Scale")+
  theme_classic() 

#for whole series: 
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

#from figures script: 
min_dist = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/min_dist.csv", header = TRUE)

#filter to scale == 50, check
min_dist2 = min_dist %>% 
  filter(scale == "50")

fig1a = ggplot(min_dist2, aes(occ))+
  geom_density(bw = "bcv", kernel = "gaussian", n = 2000, na.rm = TRUE)+
  labs(x = "Proportion of time present at site", y = "Probability Density", title = "Single Route Scale")+ 
  theme_classic() #coord_cartesian(xlim = c(0, 1), ylim = c(0, 2.5))+
fig1a

min_dist3 = min_dist %>% 
  group_by(AOU, stateroute, scale) %>% 
  summarise(occ = mean(occ)) %>% dplyr::select(everything()) 

ggplot(min_dist3, aes(occ, group = scale, color = scale))+
  geom_density(kernel = "gaussian", n = 2000, na.rm = TRUE)+
  labs(x = "Proportion of time present at site", y = "Probability Density", title = "Local Scales")+ 
  theme_classic() #coord_cartesian(xlim = c(0, 1), ylim = c(0, 2.5))+
###THEY LOOK EXACTLY THE FREAKING SAME WHAT IS WRONGGGGGGG###

#output version 
output3 = output %>% 
  group_by(AOU, stateroute, scale) %>% 
  summarise(occ = mean(occ)) %>% dplyr::select(everything()) 

ggplot(output3, aes(occ, group = scale, color = scale))+
  geom_density(kernel = "gaussian", n = 2000, na.rm = TRUE)+
  labs(x = "Proportion of time present at site", y = "Probability Density", title = "Local Scales")+ 
  theme_classic()

#figures should be the same. they are. so what the hell is wrong with the data? 

####

plot(meanOcc~aveN, data = output2) #looks good, low avg bc small sample, high % Tran relative to Core   

bbs_below_new<-data.frame(output2)
write.csv(bbs_below_new, paste(BBS, "bbs_below_new.csv", sep = ""), row.names = FALSE) #updated 12/12, on BioArk
#should be able to use the 50 stop info (1 rte) from this output to aggregate routes AFTER below scale
write.csv(bbs_below, "data/BBS/bbs_below.csv", row.names = FALSE)

#at scale of a single route (e.g. "50-1", no communities)

####Comparison of old vs new below rte data - is there a difference?####

bbs_below = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/bbs_below.csv", header = TRUE) 
bbs_below_new = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/bbs_below_new.csv", header = TRUE) 

plot(meanOcc~log(aveN), data = bbs_below_new, xlab = "Average Abundance" , ylab = "Mean Temporal Occupancy")
plot(meanOcc~log(aveN), data = bbs_below, xlab = "Average Abundance" , ylab = "Mean Temporal Occupancy")

comp = gridExtra::grid.arrange(g1, g2)
####Data prep for calculating occupancy above the scale of a BBS route####
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

#occ_counts2
#important to not remove AOU and stateroute data by year, but to halt at that step 
#so can be guided thru original occ_counts, with secondary routes as "countColumns" 
#have to gen new function

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
c_scales = c(50)
output = c()
for (scale in c_scales) {
  numGroups = floor(50/scale)
  for (g in 1:numGroups) {
    groupedCols = paste("Stop", ((g-1)*scale + 1):(g*scale), sep = "")
    temp = occ_counts2(fifty_bestAous, groupedCols, scale)
    output = rbind(output, temp) 
  }
}

bbs_above_guide = data.frame(output)
write.csv(bbs_above_guide, "scripts/R-scripts/scale_analysis/bbs_above_guide.csv", row.names = FALSE)

####Calculating occupancy scales 2:66 loop####
dist.df = read.csv("scripts/R-scripts/scale_analysis/dist_df.csv", header = TRUE)
bbs_above_guide = read.csv("scripts/R-scripts/scale_analysis/bbs_above_guide.csv", header = TRUE)
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

####scale-joining####
bbs_above = read.csv(paste(BBS, "bbs_above.csv", sep = ""), header = TRUE)
bbs_below = read.csv(paste(BBS, "bbs_below_new.csv", sep = ""), header = TRUE)

#adding maxRadius column to bbs_below w/NA's + renaming and rearranging columns accordingly, creating area cols
bbs_below = bbs_below %>% 
  mutate(maxdist = c("NA")) %>%
  dplyr::rename(focalrte = stateroute) %>%
  select(focalrte, scale, everything()) %>%
  mutate(area = (as.integer(lapply(strsplit(as.character(bbs_below$scale), 
                                            split="-"), "[", 1)))*(pi*(0.4^2)),
         scale = as.factor(paste("seg", as.integer(lapply(strsplit(as.character(bbs_below$scale), 
                                                    split="-"), "[", 1)), sep = "")))
#modify and split scale so that it's just the # of stops in each seg; not the seg order # preceded by a "-"


bbs_above = bbs_above %>% 
  dplyr::mutate(area = scale*50*(pi*(0.4^2))) %>% #area in km by # of routes * 50 stops in each rte * area of a stop (for above-route scale later)
  dplyr::select(focalrte, scale, meanOcc, pctCore, pctTran, aveN, maxdist, area)

bbs_above$scale = as.factor(bbs_above$scale)


bbs_allscales = rbind(bbs_below, bbs_above) #rbind ok since all share column names
write.csv(bbs_allscales, "data/BBS/bbs_allscales_new.csv", row.names = FALSE)
#updated 12/12/2017, also in BioArk since old copy ALSO there
write.csv(bbs_allscales, paste(BBS, "bbs_allscales_new.csv", sep = ""), row.names = FALSE)

####filter out stateroutes that are one-sided in scale####
#in terms of their representation of below vs above scale (should have both, not one alone)

bbs_allscales = read.csv(paste(BBS, "bbs_allscales_new.csv", sep = ""), header = TRUE)
bbs_allscales$logA = log10(bbs_allscales$area)
bbs_allscales$logN = log10(bbs_allscales$aveN)
bbs_allscales$lnA = log(bbs_allscales$area) #log is the natural log 
bbs_allscales$lnN = log(bbs_allscales$aveN) #rerun plots with this?

####Closer look at scales 1 vs 2 where jump occurs####
bbs_prob = bbs_allscales %>% 
  filter(scale == "seg50" | scale == "2")

ggplot(bbs_prob, aes(x = aveN, y = meanOcc, color = scale))+geom_point()
ggplot(bbs_prob, aes(x = logN, y = meanOcc, color = scale))+geom_point()
ggplot(bbs_prob, aes(x = area, y = meanOcc, color = scale))+geom_point()

#only want rtes w/all 69 scales rep'd, which at this point - there are! 
bbs_allscales2 = bbs_allscales %>% filter(meanOcc != 'NaN' & meanOcc != 'NA') %>% 
  count(focalrte) %>% filter(n == 69) %>% data.frame() #fix error to exclude NAs
bbs_allscales3 = filter(bbs_allscales, focalrte %in% bbs_allscales2$focalrte)

write.csv(bbs_allscales3, "data/BBS/bbs_allscales.csv", row.names = FALSE) 
#overwrote bbs all scales file 
#updated 09/20


####Occ-scale analysis####
####Cross-scale analysis and visualization####
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
levels(bbs_allscales$scale)
unique(bbs_allscales$scale)


mod1 = lm(meanOcc~logA, data = bbs_allscales) #expljkains ~75-80% of the variation in occ
mod2 = lm(meanOcc~logN, data = bbs_allscales)
summary(mod1)

plot(meanOcc~logA, data = bbs_allscales, xlab = "Log Area" , ylab = "Mean Temporal Occupancy")
plot(meanOcc~logN, data = bbs_allscales, xlab = "Average Abundance" , ylab = "Mean Temporal Occupancy")
#^^same pattern roughly; abundance describes ~same amt of variance as area so serves as a good proxy 

plot(meanOcc~scale, data = bbs_allscales)

#ALL files updated 09/20 ~3pm 
#bbs_allscales$preds = predict(mod1)
pred_plot = ggplot(bbs_allscales, aes(x = logA, y = meanOcc))+geom_line(aes(group = focalrte), color = "grey")+
  theme_classic() + #+geom_line(aes(y = preds), color = "red")+ #geom_smooth(model = lm, color = 'red')+
  labs(x = "Log Area", y = "Mean Community Occupancy") #+ 
  # annotate("text", x = 2.5, y = 0.45, colour = "red", label = "italic(R) ^ 2 == 0.7424", parse = TRUE)+
  # scale_color_manual(values=c("Observed"="grey", "Mean Predicted"="red"))
pred_plot

#bbs_allscales$preds2 = predict(mod2)
pred_plot2 = ggplot(bbs_allscales, aes(x = logN, y = meanOcc))+geom_line(aes(group = focalrte), color = "grey")+
  theme_classic() + #+geom_line(aes(y = preds2), color = "red")+ #geom_smooth(model = lm, color = 'red')+
  labs(x = "Log Abundance", y = "Mean Community Occupancy") #+ 
  # annotate("text", x = 4, y = 0.45, colour = "red", label = "italic(R) ^ 2 == 0.8087", parse = TRUE)+
  # scale_color_manual(values=c("Observed"="grey", "Mean Predicted"="red"))
pred_plot2

abun_p = ggplot(bbs_allscales, aes(x = logA, y = meanOcc, colour = logN))+
  geom_line(aes(group = focalrte))+
  theme_classic()+ #geom_smooth(model = lm, color = 'red')+
  labs(x = "Log Area", y = "Mean Community Occupancy")
abun_p
?geom_line 
