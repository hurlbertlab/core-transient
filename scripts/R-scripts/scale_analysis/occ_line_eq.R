#Occupancy-scale analysis

# author: Molly F. Jenkins
# date: 06/27/2017

####Summary####
#This script takes a dataset of BBS routes with occupancy values calculated for every scale, 
#scales ranging in size from 1/10th of a BBS route to 66 aggregated BBS routes. 
#Using this data we analyze the relationship between temporal occupancy of communities and scale. 
#We characterize this relationship in a series of simple linear models between occupancy and scale 
#(using log(area) as our variable of spatial scale). From these models and our existing data, 
#for every focal route we traced how occupancy changes across scale, from smallest to largest scale. 
#We characterized these changes through a series of variables: 
  #1)the occupancy value at the minimum scale for a focal route 
  #2)the occupancy value at the maximum scale for a focal route 
  #3)the slope of the line linking the minimum and maximum values 
      #the steepness (or flatness) of this line corresponds with the rate of accumulation of core species 
      #for a given community 
  #4)the midpoint occupancy value of the line at the scale of 3 aggregated routes per focal route 
      #the value of mean occupancy for the community at this scale is also a useful proxy for 
      #the rate of accumulation of core species for a given community 
  #5)the scale at which mean occupancy first reaches or surpasses 50% 
      #At what scale, at what area in km must we reach in order to reliably see consistent occupancy 
      #PREDICTION: in areas of fairly uniform habitat type, this scale should be lower 
      #PREDICTION: in areas of fairly high habitat heterogeneity, this scale should be higher 
  #6)finally, we look at the variation in occupancy explained by scale thru R^2 values 
  #7)and we look at the straightness or curviness of the actual data 
      #as compared to the data derived from our model.
      #PREDICTION: focal routes with larger "curvy" values will occur in regions of greater habitat heterogeneity, 
      #and deviance from the line will correspond with the greater environmental variance
      #associated with the location of that focal route.
      #i.e. curvy is a proxy for AUC, where greater deviance of the predicted vals vs the actual vals will result 
      #in larger overall curvy values and larger area under the curve

#We explore the variation in this relationship 
#and attempt to characterize whether or not it is best explained by habitat heterogeneity 
#using the variation present across several environmental variables as proxies for habitat heterogeneity. 

#### CURRENT ISSUES #### #revised 09/10/2017

#1) Have below-scale duplicates w/diff starting locations i.e. 
  #"scale" = Factor w/83 levels "10-1", "10-2", "10-3", "10-4", "10-5", "5-1", "5-2", "5-3", "5-4", "5-5", "5-6", "5-7", "5-8", etc. 
  #no duplicates for 1:66 obvi, these were calculated correctly 
  #can I have multi values for a single scale or should I calc avg occ and pctCore and pctTran across the segments w/in a route? 
  #i.e. occ/15 at a scale, but scales designated by segments and occ calcd initially based on the starting stop # 
  # was told this summer that that was alright and that we were pointedly NOT aggregating across the lower scales, and that's fine 
  #BUT: it DOES mean we will have pseudo-duplicates at the lower scales w/diff starting points of segments, and so 
  #multiple plotting points for the lower scales instead of a single representative point. This necessitates the "min" qualifier
  #in calculating some of the coefficients.  

#2) In calculating the AUC proxy "curvy" values, should I be squaring the differences before summing them? 
  #Because we are interested in the magnitude of deviance from the observed values in instances of greater heterogeneity, 
  #and not the specific direction - simply adding them could cancel out or diminish the magnitude or gulf of deviance 
  #in areas where the deviance is large in a negative direction at some scales, and large in a positive direction in others. 
  #These stateroutes would express similar "curviness" values as areas of high homogeneity, where this is little net difference 
  #and a smaller magnitude of deviance from the observed values. Because of this, I think we should be squaring and summing. 

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
BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/'

bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
###Pre-coefs distribution analysis###
minscales = bbs_allscales %>% 
  filter(scale == '50-1')%>% 
  summarize(min = min(meanOcc)) #0.34


topscales = bbs_allscales %>% 
  filter(scale == '50-1')

ggplot(topscales, aes(x = pctCore))+geom_histogram(bins = 20)
ggplot(topscales, aes(x = meanOcc))+geom_histogram(bins = 20)
ggplot(topscales, aes(x = pctTran))+geom_histogram(bins = 20)
#a non-trivial group of focal rtes still maintain very low % Core representation in their communities, even at our highest scales 
#I would predict that these focal rtes are largely clustered in Western and mountain regions 
#let's see: 

topscales2 = topscales %>% 
  filter(pctTran >= 0.50) %>%
  mutate(Dom = 'T') 

topscales3 = topscales %>% 
  filter(pctCore >= 0.50) %>% 
  mutate(Dom = "C")

topscales_new = rbind(topscales2, topscales3)
topscales_new$Dom = as.factor(topscales_new$Dom)
topscales_new = select(topscales_new, focalrte, meanOcc, pctCore, pctTran, logA, Dom)


#all focal rtes with all possible pairings
bbs_latlon = read.csv(paste(BBS, "good_rtes2.csv", sep = ""), header = TRUE)
bbs_latlon = bbs_latlon %>% inner_join(topscales_new, by = c("stateroute" = "focalrte"))

##the rough way: 
map('world', xlim = range(bbs_latlon$Longi), ylim = range(bbs_latlon$Lati))
points(bbs_latlon$Longi[bbs_latlon$Dom == "C"], bbs_latlon$Lati[bbs_latlon$Dom == "C"], pch = 16, col = "blue")
points(bbs_latlon$Longi[bbs_latlon$Dom == "T"], bbs_latlon$Lati[bbs_latlon$Dom == "T"], pch = 16, col = "red")

#basically how I would assume these communities would be distributed 
legend(x="bottomleft", legend = unique(bbs_latlon$Dom), fill = unique(bbs_latlon$Dom))
title("Distribution of communities at scale of 1 routes")


#at scale of 66 routes
topscales = bbs_allscales %>% 
  filter(scale == '66')

ggplot(topscales, aes(x = pctCore))+geom_histogram(bins = 30)
ggplot(topscales, aes(x = meanOcc))+geom_histogram(bins = 30)
ggplot(topscales, aes(x = pctTran))+geom_histogram(bins = 30)
#a non-trivial group of focal rtes still maintain very low % Core representation in their communities, even at our highest scales 
#I would predict that these focal rtes are largely clustered in Western and mountain regions 
#let's see: 

topscales2 = topscales %>% 
  filter(pctTran >= 0.50) %>%
  mutate(Dom = 'T') 

topscales3 = topscales %>% 
  filter(pctCore >= 0.50) %>% 
  mutate(Dom = "C")

topscales_new = rbind(topscales2, topscales3)
topscales_new$Dom = as.factor(topscales_new$Dom)
topscales_new = select(topscales_new, focalrte, meanOcc, pctCore, pctTran, logA, Dom)


#all focal rtes with all possible pairings
bbs_latlon = read.csv(paste(BBS, "good_rtes2.csv", sep = ""), header = TRUE)
bbs_latlon = bbs_latlon %>% inner_join(topscales_new, by = c("stateroute" = "focalrte"))

##the rough way: 
map('world', xlim = range(bbs_latlon$Longi), ylim = range(bbs_latlon$Lati))
points(bbs_latlon$Longi[bbs_latlon$Dom == "C"], bbs_latlon$Lati[bbs_latlon$Dom == "C"], pch = 16, col = "blue")
points(bbs_latlon$Longi[bbs_latlon$Dom == "T"], bbs_latlon$Lati[bbs_latlon$Dom == "T"], pch = 16, col = "red")

#basically how I would assume these communities would be distributed 
legend(x="bottomleft", legend = unique(bbs_latlon$Dom), fill = unique(bbs_latlon$Dom))
title("Distribution of communities at scale of 66 routes")


####Extract coefficients from scale-occupancy relationships for analysis####
OA.df = data.frame(stateroute = numeric(), OA.min = numeric(), OA.max = numeric(), OA.slope = numeric(), 
                   OA.thresh = numeric(), 
                   OA.pmin = numeric(), OA.pmax = numeric(), OA.pslope = numeric(),  
                   OA.pthresh = numeric(), 
                   OA.r2 = numeric(), OA.curvy = numeric()) 

CA.df = data.frame(stateroute = numeric(), CA.min = numeric(), CA.max = numeric(), CA.slope = numeric(), 
                   CA.thresh = numeric(), 
                   CA.pmin = numeric(), CA.pmax = numeric(), CA.pslope = numeric(),
                   CA.pthresh = numeric(), 
                   CA.r2 = numeric(), CA.curvy = numeric())

TA.df = data.frame(stateroute = numeric(), TA.min = numeric(), TA.max = numeric(), TA.slope = numeric(), 
                   TA.thresh = numeric(), 
                   TA.pmin = numeric(), TA.pmax = numeric(), TA.pslope = numeric(), 
                   TA.pthresh = numeric(), 
                   TA.r2 = numeric(), TA.curvy = numeric())

#read in data for processing
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
levels(bbs_allscales$scale)
unique(bbs_allscales$scale)
#ALL clear 07/24



####coefs####
stateroutes = unique(bbs_allscales$focalrte)

#do I even need a loop? can't I just group by stateroute and calc these ?


for(s in stateroutes){
  logsub = subset(bbs_allscales, bbs_allscales$focalrte == s)  
  #OA 
    OAlog = lm(meanOcc ~ logA, data = logsub) #lm instead of nls, reg linear model
    logsub$OApreds = predict(OAlog)
    #OApred_df = data.frame(preds = predict(OAlog), scale = logsub$scale, logA = logsub$logA)  #get preds -> is predicting unique per scale, all clear
    OAlm.r2 = lm(meanOcc ~ OApreds, data = logsub) #get r2 from model, so far this is just predmod tho 
    
    
    #ACTUAL stats (for plotting data pts): 
    OA.min = min(logsub$meanOcc[logsub$logA == min(logsub$logA)])
    OA.max = logsub$meanOcc[logsub$logA == max(logsub$logA)]
    OA.slope = ((OA.max - OA.min)/(max(logsub$logA[logsub$meanOcc == max(logsub$meanOcc)]) - min(logsub$logA[logsub$meanOcc == min(logsub$meanOcc)])))
    OA.xmid = logsub$meanOcc #vector for a given focal rte s, actual value
    OA.thresh = min(logsub$logA[logsub$meanOcc >= 0.5]) 
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #save as an area, not a "scale" 
    
    #PREDICTED stats (for fitting line): 
    OA.pmin =  min(logsub$OApreds[logsub$logA == min(logsub$logA)])
    OA.pmax = logsub$OApreds[logsub$logA == max(logsub$logA)]
    OA.pslope = ((OA.pmax - OA.pmin)/(max(logsub$logA[logsub$OApreds == max(logsub$OApreds)]) - min(logsub$logA[logsub$OApreds == min(logsub$OApreds)])))
    OA.pxmid = logsub$OApreds
    OA.pthresh = min(logsub$logA[logsub$OApreds >= 0.5]) 
    
    OA.r2 = summary(OAlm.r2)$r.squared
    OA.curvy =  sum(OA.xmid - OA.pxmid) #AUC proxy - taking diff between actual and predicted mid vals at EVERY scale and adding together
   

    OAmodel = data.frame(stateroute = s, OA.min, OA.max, OA.slope, 
                                          OA.thresh, 
                                          OA.pmin, OA.pmax, OA.pslope,
                                          OA.pthresh, 
                                          OA.r2, OA.curvy)
    
  OA.df = rbind(OA.df, OAmodel)
  #
  
   #CA
    CAlog = lm(pctCore ~ logA, data = logsub) #lm instead of nls, reg linear model
    logsub$CApreds = predict(CAlog) #just what's literally on the line but not actual preds 
    #CApred_df = data.frame(preds = predict(CAlog), scale = logsub$scale, logA = logsub$logA)  #get preds -> is predicting unique per scale, all clear
    CAlm.r2 = lm(pctCore ~ CApreds, data = logsub) #get r2 from model, so far this is just predmod tho 
    
    
    #ACTUAL stats (for plotting data pts): 
    CA.min = min(logsub$pctCore[logsub$logA == min(logsub$logA)])
    CA.max = logsub$pctCore[logsub$logA == max(logsub$logA)]
    CA.slope = ((CA.max - CA.min)/(max(logsub$logA[logsub$pctCore == max(logsub$pctCore)]) - min(logsub$logA[logsub$pctCore == min(logsub$pctCore)])))
    CA.xmid = logsub$pctCore #@ scale == 3, for a given focal rte s, actual value
    CA.thresh = min(logsub$logA[logsub$pctCore >= 0.5]) 
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #other threshold limits/metrics relevant to %core and %transient? 
    
    #PREDICTED stats (for fitting line): 
    CA.pmin =  min(logsub$CApreds[logsub$logA == min(logsub$logA)])
    CA.pmax = logsub$CApreds[logsub$logA == max(logsub$logA)]
    CA.pslope = ((CA.pmax - CA.pmin)/(max(logsub$logA[logsub$CApreds == max(logsub$CApreds)]) - min(logsub$logA[logsub$CApreds == min(logsub$CApreds)])))
    CA.pxmid = logsub$CApreds
    CA.pthresh = min(logsub$logA[logsub$CApreds >= 0.5]) 
    
    CA.r2 = summary(CAlm.r2)$r.squared
    CA.curvy =  sum(CA.xmid - CA.pxmid) #AUC proxy - taking diff between actual and predicted mid vals at EVERY scale and adding together
     
    
    CAmodel = data.frame(stateroute = s, CA.min, CA.max, CA.slope, 
               CA.thresh, 
               CA.pmin, CA.pmax, CA.pslope, 
               CA.pthresh, 
               CA.r2, CA.curvy)
    
  CA.df = rbind(CA.df, CAmodel)
  
 
  # Fitting % transient
  #TA
  TAlog = lm(pctTran ~ lnA, data = logsub) #lm instead of nls, reg linear model
    logsub$TApreds = predict(TAlog)
    #TApred_df = data.frame(preds = predict(TAlog), scale = logsub$scale, lnA = logsub$lnA)  #get preds -> is predicting unique per scale, all clear
    TAlm.r2 = lm(pctTran ~ TApreds, data = logsub) #get r2 from model, so far this is just predmod tho

    #ACTUAL stats (for plotting data pts):
    TA.min = min(logsub$pctTran[logsub$lnA == min(logsub$lnA)])
    TA.max = logsub$pctTran[logsub$lnA == max(logsub$lnA)]
    TA.slope = ((TA.max - TA.min)/(max(logsub$lnA[logsub$pctTran == max(logsub$pctTran)]) - min(logsub$lnA[logsub$pctTran == min(logsub$pctTran)])))
    TA.xmid = logsub$pctTran #@ scale == 3, for a given focal rte s, actual value
    TA.thresh = min(logsub$lnA[logsub$pctTran >= 0.50])
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #then save as a character so associated levels data doesn't stay stuck on the single data point

    #PREDICTED stats (for fitting line):
    TA.pmin =  min(logsub$TApreds[logsub$lnA == min(logsub$lnA)])
    TA.pmax = logsub$TApreds[logsub$lnA == max(logsub$lnA)]
    TA.pslope = ((TA.pmax - TA.pmin)/(max(logsub$lnA[logsub$TApreds == max(logsub$TApreds)]) - min(logsub$lnA[logsub$TApreds == min(logsub$TApreds)])))
    TA.pxmid = logsub$TApreds
    TA.pthresh = min(logsub$lnA[logsub$TApreds >= 0.50])

    TA.r2 = summary(TAlm.r2)$r.squared
    TA.curvy =  sum(TA.xmid - TA.pxmid) #AUC proxy - taking diff between actual and predicted mid vals at EVERY scale and adding together
  
    TAmodel = data.frame(stateroute = s, TA.min, TA.max, TA.slope,  
               TA.thresh,
               TA.pmin, TA.pmax, TA.pslope,
               TA.pthresh,
               TA.r2, TA.curvy)


  TA.df = rbind(TA.df, TAmodel)

  
} #end of loop 
  
  
#join all together using inner_join by focal rte, not cbind 
coefs = OA.df %>% 
  inner_join(CA.df, OA.df, by = "stateroute") %>% 
  inner_join(TA.df, OA.df, by = "stateroute") 
 
write.csv(coefs, "scripts/R-scripts/scale_analysis/coefs.csv", row.names = FALSE) #updated 07/27
#exp mods have much better r2 vals for pctTran than power 
#checked, working correctly 08/28, output not NA's but normal!

####Plotting occupancy-scale relationships with observed and predicted values####
#work in progress 08/28
#do I want to plot slope and line of predicted values over the top of actual? should be an easy sub 

bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
coefs = read.csv("scripts/R-scripts/scale_analysis/coefs.csv", header = TRUE)
goodrtes = read.csv(paste(BBS, "good_rtes2.csv", sep = ""), header = TRUE)
  
  
#merge longi and lati to bbs_allscales prior to east vs west plotting 
bbs_allscales = bbs_allscales %>% 
  left_join(goodrtes, by = c("focalrte" = "stateroute"))
  

#plot observed occ scale thresh or mid vals vs pthresh or pmid vals across scales 
#subtract actual - pred to gen new column of AUC vals for each scale  

#first plot freqs of occupancy vals with vals binned every .05 and .1 incremements for diff scales: 

#First plots: scale = 1, x axis = meanOcc, y axis = freq of meanOcc vals (hist) 
bbs_one = bbs_allscales %>% 
  filter(scale == "50-1")

one_rtfreq = ggplot(bbs_one, aes(x=meanOcc))+geom_histogram(bins = 30) + coord_cartesian(xlim = c(0, 0.95))
one_rtfreq
#compare btw/East vs West N. American routes, split in half and compare to predicted whiteboard sketches 
#since statecode/focalrte names aren't assigned in order from East to West, filter by greater than or less than longitudinal cutoff 
#add route coordinate data first and then filter based on that 



bbs_one_E = bbs_one %>% 
  filter(Longi > -100) #greater than -100 is -99, -98, -97 etc. 
bbs_one_W = bbs_one %>% 
  filter(Longi <= -100 ) #less than or equal to -100 is further away from 0 and further negative


#plotting comparisons:
one_rtfreqE = ggplot(bbs_one_E, aes(x=meanOcc))+geom_histogram(bins = 30) + coord_cartesian(xlim = c(0, 0.95))
one_rtfreqE

one_rtfreqW = ggplot(bbs_one_W, aes(x=meanOcc))+geom_histogram(bins = 30) + coord_cartesian(xlim = c(0, 0.95))
one_rtfreqW


#Secondary comparison plots: scale = 66, x-axis = meanOcc, y axis = freq of meanOcc vals (hist)
bbs_top = bbs_allscales %>% 
  filter(scale == "66")

lndscpe_rtfreq = ggplot(bbs_top, aes(x=meanOcc))+geom_histogram(bins = 30)+coord_cartesian(xlim = c(0, 0.95)) 
lndscpe_rtfreq
####YAAASSSS this is what I wanted to see ^^ 
#top scales freq is tighter, but also way less normal

bbs_top_E = bbs_top %>% 
  filter(Longi > -100) 
bbs_top_W = bbs_top %>% 
  filter(Longi <= -100)

#plotting comparisons: 

lndscpe_rtfreqE = ggplot(bbs_top_E, aes(x=meanOcc))+geom_histogram(bins = 30)+coord_cartesian(xlim = c(0, 0.95)) 
lndscpe_rtfreqE

lndscpe_rtfreqW = ggplot(bbs_top_W, aes(x=meanOcc))+geom_histogram(bins = 30)+coord_cartesian(xlim = c(0, 0.95)) 
lndscpe_rtfreqW

#WOW LOOK AT THOSE DISCREPANCIES!!!!


###idk what I was trying to do here, revisit w/lab notebook later 
coefs_mini = coefs %>%
  select(stateroute, OA.pmin, OA.pmax) %>%
  gather(key = OA.pmin, 
         value = occ_val, 
         OA.pmin:OA.pmax, 
         na.rm = TRUE) %>% #doubled size of df, every rte has associated max and min vals 
  rename(linemetric = OA.pmin)
ggplot(coefs_mini, aes(x = linemetric, y = occ_val))+geom_boxplot(aes(fill = linemetric))+
  theme_classic()+theme(legend.position="none")


#will go inside plotting forloop 
coef_join = coefs %>% inner_join(bbs_allscales, by = c("stateroute"="focalrte"))
#stateroutes = unique(bbs_allscales$focalrte)
s = 2001
coef_sub = subset(coef_join, coef_join$stateroute == s)


theme_set(theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
#OA
OAlog = lm(meanOcc ~ logA, data = coef_sub) #lm instead of nls, reg linear model
OApreds = data.frame(preds = predict(OAlog), scale = coef_sub$scale, logA = coef_sub$logA) 


plot1 = ggplot(coef_join, aes(x = logA, y = meanOcc))+geom_point()+
  geom_smooth(aes(colour = stateroute), intercept = OA.xmid, slope = OA.pslope) +labs(x = "Log area", y = "Mean % Occupancy")+
  coord_cartesian(ylim = c(0, 1))


ggplot(coef_join, aes(x = logA, y = meanOcc))+geom_point()+geom_point(aes(x = OA.min))
#in order to plot min and maxes need x and y, just duplicating vals in both directions rn!


####Old plotting code from logfcn####
preds.df = data.frame(stateroute = numeric(), logA = numeric(), 
                      OApreds= numeric(), ONpreds = numeric(), 
                      CApreds = numeric(), CNpreds = numeric(),
                      TApreds = numeric(), TNpreds = numeric())


pdf("output/plots/Molly Plots/BBS_scaleplots.pdf", onefile = TRUE)
coef_join = coefs %>% inner_join(bbs_allscales3, by = c("stateroute"="focalrte"))
stateroutes = unique(bbs_allscales3$focalrte)

#extracting predicted values and plotting in same loop
for (s in stateroutes) {
  theme_set(theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
  coef_sub = subset(coef_join, coef_join$stateroute == s)
  logA = coef_sub$logA
  
  #OA
  OApreds = logistic_fcn(coef_sub[,33], coef_sub[,2], coef_sub[,3], coef_sub[,4]) 
  plot1 = ggplot(coef_sub, aes(x = logA, y = meanOcc))+geom_point(colour = "firebrick")+
    geom_line(aes(x = logA, y = OApreds), color = "navy") +labs(x = "Log area", y = "Mean % Occupancy")+
    coord_cartesian(ylim = c(0, 1))
  
  
  #ON
  ONpreds = logistic_fcn(coef_sub[,34], coef_sub[,6], coef_sub[,7], coef_sub[,8])
  plot2 = ggplot(coef_sub, aes(x = logN, y = meanOcc))+geom_point(colour = "firebrick")+
    geom_line(aes(x = logN, y = ONpreds), color = "navy") +labs(x = "Log abundance", y = "Mean % Occupancy")+
    coord_cartesian(ylim = c(0, 1))
  
  
  #CA
  CApreds = logistic_fcn(coef_sub[,33], coef_sub[,10], coef_sub[,11], coef_sub[,12])
  plot1_2= ggplot(coef_sub, aes(x = logA, y = pctCore))+geom_point(colour = "turquoise")+
    geom_line(aes(x = logA, y = CApreds), color = "navy")+labs(x = "Log area", y = "% Core Occupancy")+
    coord_cartesian(ylim = c(0, 1))
  
  #aveN
  #CN
  CNpreds = logistic_fcn(coef_sub[,34], coef_sub[,14], coef_sub[,15], coef_sub[,16])
  plot2_2= ggplot(coef_sub, aes(x = logN, y = pctCore))+geom_point(colour = "turquoise")+
    geom_line(aes(x = logN, y = CNpreds), color = "navy")+labs(x = "Log abundance", y = "% Core Occupancy")+
    coord_cartesian(ylim = c(0, 1))
  
  #using exponential function since higher explanatory power than pwr function
  #TA
  TApreds =  coef_sub[,35]*(coef_sub[,18]) #35 = optimum; replacing ^ with * bc natural log, removing -1
  plot1_3 = ggplot(coef_sub, aes(x = lnA, y = log(pctTran)))+geom_point(colour = "olivedrab")+
    geom_line(aes(x = lnA, y = log(TApreds)), color = "navy") +labs(x = "Log area", y = "% Transient Occupancy")+
    coord_cartesian(ylim = c(-4, 1)) #FIX
  
  #TN
  TNpreds = coef_sub[,36]*(coef_sub[,22])
  plot2_3 = ggplot(coef_sub, aes(x = lnN, y = log(pctTran)))+geom_point(colour = "olivedrab")+
    geom_line(aes(x = lnN, y = TNpreds), color = "navy")+labs(x = "Log abundance", y = "% Transient Occupancy")+
    coord_cartesian(ylim = c(-4, 1))
  
  #storing plots
  predplot = grid.arrange(plot1, plot2, plot1_2, plot2_2, plot1_3, plot2_3,
                          ncol=2, top = paste("predplot_", s, sep = ""))
  #storing preds:
  temp.df = data.frame(stateroute = s, logA = logA, 
                       OApreds= OApreds , ONpreds = ONpreds, 
                       CApreds = CApreds, CNpreds = CNpreds,
                       TApreds = TApreds, TNpreds = TNpreds)
  preds.df = rbind(preds.df, temp.df)
  
}
dev.off()
write.csv(preds.df, "C:/git/core-transient/scripts/R-scripts/scale_analysis/preds.csv", row.names = FALSE)

