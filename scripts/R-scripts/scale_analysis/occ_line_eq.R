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

#3) Curviness is a great proxy for calculating the AUC per focal route, but it doesn't allow us to see how that changes across scales. 
  #Considering having a second df output from the creation of coefs that allows us to see how those individual deviances change 
  #esp across scales. 

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

####Extract coefficients from scale-occupancy relationships for analysis####
OA.df = data.frame(stateroute = numeric(), OA.min = numeric(), OA.max = numeric(), OA.slope = numeric(), 
                   OA.thresh = numeric(), 
                   OA.pmin = numeric(), OA.pmax = numeric(), OA.pslope = numeric(),  
                   OA.pthresh = numeric(), 
                   OA.r2 = numeric(), OA.AUC = numeric()) 

CA.df = data.frame(stateroute = numeric(), CA.min = numeric(), CA.max = numeric(), CA.slope = numeric(), 
                   CA.thresh = numeric(), 
                   CA.pmin = numeric(), CA.pmax = numeric(), CA.pslope = numeric(),
                   CA.pthresh = numeric(), 
                   CA.r2 = numeric(), CA.AUC = numeric())

TA.df = data.frame(stateroute = numeric(), TA.min = numeric(), TA.max = numeric(), TA.slope = numeric(), 
                   TA.thresh = numeric(), 
                   TA.pmin = numeric(), TA.pmax = numeric(), TA.pslope = numeric(), 
                   TA.pthresh = numeric(), 
                   TA.r2 = numeric(), TA.AUC = numeric())

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
    OA.AUC =  sum(abs(OA.xmid - OA.pxmid)) #AUC proxy - taking diff between actual and predicted mid vals at EVERY scale and adding together
   

    OAmodel = data.frame(stateroute = s, OA.min, OA.max, OA.slope, 
                                          OA.thresh, 
                                          OA.pmin, OA.pmax, OA.pslope,
                                          OA.pthresh, 
                                          OA.r2, OA.AUC)
    
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
    CA.AUC =  sum(abs(CA.xmid - CA.pxmid)) #AUC proxy - taking diff between actual and predicted mid vals at EVERY scale and adding together
     
    
    CAmodel = data.frame(stateroute = s, CA.min, CA.max, CA.slope, 
               CA.thresh, 
               CA.pmin, CA.pmax, CA.pslope, 
               CA.pthresh, 
               CA.r2, CA.AUC)
    
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
    TA.AUC =  sum(abs(TA.xmid - TA.pxmid)) #AUC proxy - taking diff between actual and predicted mid vals at EVERY scale and adding together
  
    TAmodel = data.frame(stateroute = s, TA.min, TA.max, TA.slope,  
               TA.thresh,
               TA.pmin, TA.pmax, TA.pslope,
               TA.pthresh,
               TA.r2, TA.AUC)


  TA.df = rbind(TA.df, TAmodel)

  
} #end of loop 
  
  
#join all together using inner_join by focal rte, not cbind 
coefs = OA.df %>% 
  inner_join(CA.df, OA.df, by = "stateroute") %>% 
  inner_join(TA.df, OA.df, by = "stateroute") 
 
write.csv(coefs, "scripts/R-scripts/scale_analysis/coefs.csv", row.names = FALSE) #updated 09/21
#exp mods have much better r2 vals for pctTran than power 
#checked, working correctly 08/28, output not NA's but normal!

####Plotting occupancy-scale relationships with observed and predicted values####
#work in progress
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
  filter(scale == "seg50")

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
one_rtfreqE = ggplot(bbs_one_E, aes(x=meanOcc))+geom_histogram(bins = 30) + coord_cartesian(xlim = c(0, 0.95), ylim = c(0, 80))+labs(title = "Single rte East") 
one_rtfreqE

one_rtfreqW = ggplot(bbs_one_W, aes(x=meanOcc))+geom_histogram(bins = 30) + coord_cartesian(xlim = c(0, 0.95), ylim = c(0, 80))+labs(title = "Single rte West") 
one_rtfreqW


#Secondary comparison plots: scale = 66, x-axis = meanOcc, y axis = freq of meanOcc vals (hist)
bbs_top = bbs_allscales %>% 
  filter(scale == "66")

lndscpe_rtfreq = ggplot(bbs_top, aes(x=meanOcc))+geom_histogram(bins = 30)+coord_cartesian(xlim = c(0, 0.95), ylim = c(0, 80)) 
lndscpe_rtfreq
####YAAASSSS this is what I wanted to see ^^ 
#top scales freq is tighter, but also way less normal

bbs_top_E = bbs_top %>% 
  filter(Longi > -100) 
bbs_top_W = bbs_top %>% 
  filter(Longi <= -100)

#plotting comparisons: 

lndscpe_rtfreqE = ggplot(bbs_top_E, aes(x=meanOcc))+geom_histogram(bins = 30)+coord_cartesian(xlim = c(0.5, 0.95), ylim = c(0, 80))+labs(title = "Landscape East") 
lndscpe_rtfreqE

lndscpe_rtfreqW = ggplot(bbs_top_W, aes(x=meanOcc))+geom_histogram(bins = 30)+coord_cartesian(xlim = c(0.5, 0.95), ylim = c(0, 80))+labs(title = "Landscape West")  
lndscpe_rtfreqW

#WOW LOOK AT THOSE DISCREPANCIES!!!!
comparisons = grid.arrange(one_rtfreqE, one_rtfreqW, lndscpe_rtfreqE, lndscpe_rtfreqW)

#left off and stopped here 09/10
#now plot curvy vals at diff routes bc effectively summarizing area under curve across scales for each route 
#but are we interested in SEEING the way that AUC varies across scales moreso than across longitudes 
#so y axis = curvy, x axis = longitude (?) 

#so AUC should vary with scale, but also with east vs west divide -> greater AUC vals in West = greater deviance from obs on avg
#lati + longi + coefs 

coefs = coefs %>% 
  left_join(goodrtes, by = "stateroute")

auc_plot = ggplot(coefs, aes(x = OA.AUC))+geom_histogram(bins = 30)
auc_plot
#looks pretty normal which is good, little to no area under curve means close good fit with some minor tendency towards overestimating and going too high 
AUC_E = coefs %>% 
  filter(Longi > -100) 
AUC_W = coefs %>% 
  filter(Longi <= -100)


AUC_Eplot = ggplot(AUC_E, aes(x = OA.AUC))+geom_histogram(bins = 30)+
  coord_cartesian(xlim = c(-6e-14, 6e-14), ylim = c(0, 75))+labs(y = "AUC East")
AUC_Eplot #mirrors the first dist 

AUC_Wplot = ggplot(AUC_W, aes(x = OA.AUC))+geom_histogram(bins = 30)+
  coord_cartesian(xlim = c(-6e-14, 6e-14), ylim = c(0, 75))+labs(y = "AUC West")
AUC_Wplot

comp = grid.arrange(AUC_Eplot, AUC_Wplot)
#bigger spread, def not normal dist if we narrowed down