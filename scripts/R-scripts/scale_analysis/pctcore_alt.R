#Percent-Core Occupancy-scale analysis: alternative to mean occupancy for ease of reader + metric 

####Occ-scale analysis####

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
bbs_allsub = bbs_allscales %>% filter(focalrte == 33901 | focalrte == 72035 | focalrte == 60024)
bbs_allsub$focalrte = as.factor(bbs_allsub$focalrte)
#use this to assign neutral colors for each factor level per what color scheme is ideal?


pred_plot = ggplot(bbs_allscales, aes(x = logA, y = meanOcc))+geom_line(aes(group = focalrte), color = "grey")+
  theme_classic()+geom_line(data = bbs_allsub, aes(x = logA, y = meanOcc, group = as.factor(focalrte), color = as.factor(focalrte)), size = 2)+ #geom_smooth(model = lm, color = 'red')+
  labs(x = "Log Area", y = "Mean Community Occupancy")+scale_color_viridis(discrete = TRUE) 
pred_plot #remember to thicken black line 

#bbs_allscales$preds2 = predict(mod2)
pred_plot2 = ggplot(bbs_allscales, aes(x = logN, y = meanOcc))+geom_line(aes(group = focalrte), color = "grey")+
  theme_classic()+geom_line(data = bbs_allsub, aes(x = logN, y = meanOcc, group = as.factor(focalrte), color = as.factor(focalrte)), size = 2)+ #geom_smooth(model = lm, color = 'red')+
  labs(x = "Log Abundance", y = "Mean Community Occupancy") 
pred_plot2 #remember to thicken black line 

abun_p = ggplot(bbs_allscales, aes(x = logA, y = meanOcc, colour = logN))+
  geom_line(aes(group = focalrte))+
  theme_classic()+ #geom_smooth(model = lm, color = 'red')+
  labs(x = "Log Area", y = "Mean Community Occupancy")
abun_p
?geom_line 



####Occ-line equation for pct Core scale relationship####

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
#4)the scale at which mean occupancy first reaches or surpasses 50% 
#At what scale, at what area in km must we reach in order to reliably see consistent occupancy 
#PREDICTION: in areas of fairly uniform habitat type, this scale should be lower 
#PREDICTION: in areas of fairly high habitat heterogeneity, this scale should be higher 
#5)and we look at the straightness or curvature of the actual data 
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
OA.df = data.frame(stateroute = numeric(), OA.min = numeric(), OA.max = numeric(), 
                   OA.slope = numeric(), 
                   OA.mid = numeric(), 
                   OA.curvature = numeric())
ON.df = data.frame(stateroute = numeric(), ON.min = numeric(), ON.max = numeric(), 
                   ON.slope = numeric(), 
                   ON.mid = numeric(), 
                   ON.curvature = numeric())



#read in data for processing
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
levels(bbs_allscales$scale)
unique(bbs_allscales$scale)
length(unique(bbs_allscales$focalrte))
bbs_allscales = na.omit(bbs_allscales)
length(unique(bbs_allscales$focalrte)) #ommitted 2 routes w/missing data


####coefs####
stateroutes = unique(bbs_allscales$focalrte)

#do I even need a loop? can't I just group by stateroute and calc these ?


for(s in stateroutes){
  logsub = subset(bbs_allscales, bbs_allscales$focalrte == s)  
  #OA 
  OAlog = lm(meanOcc ~ logA, data = logsub) #lm instead of nls, reg linear model
  logsub$OApreds = predict(OAlog)
  #OApred_df = data.frame(preds = predict(OAlog), scale = logsub$scale, logA = logsub$logA)  #get preds -> is predicting unique per scale, all clear
  #ACTUAL stats (for plotting data pts): 
  OA.min = min(logsub$meanOcc[logsub$logA == min(logsub$logA)])
  OA.max = logsub$meanOcc[logsub$logA == max(logsub$logA)]
  OA.mid = min(logsub$logA[logsub$meanOcc >= 0.5]) 
  OA.slope = ((OA.max - OA.min)/(max(logsub$logA[logsub$meanOcc == max(logsub$meanOcc)]) - min(logsub$logA[logsub$meanOcc == min(logsub$meanOcc)])))
  #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
  #save as an area, not a "scale" 
  
  OA.vec = logsub$meanOcc #vector for a given focal rte s, actual value
  OA.pvec = logsub$OApreds #vector for given focal rte s, pred values
  OA.curvature =  sum(abs(OA.vec - OA.pvec)) 
  #AUC proxy - taking diff between actual and predicted mid vals at EVERY scale and adding together
  OAmodel = data.frame(stateroute = s, OA.min, OA.max, OA.slope, 
                       OA.mid, OA.curvature)
  
  OA.df = rbind(OA.df, OAmodel)
  #
  
  #ON 
  ONlog = lm(meanOcc ~ logN, data = logsub) #lm instead of nls, reg linear model
  logsub$ONpreds = predict(ONlog)
  #ONpred_df = data.frame(preds = predict(ONlog), scale = logsub$scale, logN = logsub$logN)  #get preds -> is predicting unique per scale, all clear
  #ACTUAL stats (for plotting data pts): 
  ON.min = min(logsub$meanOcc[logsub$logN == min(logsub$logN)])
  ON.max = logsub$meanOcc[logsub$logN == max(logsub$logN)]
  ON.mid = min(logsub$logN[logsub$meanOcc >= 0.5]) 
  ON.slope = ((ON.max - ON.min)/(max(logsub$logN[logsub$meanOcc == max(logsub$meanOcc)]) - min(logsub$logN[logsub$meanOcc == min(logsub$meanOcc)])))
  #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
  #save as an area, not a "scale" 
  
  ON.vec = logsub$meanOcc #vector for a given focal rte s, actual value
  ON.pvec = logsub$ONpreds #vector for given focal rte s, pred values
  ON.curvature =  sum(abs(ON.vec - ON.pvec)) 
  #NUC proxy - taking diff between actual and predicted mid vals at EVERY scale and adding together
  ONmodel = data.frame(stateroute = s, ON.min, ON.max, ON.slope, 
                       ON.mid, ON.curvature)
  
  ON.df = rbind(ON.df, ONmodel)
  #
}  


#join all together using inner_join by focal rte, not cbind 
coefs = OA.df %>% 
  inner_join(ON.df, OA.df, by = "stateroute")

write.csv(coefs, "scripts/R-scripts/scale_analysis/coefs.csv", row.names = FALSE) 
#updated 11/21, removal of redundant coefs and inclusion of ON


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


