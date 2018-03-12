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
library(viridis)

# To run this script, you need temperature, precip, etc data, 
# which are currently stored in the following directories off of github: 

# Data directories
tempdatadir = '//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/BIOCLIM_meanTemp/'
precipdata = '//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/2-25-2011/prec/'
ndvidata = "//bioark.ad.unc.edu/HurlbertLab/GIS/MODIS NDVI/"
BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/Intermediate scripts/BBS scaled/'


####Cross-scale analysis and visualization####
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
levels(bbs_allscales$scale)
unique(bbs_allscales$scale)


mod1 = lm(pctCore~logA, data = bbs_allscales) #89%
mod2 = lm(pctCore~logN, data = bbs_allscales) #90%
summary(mod1)

plot(pctCore~logA, data = bbs_allscales, xlab = "Log Area" , ylab = "Percent Core Species in Community")
plot(pctCore~logN, data = bbs_allscales, xlab = "Average Abundance" , ylab = "Percent Core Species in Community")
#^^same pattern roughly; abundance describes ~same amt of variance as area so serves as a good proxy 



bbs_allsub = bbs_allscales %>% filter(focalrte == 33901 | focalrte == 72035 | focalrte == 44032)
bbs_allsub$focalrte = as.factor(bbs_allsub$focalrte)
#use this to assign diff colors for each factor level per what color scheme is ideal?


pred_plot = ggplot(bbs_allscales, aes(x = logA, y = pctCore))+geom_line(aes(group = focalrte), color = "grey")+
  theme_classic()+geom_line(data = bbs_allsub, aes(x = logA, y = pctCore, group = as.factor(focalrte), color = as.factor(focalrte)), size = 2)+ #geom_smooth(model = lm, color = 'red')+
  labs(x = "Log Area", y = "Percent Core Species in Community")+scale_color_viridis(discrete = TRUE)+
  theme(axis.title = element_text(size = 18))+theme(legend.position = c(0.80, 0.25)) 
pred_plot 

#reevaluate once new curviness vals extracted 



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


####Extract coefficients from scale-occupancy relationships for analysis####
PCA.df = data.frame(stateroute = numeric(), PCA.min = numeric(), PCA.max = numeric(), 
                   PCA.slope = numeric(), 
                   PCA.mid = numeric(), 
                   PCA.curvature = numeric())
PCN.df = data.frame(stateroute = numeric(), PCN.min = numeric(), PCN.max = numeric(), 
                   PCN.slope = numeric(), 
                   PCN.mid = numeric(), 
                   PCN.curvature = numeric())



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
  #PCA 
  PCAlog = lm(pctCore ~ logA, data = logsub) #lm instead of nls, reg linear model
  logsub$PCApreds = predict(PCAlog)
  #PCApred_df = data.frame(preds = predict(PCAlog), scale = logsub$scale, logA = logsub$logA)  #get preds -> is predicting unique per scale, all clear
  #ACTUAL stats (for plotting data pts): 
  PCA.min = min(logsub$pctCore[logsub$logA == min(logsub$logA)])
  PCA.max = logsub$pctCore[logsub$logA == max(logsub$logA)]
  PCA.mid = min(logsub$logA[logsub$pctCore >= 0.5]) 
  PCA.slope = ((PCA.max - PCA.min)/(max(logsub$logA[logsub$pctCore == max(logsub$pctCore)]) - min(logsub$logA[logsub$pctCore == min(logsub$pctCore)])))
  #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
  #save as an area, not a "scale" 
  
  PCA.vec = logsub$pctCore #vector for a given focal rte s, actual value
  PCA.pvec = logsub$PCApreds #vector for given focal rte s, pred values
  PCA.curvature =  sum(PCA.vec - PCA.pvec) 
  #AUC proxy - taking diff between actual and predicted mid vals at EVERY scale and adding together
  PCAmodel = data.frame(stateroute = s, PCA.min, PCA.max, PCA.slope, 
                       PCA.mid, PCA.curvature)
  
  PCA.df = rbind(PCA.df, PCAmodel)
  #
  
  #PCN 
  PCNlog = lm(pctCore ~ logN, data = logsub) #lm instead of nls, reg linear model
  logsub$PCNpreds = predict(PCNlog)
  #PCNpred_df = data.frame(preds = predict(PCNlog), scale = logsub$scale, logN = logsub$logN)  #get preds -> is predicting unique per scale, all clear
  #ACTUAL stats (for plotting data pts): 
  PCN.min = min(logsub$pctCore[logsub$logN == min(logsub$logN)])
  PCN.max = logsub$pctCore[logsub$logN == max(logsub$logN)]
  PCN.mid = min(logsub$logN[logsub$pctCore >= 0.5]) 
  PCN.slope = ((PCN.max - PCN.min)/(max(logsub$logN[logsub$pctCore == max(logsub$pctCore)]) - min(logsub$logN[logsub$pctCore == min(logsub$pctCore)])))
  #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
  #save as an area, not a "scale" 
  
  PCN.vec = logsub$pctCore #vector for a given focal rte s, actual value
  PCN.pvec = logsub$PCNpreds #vector for given focal rte s, pred values
  PCN.curvature =  sum(PCN.vec - PCN.pvec)
  #NUC proxy - taking diff between actual and predicted mid vals at EVERY scale and adding together
  PCNmodel = data.frame(stateroute = s, PCN.min, PCN.max, PCN.slope, 
                       PCN.mid, PCN.curvature)
  
  PCN.df = rbind(PCN.df, PCNmodel)
  #
}  


#join all together using inner_join by focal rte, not cbind 
core_coefs = PCA.df %>% 
  inner_join(PCN.df, PCA.df, by = "stateroute")

write.csv(core_coefs, "scripts/R-scripts/scale_analysis/core_coefs.csv", row.names = FALSE) 
#updated 1/31, removal of redundant coefs and inclusion of ON


####Env analysis####
#Background data prep is just environmental data prep, nothing involving occupancy until env_all and core_coefs merge

####abitat hetero measures and scale independent of coefs (for predictions)####   
core_coefs = read.csv("scripts/R-scripts/scale_analysis/core_coefs.csv", header = TRUE) #AUC etc. 
env_all = read.csv("scripts/R-scripts/scale_analysis/intermed/env_all.csv", header = TRUE) #AUC etc. 
#calc areas of scales and run against logA consistently ? 

env_all$area = env_all$scale #*40 etc etc etc

####Coefs to habitat het####

#coefs to top scale env characterizing data
core_env_coefs = env_all %>%
  inner_join(core_coefs, by = "stateroute") #and also join single rte

#since now reflective of all scales, 62370 rows, 36 cols 
write.csv(core_env_coefs, "scripts/R-scripts/scale_analysis/core_env_coefs.csv", row.names = FALSE)
#updated 12/14


####Coef & habitat heterogeneity models####
core_env_coefs = read.csv("scripts/R-scripts/scale_analysis/core_env_coefs.csv", header = TRUE) #same # of cols as old 

#check out cov matrix to inform model generation and predictions:
covmatrix = round(cor(core_env_coefs[, 3:ncol(core_env_coefs)]), 2) #since clipped stateroute don't need to clip again - should I clip scale?
covmatrix = as.data.frame(covmatrix)
write.csv(covmatrix, "scripts/R-scripts/scale_analysis/core_covmatrix.csv", row.names = FALSE)
#mean and var - interpret how covary with coefs and direction - i.e. ndvi mean covaries positively with pmin, 
#but elev mean covaries negatively with pmin 
#and both variances covary negatively with pmin 
#ndvi mean covaries negatively with pthresh, elev mean positively
#no other variables have this divergent relationship in directionality of covariance, 
#ndvi var and elev var always in unison when strong
#hab het variance measures: pslope and pthresh positive, pmin and pmax both negative covariance


####UP NEXT: run series of models and reassess/renew plots from spring####
# nested loop for examining variation in coefs/fitted curves explained by env heterogeneity 
#so: response = coefficients = dependent; predictor = environmental heterogeneity = independent

#first need to make sure JUST looking at variance characterizing site, not means -> filter out 

core_rsqrd_hetero = data.frame(dep = character(), ind = character(), 
                          r2 = numeric(), adjr = numeric(), corr_r = numeric(), uppr = numeric(), lowr = numeric())
#modify to include plotting of obs values for each stateroute vs pred line 
#and plot these with r squared vals as annotations to plots too 

for (d in 3:6) { #adjust columns appropriately -> make sure correct order of ind and dep vars!
  for (i in 8:17) {
    tempmod = lm(core_env_coefs[,d] ~ core_env_coefs[,i])
    tempcor = cor.test(core_env_coefs[,d], core_env_coefs[,i], method = "pearson")
    
    
    tempdf = data.frame(dep = names(core_env_coefs)[d], 
                        ind = names(core_env_coefs)[i], 
                        r2 = summary(tempmod)$r.squared, 
                        adjr = summary(tempmod)$adj.r.squared, 
                        corr_r = as.numeric(tempcor$estimate), 
                        uppr = as.numeric(tempcor$conf.int[2]), 
                        lowr = as.numeric(tempcor$conf.int[1]))
    
    # templot = ggplot(data = env_coefs, aes(x = env_coefs[,i], y = env_coefs[,d]))+geom_point()+
    #   geom_line(aes(y = predict(tempmod), color = 'Model'))+
    #   labs(x = names(env_coefs)[i], y = names(env_coefs)[d])+guides(color = "none")+
    #   annotate("text", x = 0.5*max(env_coefs[,i]), y = 0.5*max(env_coefs[,d]), 
    #            label = paste("italic(R) ^ 2 ==", tempdf$r2, sep = ""), parse = TRUE, 
    #            color = "red", size = 5.5) 
    # ggsave(templot, filename=paste("env_coefs", names(env_coefs)[d], 
    #                                names(env_coefs)[i],".png",sep=""))
    
    core_rsqrd_hetero = rbind(core_rsqrd_hetero, tempdf)
  }
}


write.csv(core_rsqrd_hetero, "scripts/R-scripts/scale_analysis/core_rsqrd_hetero.csv", row.names = FALSE) 
#updated 01/22 using corrected hab_het vals, only variances characterizing sites


####Visually Characterizing measures of habitat heterogeneity####
core_rsqrd_hetero = read.csv("scripts/R-scripts/scale_analysis/core_rsqrd_hetero.csv", header = TRUE)
# hab_het = read.csv("scripts/R-scripts/scale_analysis/hab_het.csv", header = TRUE)

r_plot = ggplot(data = core_rsqrd_hetero, aes(y = corr_r))+geom_col(aes(x=dep))+facet_wrap(~ind)+
  theme_bw()
r_plot 


core_env_coefs = read.csv("scripts/R-scripts/scale_analysis/core_env_coefs.csv", header = TRUE)

#scale on x and r on y, panel by coef of interest, line color by var measure

# goal plot -> ggplot(envcoefs, aes(x = scale, y = corr_r))+geom_line(aes(color = dep))+facet_wrap(~ind)
#I want a corr_r value for every dep and ind variable at every scale, for every focal
#for every scale, for every focal route - will have a LOT - maybe just do a subset for meeting 

#the correlation coefficients themselves won't change, bc representative of the overall 
#occ-scale relationship, that's fine - the hab_het vals will change though bc measures 
#at each scale 
#starting at scale of 1 since that's lowest res we have for habhet across scales, 
#rerun previous dep/ind loop with new mods


core_scales_hetero = data.frame(dep = character(), ind = character(), 
                           r2 = numeric(), adjr = numeric(), corr_r = numeric(), 
                           uppr = numeric(), lowr = numeric(), scale = numeric())
#modify to include plotting of obs values for each stateroute vs pred line 
#and plot these with r squared vals as annotations to plots too 
#setwd("C:/git/core-transient/output/plots/Molly_Plots/habhet/")
scales = unique(core_env_coefs$scale)


for (s in scales) {
  env_coefs2 = core_env_coefs %>% 
    filter(scale == s)
  for (d in 3:6) { #adjust columns appropriately -> make sure correct order of ind and dep vars!
    for (i in 8:17) {
      tempmod = lm(env_coefs2[,d] ~ env_coefs2[,i])
      tempcor = cor.test(env_coefs2[,d], env_coefs2[,i], method = "pearson")
      
      
      tempdf = data.frame(dep = names(env_coefs2)[d], 
                          ind = names(env_coefs2)[i], 
                          r2 = summary(tempmod)$r.squared, 
                          adjr = summary(tempmod)$adj.r.squared, 
                          corr_r = as.numeric(tempcor$estimate),
                          uppr = as.numeric(tempcor$conf.int[2]),
                          lowr = as.numeric(tempcor$conf.int[1]),
                          scale = s)
      
      # templot = ggplot(data = env_coefs, aes(x = env_coefs[,i], y = env_coefs[,d]))+geom_point()+
      #   geom_line(aes(y = predict(tempmod), color = 'Model'))+
      #   labs(x = names(env_coefs)[i], y = names(env_coefs)[d])+guides(color = "none")+
      #   annotate("text", x = 0.5*max(env_coefs[,i]), y = 0.5*max(env_coefs[,d]), 
      #            label = paste("italic(R) ^ 2 ==", tempdf$r2, sep = ""), parse = TRUE, 
      #            color = "red", size = 5.5) 
      # ggsave(templot, filename=paste("env_coefs", names(env_coefs)[d], 
      #                                names(env_coefs)[i],".png",sep=""))
      
      core_scales_hetero = rbind(core_scales_hetero, tempdf)
    }
  }
}

write.csv(core_scales_hetero, "scripts/R-scripts/scale_analysis/core_scales_hetero.csv", row.names = FALSE) 
#updated 01/22 using corrected hab_het vals, only variances characterizing sites

#INTERPRETATION: 
#elevation and ndvi at the highest scales explain more variation in the pmin and pthresh values 
#compared to any other measures of habitat heterogeneity 
#However, elevation explains more variation than ndvi at the top scales - AND 
#the gulf between ndvi and elevation in terms of explanatory power widens at the larger scales 
# - the two measures are closer together in importance at the lowest scales. 
#NDVI variance at local scale actually has an 'outlier' point further above even local elev variation 
#depending on which coefficient metric that corresponds to, it may be that ndvi explains more 
#local variation in heterogeneity and variation at the lower scales of the occ-scale relationship 
#while elevational heterogeneity explains variation at the higher scales of the occ-scale relationship?  



####Alt figures for pct Core####

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

####Plotting NULL all routes with 3 highlighted "types####
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
core_coefs = read.csv("scripts/R-scripts/scale_analysis/core_coefs.csv", header = TRUE) #AUC etc.

coefs_ranked = core_coefs %>% 
  arrange(PCA.curvature) #middle teal line should be least curvy 


bbs_allsub = bbs_allscales %>% filter(focalrte == 33901 | focalrte == 88005 | focalrte == 72035) #no longer 44032!
bbs_allsub$focalrte = as.factor(bbs_allsub$focalrte)
#use this to assign diff colors for each factor level per what color scheme is ideal?


pred_plot = ggplot(bbs_allscales, aes(x = logA, y = pctCore))+geom_line(aes(group = focalrte), color = "grey")+
  theme_classic()+geom_line(data = bbs_allsub, aes(x = logA, y = pctCore, group = as.factor(focalrte), color = as.factor(focalrte)), size = 2)+ #geom_smooth(model = lm, color = 'red')+
  labs(x = "Log Area", y = "Proportion Core Species in Community")+scale_color_viridis(discrete = TRUE, name = "BBS route")+
  theme(axis.title = element_text(size = 18))+theme(legend.position = "none") 
pred_plot 

pred_abuns = ggplot(bbs_allscales, aes(x = logN, y = pctCore))+geom_line(aes(group = focalrte), color = "grey")+
  theme_classic()+geom_line(data = bbs_allsub, aes(x = logN, y = pctCore, group = as.factor(focalrte), color = as.factor(focalrte)), size = 2)+ #geom_smooth(model = lm, color = 'red')+
  labs(x = "Log Abundance", y = "")+scale_color_viridis(discrete = TRUE, name = "BBS route")+
  theme(axis.title = element_text(size = 18))+theme(legend.position = c(0.80, 0.25)) 
pred_abuns 

p1 = grid.arrange(pred_plot, pred_abuns, ncol = 2)

####Corrr_confints alternate with pctCore scale relationship coefs in lieu of original coefs####
core_scales_hetero = read.csv("scripts/R-scripts/scale_analysis/core_scales_hetero.csv", header = TRUE)

scales_hetero_v = core_scales_hetero %>% 
  filter(dep == "elev.var" | dep == "ndvi.var") 

#scale on x and r on y, panel by coef of interest, line color by var measure
ggplot(scales_hetero_v, aes(x = scale, y = corr_r))+
  geom_line(aes(color = dep))+facet_wrap(~ind)+theme_classic()+
  geom_abline(intercept = 0, slope = 0)+
  theme_classic()+theme(axis.title = element_text(size = 18), axis.text = element_text(size = 16))+
  labs(x = "Number of aggregated BBS Routes", y = "Pearson's correlation estimate")+theme(legend.position = c(0.80, 0.20)) 

#I want a corr_r value for every dep and ind variable at every scale, for every focal
#for every scale, for every focal route - will have a LOT - maybe just do a subset for meeting 

#the correlation coefficients themselves won't change, bc representative of the overall 
#occ-scale relationship, that's fine - the hab_het vals will change though bc measures 
#at each scale 
#starting at scale of 1 since that's lowest res we have for habhet across scales, 
#rerun previous dep/ind loop with new mods


#at top scales, with just variances - diamond shape figure that parallels prediction table (alt to outcome table)
scales_hetero2 = core_scales_hetero %>% 
  filter(scale == 66) %>% 
  filter(dep == "elev.var" | dep == "ndvi.var") %>% 
  filter(ind == "PCA.curvature" | ind == "PCA.max" | ind == "PCA.mid"| ind == "PCA.min" | ind == "PCA.slope")

ggplot(scales_hetero2, aes(x = ind, y = corr_r))+
  geom_pointrange(aes(shape = dep, ymin = lowr, ymax = uppr))+geom_abline(intercept = 0, slope = 0)+
  theme_classic()+theme(axis.title = element_text(size = 18), axis.text = element_text(size = 16))+
  labs(x = "Occupancy-scale parameters", y = "Pearson's correlation estimate")+
  scale_x_discrete(limit = c("PCA.curvature","PCA.max","PCA.mid","PCA.min","PCA.slope"),
                   labels = c("Curvature","Max","Scale 0.5", "Min", "Slope"))+
  scale_shape_discrete(name="Habitat Heterogeneity",
                       breaks=c("elev.var", "ndvi.var"),
                       labels=c("Elevation", "NDVI"))
