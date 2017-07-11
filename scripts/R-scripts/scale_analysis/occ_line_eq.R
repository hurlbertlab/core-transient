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

####Extract coefficients from scale-occupancy relationships for analysis####
OA.df = data.frame(stateroute = numeric(), OA.alt_xmid_pred = numeric(), OA.alt_xmid_dev= numeric(), OA.max= numeric(), OA.min= numeric(), OA.r2= numeric())
ON.df = data.frame(stateroute = numeric(), ON.alt_xmid_pred = numeric(), ON.alt_xmid_dev= numeric(), ON.max= numeric(), ON.min= numeric(), ON.r2= numeric())
CA.df = data.frame(stateroute = numeric(), CA.alt_xmid_pred = numeric(), CA.alt_xmid_dev= numeric(), CA.max= numeric(), CA.min= numeric(), CA.r2= numeric())
CN.df = data.frame(stateroute = numeric(), CN.alt_xmid_pred = numeric(), CN.alt_xmid_dev= numeric(), CN.max= numeric(), CN.min= numeric(), CN.r2= numeric())
# TA.df = data.frame(stateroute = numeric(), TAexp= numeric(), TApow = numeric(), TAexp.r2 = numeric(), TApow.r2 = numeric())
# TN.df = data.frame(stateroute = numeric(), TNexp= numeric(), TNpow = numeric(), TNexp.r2 = numeric(), TNpow.r2 = numeric())
warnings = data.frame(stateroute = numeric(), warning = character())


#read in data for processing
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
stateroutes = unique(bbs_allscales$focalrte) #this stuff is the same, looks normal ^


#07/11 version of tryCatch
for(s in stateroutes){
  logsub = subset(bbs_allscales, bbs_allscales$focalrte == s)  
  #fitting the log curve for area (for each route)
  
  #OA 
  OAmodel = tryCatch({
    OAlog = lm(meanOcc ~ logA, data = logsub) #lm instead of nls, reg linear model
    OApred = predict(OAlog) #get preds -> is predicting unique per scale, all clear
    #need to link back to df tho so can filter out by scale? 
    OAlm.r2 = lm(logsub$meanOcc ~ OApred) #get r2 from model 
    OA.alt_xmid = logsub$meanOcc[logsub$scale == 3] #@ scale == 3, for a given focal rte s, actual value
    #logsub[21,3] achieves same thing
    
    OA.alt_xmid_pred = OApred[21]
    OA.alt_xmid_dev = (OApred[21] - OA.alt_xmid)^2 #squared deviance of pred from actual val #need pred AT SCALE = 3 THO
    #know that it hs to be the nth item if 83 scales consistent in order in logsub,
    #so can I just extract it at OApred[n]? 18 below-route scales, then 1+ aggregated so 3 -> 21
    #find better way but so far, workable, and doesn't involve tacking the preds onto the logsub df
    
    OA.max = max(OApred) # @ max scale - what point is at the "end of the line", for a given focal rte s?
    OA.min = min(OApred) # @ min scale - what point is at the beginning of the line, for a given focal rte s?
    OA.r2 <- summary(OAlm.r2)$r.squared
    data.frame(stateroute = s, OA.alt_xmid_pred, OA.alt_xmid_dev, OA.max, OA.min, OA.r2)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    OA.alt_xmid_pred = NA
    OA.alt_xmid_dev = NA 
    OA.max = NA
    OA.min = NA
    OA.r2 = NA
    temp = data.frame(stateroute = s, OA.alt_xmid_pred, OA.alt_xmid_dev, OA.max, OA.min, OA.r2)
    return(temp)
    
  })
  OA.df = rbind(OA.df, OAmodel)
  
  #ON 
  ONmodel = tryCatch({
    ONlog = lm(meanOcc ~ logN, data = logsub) #lm instead of nls, reg linear model
    ONpred = predict(ONlog) #get preds -> is predicting unique per scale, all clear
    #need to link back to df tho so can filter out by scale? 
    ONlm.r2 = lm(logsub$meanOcc ~ ONpred) #get r2 from model 
    ON.alt_xmid = logsub$meanOcc[logsub$scale == 3] #@ scale == 3, for a given focal rte s, actual value
    #logsub[21,3] achieves same thing
    
    ON.alt_xmid_pred = ONpred[21]
    ON.alt_xmid_dev = (ONpred[21] - ON.alt_xmid)^2 #squared deviance of pred from actual val #need pred AT SCALE = 3 THO
    #know that it hs to be the nth item if 83 scales consistent in order in logsub,
    #so can I just extract it at ONpred[n]? 18 below-route scales, then 1+ aggregated so 3 -> 21
    #find better way but so far, workable, and doesn't involve tacking the preds onto the logsub df
    
    ON.max = max(ONpred) # @ max scale - what point is at the "end of the line", for a given focal rte s?
    ON.min = min(ONpred) # @ min scale - what point is at the beginning of the line, for a given focal rte s?
    ON.r2 <- summary(ONlm.r2)$r.squared
    data.frame(stateroute = s, ON.alt_xmid_pred, ON.alt_xmid_dev, ON.max, ON.min, ON.r2)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    ON.alt_xmid_pred = NA
    ON.alt_xmid_dev = NA 
    ON.max = NA
    ON.min = NA
    ON.r2 = NA
    temp = data.frame(stateroute = s, ON.alt_xmid_pred, ON.alt_xmid_dev, ON.max, ON.min, ON.r2)
    return(temp)
    
  })
  ON.df = rbind(ON.df, ONmodel)
  
  
  
  #CA
  CAmodel = tryCatch({
    CAlog = lm(pctCore ~ logA, data = logsub) #lm instead of nls, reg linear model
    CApred = predict(CAlog) #get preds -> is predicting unique per scale, all clear
    #need to link back to df tho so can filter out by scale? 
    CAlm.r2 = lm(logsub$pctCore ~ CApred) #get r2 from model 
    CA.alt_xmid = logsub$pctCore[logsub$scale == 3] #@ scale == 3, for a given focal rte s, actual value
    #logsub[21,3] achieves same thing
    
    CA.alt_xmid_pred = CApred[21]
    CA.alt_xmid_dev = (CApred[21] - CA.alt_xmid)^2 #squared deviance of pred from actual val #need pred AT SCALE = 3 THO
    #know that it hs to be the nth item if 83 scales consistent in order in logsub,
    #so can I just extract it at CApred[n]? 18 below-route scales, then 1+ aggregated so 3 -> 21
    #find better way but so far, workable, and doesn't involve tacking the preds onto the logsub df
    
    CA.max = max(CApred) # @ max scale - what point is at the "end of the line", for a given focal rte s?
    CA.min = min(CApred) # @ min scale - what point is at the beginning of the line, for a given focal rte s?
    CA.r2 <- summary(CAlm.r2)$r.squared
    data.frame(stateroute = s, CA.alt_xmid_pred, CA.alt_xmid_dev, CA.max, CA.min, CA.r2)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    CA.alt_xmid_pred = NA
    CA.alt_xmid_dev = NA 
    CA.max = NA
    CA.min = NA
    CA.r2 = NA
    temp = data.frame(stateroute = s, CA.alt_xmid_pred, CA.alt_xmid_dev, CA.max, CA.min, CA.r2)
    return(temp)
    
  })
  CA.df = rbind(CA.df, CAmodel)
  
  #CN 
  CNmodel = tryCatch({
    CNlog = lm(pctCore ~ logN, data = logsub) #lm instead of nls, reg linear model
    CNpred = predict(CNlog) #get preds -> is predicting unique per scale, all clear
    #need to link back to df tho so can filter out by scale? 
    CNlm.r2 = lm(logsub$pctCore ~ CNpred) #get r2 from model 
    CN.alt_xmid = logsub$pctCore[logsub$scale == 3] #@ scale == 3, for a given focal rte s, actual value
    #logsub[21,3] achieves same thing
    
    CN.alt_xmid_pred = CNpred[21]
    CN.alt_xmid_dev = (CNpred[21] - CN.alt_xmid)^2 #squared deviance of pred from actual val #need pred AT SCALE = 3 THO
    #know that it hs to be the nth item if 83 scales consistent in order in logsub,
    #so can I just extract it at CNpred[n]? 18 below-route scales, then 1+ aggregated so 3 -> 21
    #find better way but so far, workable, and doesn't involve tacking the preds onto the logsub df
    
    CN.max = max(CNpred) # @ max scale - what point is at the "end of the line", for a given focal rte s?
    CN.min = min(CNpred) # @ min scale - what point is at the beginning of the line, for a given focal rte s?
    CN.r2 <- summary(CNlm.r2)$r.squared
    data.frame(stateroute = s, CN.alt_xmid_pred, CN.alt_xmid_dev, CN.max, CN.min, CN.r2)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    CN.alt_xmid_pred = NA
    CN.alt_xmid_dev = NA 
    CN.max = NA
    CN.min = NA
    CN.r2 = NA
    temp = data.frame(stateroute = s, CN.alt_xmid_pred, CN.alt_xmid_dev, CN.max, CN.min, CN.r2)
    return(temp)
    
  })
  CN.df = rbind(CN.df, CNmodel)
  
#   
#   
#   # Fitting % transient
#   #TA #revisit!!
#   TAlog = lm(log(pctTran) ~ lnA, data = logsub) #try with log10(pctTran), log(pctTran) ~ logA, and pctTran ~ logA since relationships wonky  
#   TA = lm(log(pctTran) ~ area, data = logsub)
#   TA.temp = data.frame(stateroute = s, 
#                        TAexp = TAlog$coefficients[2],
#                        TApow = TA$coefficients[2], 
#                        TAexp.r2 = summary(TAlog)$r.squared, 
#                        TApow.r2 = summary(TA)$r.squared) 
#   TA.df = rbind(TA.df, TA.temp)
#   
#   #TN  
#   TNlog = lm(log(pctTran) ~ lnN, data = logsub)
#   TN = lm(log(pctTran) ~ area, data = logsub)
#   TN.temp = data.frame(stateroute = s, 
#                        TNexp = TNlog$coefficients[2],
#                        TNpow = TN$coefficients[2], 
#                        TNexp.r2 = summary(TNlog)$r.squared, 
#                        TNpow.r2 = summary(TN)$r.squared)
#   TN.df = rbind(TN.df, TN.temp)
  }

#join all together using inner_join by focal rte, not cbind 
coefs = OA.df %>% 
  inner_join(ON.df, OA.df, by = "stateroute") %>% 
  inner_join(CA.df, OA.df, by = "stateroute") %>% 
  inner_join(CN.df, OA.df, by = "stateroute") 
  
coefs_2 = na.omit(coefs)
  
  
  # inner_join(TA.df, OA.df, by = "stateroute") %>% 
  # inner_join(TN.df, OA.df, by = "stateroute")  

write.csv(coefs, "C:/git/core-transient/scripts/R-scripts/scale_analysis/coefs.csv", row.names = FALSE) #updated 06/19
#exp mods have much better r2 vals for pctTran than power 

####Plotting occupancy-scale relationships with observed and predicted values####
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
bbs_allscales$logA = log10(bbs_allscales$area)
bbs_allscales$logN = log10(bbs_allscales$aveN)
bbs_allscales$lnA = log(bbs_allscales$area) #log is the natural log 
bbs_allscales$lnN = log(bbs_allscales$aveN) #rerun plots with this?
####filter out stateroutes that are one-sided in scale####
#in terms of their representation of below vs above scale (should have both, not one alone)
bbs_allscales2 = bbs_allscales %>% count(focalrte) %>% filter(n == 83) %>% data.frame() 
bbs_allscales3 = filter(bbs_allscales, focalrte %in% bbs_allscales2$focalrte)


coefs = read.csv("scripts/R-scripts/scale_analysis/coefs.csv", header = TRUE)


#function for extracting predicted values from models built with observed data
logistic_fcn = function(x, Asym, xmid, scal) {
  out = Asym/(1 + exp((xmid - x)/scal))
  return(out)
}

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
