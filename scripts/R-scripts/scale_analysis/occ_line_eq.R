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



####Extract coefficients from scale-occupancy relationships for analysis####
OA.df = data.frame(stateroute = numeric(), OA.alt_xmid_pred = numeric(), OA.alt_xmid_dev= numeric(), 
                   OA.mid_occ = numeric(), OA.slope = numeric(), OA.max= numeric(), OA.min= numeric(), OA.r2= numeric())
ON.df = data.frame(stateroute = numeric(), ON.alt_xmid_pred = numeric(), ON.alt_xmid_dev= numeric(), 
                   ON.mid_occ = numeric(), ON.slope = numeric(), ON.max= numeric(), ON.min= numeric(), ON.r2= numeric())
CA.df = data.frame(stateroute = numeric(), CA.alt_xmid_pred = numeric(), CA.alt_xmid_dev= numeric(), 
                   CA.mid_occ = numeric(), CA.slope = numeric(), CA.max= numeric(), CA.min= numeric(), CA.r2= numeric())
CN.df = data.frame(stateroute = numeric(), CN.alt_xmid_pred = numeric(), CN.alt_xmid_dev = numeric(), 
                   CN.mid_occ = numeric(), CN.slope = numeric(), CN.max = numeric(), CN.min = numeric(), CN.r2 = numeric())
TA.df = data.frame(stateroute = numeric(), TA.alt_xmid_pred = numeric(), TA.alt_xmid_dev= numeric(), 
                   TA.mid_occ = numeric(), TA.slope = numeric(), TA.max= numeric(), TA.min= numeric(), TA.r2= numeric())
TN.df = data.frame(stateroute = numeric(), TN.alt_xmid_pred = numeric(), TN.alt_xmid_dev = numeric(), 
                   TN.mid_occ = numeric(), TN.slope = numeric(), TN.max = numeric(), TN.min = numeric(), TN.r2 = numeric())

warnings = data.frame(stateroute = numeric(), warning = character())


#read in data for processing
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
stateroutes = unique(bbs_allscales$focalrte) #this stuff is the same, looks normal ^

#make sure scale is leveled in order: 
bbs_allscales$scale = factor(bbs_allscales$scale, 
                             levels = c('5-1', '5-2', '5-3', '5-4', '5-5', '5-6', '5-7', '5-8', '5-9', '5-10',
                                        '10-1', '10-2', '10-3', '10-4', '10-5', '25-1', '25-2', '50-1',
                                        '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12',
                                        '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24',
                                        '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36',
                                        '37', '38', '39', '40', '41', '42', '43', '44', '45', '46', '47', '48',
                                        '49', '50', '51', '52', '53', '54', '55', '56', '57', '58', '59', '60',
                                        '61', '62', '63', '64', '65'), ordered=TRUE)


levels(bbs_allscales$scale)
#NEED to do or it won't be in order

#07/12 version of tryCatch
for(s in stateroutes){
  logsub = subset(bbs_allscales, bbs_allscales$focalrte == s)  
  #fitting the log curve for area (for each route)
  
  #OA 
  OAmodel = tryCatch({
    OAlog = lm(meanOcc ~ logA, data = logsub) #lm instead of nls, reg linear model
    OApred_df = data.frame(preds = predict(OAlog), scale = logsub$scale)  #get preds -> is predicting unique per scale, all clear
    OAlm.r2 = lm(logsub$meanOcc ~ OApred_df$preds) #get r2 from model 
    
    
    OA.alt_xmid = logsub$meanOcc[logsub$scale == 3] #@ scale == 3, for a given focal rte s, actual value
    #logsub[21,3] achieves same thing
    OA.alt_xmid_pred = OApred_df$preds[OApred_df$scale == 3]
    OA.alt_xmid_dev = (OA.alt_xmid - OA.alt_xmid_pred)^2 #squared deviance of pred from actual val #need pred AT SCALE = 3 THO
    OA.mid_occ = as.character(min(logsub$scale[logsub$meanOcc > 0.49 & logsub$meanOcc < 0.60])) 
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #then save as a character so associated levels data doesn't stay stuck on the single data point
    
    
    #eq of a line 
    #((y2-y1)/(x2-x1)) 
    #meanOcc vals are y, logA is the x 
    #x and y are dictated by the original model 
    OA.slope = ((max(logsub$logA) - min(logsub$logA))/(max(logsub$logA) - min(logsub$logA)))
    #max in BOTH dimensions, x and y
    OA.max = max(logsub$meanOcc[logsub$logA == max(logsub$logA)]) #what point is at the beginning of the line, for a given focal rte s?
    OA.min = min(logsub$meanOcc[logsub$logA == min(logsub$logA)])
    
    OA.r2 <- summary(OAlm.r2)$r.squared
    data.frame(stateroute = s, OA.alt_xmid_pred, OA.alt_xmid_dev, OA.mid_occ, OA.slope, OA.max, OA.min, OA.r2)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    OA.alt_xmid_pred <- NA
    OA.alt_xmid_dev <- NA 
    OA.mid_occ <- NA
    OA.slope <- NA
    OA.max <- NA
    OA.min <- NA
    OA.r2 <- NA
    
    
    temp = data.frame(stateroute = s, OA.alt_xmid_pred, OA.alt_xmid_dev, OA.mid_occ, OA.slope, OA.max, OA.min, OA.r2)
    return(temp)
    
  })
  OA.df = rbind(OA.df, OAmodel)
  
  #ON 
  ONmodel = tryCatch({
    ONlog = lm(meanOcc ~ logN, data = logsub) #lm instead of nls, reg linear model
    ONpred_df = data.frame(preds = predict(ONlog), scale = logsub$scale)  #get preds -> is predicting unique per scale, all clear
    ONlm.r2 = lm(logsub$meanOcc ~ ONpred_df$preds) #get r2 from model 
    
    
    ON.alt_xmid = logsub$meanOcc[logsub$scale == 3] #@ scale == 3, for a given focal rte s, actual value
    #logsub[21,3] achieves same thing
    ON.alt_xmid_pred = ONpred_df$preds[ONpred_df$scale == 3]
    ON.alt_xmid_dev = (ON.alt_xmid - ON.alt_xmid_pred)^2 #squared deviance of pred from actual val #need pred AT SCALE = 3 THO
    ON.mid_occ = as.character(min(logsub$scale[logsub$meanOcc > 0.49 & logsub$meanOcc < 0.60])) 
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #then save as a character so associated levels data doesn't stay stuck on the single data point
    
    
    #eq of a line 
    #((y2-y1)/(x2-x1)) 
    #meanOcc vals are y, logN is the x 
    #x and y are dictated by the original model 
    ON.slope = ((max(logsub$meanOcc) - min(logsub$meanOcc))/(max(logsub$logN) - min(logsub$logN)))
    #max in BOTH dimensions, x and y
    ON.max = max(logsub$meanOcc[max(logsub$logN)]) #what point is at the "end of the line", for a given focal rte s? 
    ON.min = min(logsub$meanOcc[min(logsub$logN)]) #what point is at the beginning of the line, for a given focal rte s?
    
    
    ON.r2 <- summary(ONlm.r2)$r.squared
    data.frame(stateroute = s, ON.alt_xmid_pred, ON.alt_xmid_dev, ON.mid_occ, ON.slope, ON.max, ON.min, ON.r2)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    ON.alt_xmid_pred = NA
    ON.alt_xmid_dev = NA 
    ON.mid_occ = NA
    ON.slope = NA
    ON.max = NA
    ON.min = NA
    ON.r2 = NA
    temp = data.frame(stateroute = s, ON.alt_xmid_pred, ON.alt_xmid_dev, ON.mid_occ, ON.slope, ON.max, ON.min, ON.r2)
    return(temp)
    
  })
  ON.df = rbind(ON.df, ONmodel)
  
  
  
  #CA
  CAmodel = tryCatch({
    CAlog = lm(pctCore ~ logA, data = logsub) #lm instead of nls, reg linear model
    CApred_df = data.frame(preds = predict(CAlog), scale = logsub$scale)  #get preds -> is predicting unique per scale, all clear
    CAlm.r2 = lm(logsub$pctCore ~ CApred_df$preds) #get r2 from model 
    
    
    CA.alt_xmid = logsub$pctCore[logsub$scale == 3] #@ scale == 3, for a given focal rte s, actual value
    #logsub[21,3] achieves same thing
    CA.alt_xmid_pred = CApred_df$preds[CApred_df$scale == 3]
    CA.alt_xmid_dev = (CA.alt_xmid - CA.alt_xmid_pred)^2 #squared deviance of pred from actual val #need pred AT SCALE = 3 THO
    CA.mid_occ = as.character(min(logsub$scale[logsub$meanOcc > 0.49 & logsub$meanOcc < 0.60])) 
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #then save as a character so associated levels data doesn't stay stuck on the single data point
    
    
    #eq of a line 
    #((y2-y1)/(x2-x1)) 
    #pctCore vals are y, logA is the x 
    #x and y are dictated by the original model 
    CA.slope = ((max(logsub$pctCore) - min(logsub$pctCore))/(max(logsub$logA) - min(logsub$logA)))
    #max in BOTH dimensions, x and y
    CA.max = max(logsub$pctCore[max(logsub$logA)]) #what point is at the "end of the line", for a given focal rte s? 
    CA.min = min(logsub$pctCore[min(logsub$logA)]) #what point is at the beginning of the line, for a given focal rte s?
    
    
    CA.r2 <- summary(CAlm.r2)$r.squared
    data.frame(stateroute = s, CA.alt_xmid_pred, CA.alt_xmid_dev, CA.mid_occ, CA.slope, CA.max, CA.min, CA.r2)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    CA.alt_xmid_pred = NA
    CA.alt_xmid_dev = NA 
    CA.mid_occ = NA
    CA.slope = NA
    CA.max = NA
    CA.min = NA
    CA.r2 = NA
    temp = data.frame(stateroute = s, CA.alt_xmid_pred, CA.alt_xmid_dev, CA.mid_occ, CA.slope, CA.max, CA.min, CA.r2)
    return(temp)
    
  })
  CA.df = rbind(CA.df, CAmodel)
  
  #CN 
  CNmodel = tryCatch({
    CNlog = lm(pctCore ~ logN, data = logsub) #lm instead of nls, reg linear model
    CNpred_df = data.frame(preds = predict(CNlog), scale = logsub$scale)  #get preds -> is predicting unique per scale, all clear
    CNlm.r2 = lm(logsub$pctCore ~ CNpred_df$preds) #get r2 from model 
    
    
    CN.alt_xmid = logsub$pctCore[logsub$scale == 3] #@ scale == 3, for a given focal rte s, actual value
    #logsub[21,3] achieves same thing
    CN.alt_xmid_pred = CNpred_df$preds[CNpred_df$scale == 3]
    CN.alt_xmid_dev = (CN.alt_xmid - CN.alt_xmid_pred)^2 #squared deviance of pred from actual val #need pred AT SCALE = 3 THO
    CN.mid_occ = as.character(min(logsub$scale[logsub$meanOcc > 0.49 & logsub$meanOcc < 0.60])) 
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #then save as a character so associated levels data doesn't stay stuck on the single data point
    
    
    #eq of a line 
    #((y2-y1)/(x2-x1)) 
    #pctCore vals are y, logN is the x 
    #x and y are dictated by the original model 
    CN.slope = ((max(logsub$pctCore) - min(logsub$pctCore))/(max(logsub$logN) - min(logsub$logN)))
    #max in BOTH dimensions, x and y
    CN.max = max(logsub$pctCore[max(logsub$logN)]) #what point is at the "end of the line", for a given focal rte s? 
    CN.min = min(logsub$pctCore[min(logsub$logN)]) #what point is at the beginning of the line, for a given focal rte s?
    
    
    CN.r2 <- summary(CNlm.r2)$r.squared
    data.frame(stateroute = s, CN.alt_xmid_pred, CN.alt_xmid_dev, CN.mid_occ, CN.slope, CN.max, CN.min, CN.r2)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    CN.alt_xmid_pred = NA
    CN.alt_xmid_dev = NA 
    CN.mid_occ = NA
    CN.slope = NA
    CN.max = NA
    CN.min = NA
    CN.r2 = NA
    temp = data.frame(stateroute = s, CN.alt_xmid_pred, CN.alt_xmid_dev, CN.mid_occ, CN.slope, CN.max, CN.min, CN.r2)
    return(temp)
    
  })
  CN.df = rbind(CN.df, CNmodel)
  


  # Fitting % transient
TAmodel = tryCatch({
  TAlog = lm(log(pctTran) ~ lnA, data = logsub) #try with log10(pctTran), log(pctTran) ~ logA, and pctTran ~ logA since relationships wonky
  TA = lm(log(pctTran) ~ area, data = logsub)
  TApred_df = data.frame(preds = predict(TAlog), scale = logsub$scale)  #get preds -> is predicting unique per scale, all clear
  TAlm.r2 = lm(logsub$pctTran ~ TApred_df$preds) #get r2 from model


  TA.alt_xmid = logsub$pctTran[logsub$scale == 3] #@ scale == 3, for a given focal rte s, actual value
  #logsub[21,3] achieves same thing
  TA.alt_xmid_pred = TApred_df$preds[TApred_df$scale == 3]
  TA.alt_xmid_dev = (TA.alt_xmid - TA.alt_xmid_pred)^2 #squared deviance of pred from actual val #need pred AT SCALE = 3 THO
  TA.mid_occ = as.character(min(logsub$scale[logsub$meanOcc > 0.49 & logsub$meanOcc < 0.60]))
 #for trans and % core, adapt or same? meanocc overall?
  #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
  #then save as a character so associated levels data doesn't stay stuck on the single data point


  #eq of a line
  #((y2-y1)/(x2-x1))
  #pctTran vals are y, lnA is the x
  #x and y are dictated by the original model
  TA.slope = ((max(logsub$pctTran) - min(logsub$pctTran))/(max(logsub$lnA) - min(logsub$lnA)))
  #max in BOTH dimensions, x and y
  TA.max = max(logsub$pctTran[max(logsub$lnA)]) #what point is at the "end of the line", for a given focal rte s?
  TA.min = min(logsub$pctTran[min(logsub$lnA)]) #what point is at the beginning of the line, for a given focal rte s?


  TA.r2 <- summary(TAlm.r2)$r.squared
  data.frame(stateroute = s, TA.alt_xmid_pred, TA.alt_xmid_dev, TA.mid_occ, TA.slope, TA.max, TA.min, TA.r2)

}, warning = function(w) {
  warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
}, error = function(e) {
  TA.alt_xmid_pred = NA
  TA.alt_xmid_dev = NA
  TA.mid_occ = NA
  TA.slope = NA
  TA.max = NA
  TA.min = NA
  TA.r2 = NA
  temp = data.frame(stateroute = s, TA.alt_xmid_pred, TA.alt_xmid_dev, TA.mid_occ, TA.slope, TA.max, TA.min, TA.r2)
  return(temp)
  })
  
TA.df = rbind(TA.df, TAmodel)

TNmodel = tryCatch({
    TNlog = lm(log(pctTran) ~ lnN, data = logsub) #try with log10(pctTran), log(pctTran) ~ logA, and pctTran ~ logA since relationships wonky
    TN = lm(log(pctTran) ~ aveN, data = logsub)
    TNpred_df = data.frame(preds = predict(TNlog), scale = logsub$scale)  #get preds -> is predicting unique per scale, all clear
    TNlm.r2 = lm(logsub$pctTran ~ TNpred_df$preds) #get r2 from model


    TN.alt_xmid = logsub$pctTran[logsub$scale == 3] #@ scale == 3, for a given focal rte s, actual value
    #logsub[21,3] achieves same thing
    TN.alt_xmid_pred = TNpred_df$preds[TNpred_df$scale == 3]
    TN.alt_xmid_dev = (TN.alt_xmid - TN.alt_xmid_pred)^2 #squared deviance of pred from actual val #need pred AT SCALE = 3 THO
    TN.mid_occ = as.character(min(logsub$scale[logsub$meanOcc > 0.49 & logsub$meanOcc < 0.60]))
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #then save as a character so associated levels data doesn't stay stuck on the single data point


    #eq of a line
    #((y2-y1)/(x2-x1))
    #pctTran vals are y, lnN is the x
    #x and y are dictated by the original model
    TN.slope = ((max(logsub$pctTran) - min(logsub$pctTran))/(max(logsub$lnN) - min(logsub$lnN)))
    #max in BOTH dimensions, x and y
    TN.max = max(logsub$pctTran[max(logsub$lnN)]) #what point is at the "end of the line", for a given focal rte s?
    TN.min = min(logsub$pctTran[min(logsub$lnN)]) #what point is at the beginning of the line, for a given focal rte s?


    TN.r2 <- summary(TNlm.r2)$r.squared
    data.frame(stateroute = s, TN.alt_xmid_pred, TN.alt_xmid_dev, TN.mid_occ, TN.slope, TN.max, TN.min, TN.r2)

  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    TN.alt_xmid_pred = NA
    TN.alt_xmid_dev = NA
    TN.mid_occ = NA
    TN.slope = NA
    TN.max = NA
    TN.min = NA
    TN.r2 = NA
    temp = data.frame(stateroute = s, TN.alt_xmid_pred, TN.alt_xmid_dev, TN.mid_occ, TN.slope, TN.max, TN.min, TN.r2)
    return(temp)
    })
  
TN.df = rbind(TN.df, TNmodel)
}  
  
  
#join all together using inner_join by focal rte, not cbind 
coefs = OA.df %>% 
  inner_join(ON.df, OA.df, by = "stateroute") %>% 
  inner_join(CA.df, OA.df, by = "stateroute") %>% 
  inner_join(CN.df, OA.df, by = "stateroute") %>% 
  inner_join(TA.df, OA.df, by = "stateroute") %>% 
  inner_join(TN.df, OA.df, by = "stateroute")  

  
coefs_2 = na.omit(coefs)
  
  
write.csv(coefs_2, "scripts/R-scripts/scale_analysis/coefs.csv", row.names = FALSE) #updated 07/12
#exp mods have much better r2 vals for pctTran than power 

####Plotting occupancy-scale relationships with observed and predicted values####
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
bbs_allscales$logA = log10(bbs_allscales$area)
bbs_allscales$logN = log10(bbs_allscales$aveN)
bbs_allscales$lnA = log(bbs_allscales$area) #log is the natural log 
bbs_allscales$lnN = log(bbs_allscales$aveN) #rerun plots with this?

coefs = read.csv("scripts/R-scripts/scale_analysis/coefs.csv", header = TRUE)


preds.df = data.frame(stateroute = numeric(), logA = numeric(), 
                      OApreds= numeric(), ONpreds = numeric(), 
                      CApreds = numeric(), CNpreds = numeric(),
                      TApreds = numeric(), TNpreds = numeric())


pdf("output/plots/Molly Plots/BBS_scaleplots.pdf", onefile = TRUE)
coef_join = coefs %>% inner_join(bbs_allscales, by = c("stateroute"="focalrte"))
stateroutes = unique(bbs_allscales$focalrte)

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

