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
BBS = '//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/'



####Extract coefficients from scale-occupancy relationships for analysis####
OA.df = data.frame(stateroute = numeric(), OA.min = numeric(), OA.max = numeric(), OA.slope = numeric(), 
                   OA.xmid = numeric(), OA.thresh = numeric(), 
                   OA.pmin = numeric(), OA.pmax = numeric(), OA.pslope = numeric(), 
                   OA.pxmid = numeric(), OA.pthresh = numeric(), 
                   OA.r2 = numeric(), OA.curvy = numeric()) 

ON.df = data.frame(stateroute = numeric(), ON.min = numeric(), ON.max = numeric(), ON.slope = numeric(), 
                   ON.xmid = numeric(), ON.thresh = numeric(), 
                   ON.pmin = numeric(), ON.pmax = numeric(), ON.pslope = numeric(), 
                   ON.pxmid = numeric(), ON.pthresh = numeric(), 
                   ON.r2 = numeric(), ON.curvy = numeric())

CA.df = data.frame(stateroute = numeric(), CA.min = numeric(), CA.max = numeric(), CA.slope = numeric(), 
                   CA.xmid = numeric(), CA.thresh = numeric(), 
                   CA.pmin = numeric(), CA.pmax = numeric(), CA.pslope = numeric(), 
                   CA.pxmid = numeric(), CA.pthresh = numeric(), 
                   CA.r2 = numeric(), CA.curvy = numeric())

CN.df = data.frame(stateroute = numeric(), CN.min = numeric(), CN.max = numeric(), CN.slope = numeric(), 
                   CN.xmid = numeric(), CN.thresh = numeric(), 
                   CN.pmin = numeric(), CN.pmax = numeric(), CN.pslope = numeric(), 
                   CN.pxmid = numeric(), CN.pthresh = numeric(), 
                   CN.r2 = numeric(), CN.curvy = numeric())

TA.df = data.frame(stateroute = numeric(), TA.min = numeric(), TA.max = numeric(), TA.slope = numeric(), 
                   TA.xmid = numeric(), TA.thresh = numeric(), 
                   TA.pmin = numeric(), TA.pmax = numeric(), TA.pslope = numeric(), 
                   TA.pxmid = numeric(), TA.pthresh = numeric(), 
                   TA.r2 = numeric(), TA.curvy = numeric())

TN.df = data.frame(stateroute = numeric(), TN.min = numeric(), TN.max = numeric(), TN.slope = numeric(), 
                   TN.xmid = numeric(), TN.thresh = numeric(), 
                   TN.pmin = numeric(), TN.pmax = numeric(), TN.pslope = numeric(), 
                   TN.pxmid = numeric(), TN.pthresh = numeric(), 
                   TN.r2 = numeric(), TN.curvy = numeric())

warnings = data.frame(stateroute = numeric(), warning = character())


#read in data for processing
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
stateroutes = unique(bbs_allscales$focalrte) #this stuff is the same, looks normal ^
unique(bbs_allscales$scale)
levels(bbs_allscales$scale) #checking before running loop that all scales rep'd


#make sure scale is leveled in order: 
bbs_allscales$scale = factor(bbs_allscales$scale, 
                              levels = c('5-1', '5-2', '5-3', '5-4', '5-5', '5-6', '5-7', '5-8', '5-9', '5-10',
                                         '10-1', '10-2', '10-3', '10-4', '10-5', '25-1', '25-2', '50-1', 
                                         '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12',
                                         '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24',
                                         '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36',
                                         '37', '38', '39', '40', '41', '42', '43', '44', '45', '46', '47', '48',
                                         '49', '50', '51', '52', '53', '54', '55', '56', '57', '58', '59', '60',
                                         '61', '62', '63', '64', '65', '66'), ordered=TRUE)


levels(bbs_allscales$scale)
unique(bbs_allscales$scale)
#ALL clear

#07/16 version of tryCatch
for(s in stateroutes){
  logsub = subset(bbs_allscales, bbs_allscales$focalrte == s)  
  #fitting the log curve for area (for each route)
  
  #OA 
  OAmodel = tryCatch({
    OAlog = lm(meanOcc ~ logA, data = logsub) #lm instead of nls, reg linear model
    logsub$OApreds = predict(OAlog)
    #OApred_df = data.frame(preds = predict(OAlog), scale = logsub$scale, logA = logsub$logA)  #get preds -> is predicting unique per scale, all clear
    OAlm.r2 = lm(logsub$meanOcc ~ OApred_df$preds) #get r2 from model, so far this is just predmod tho 
    
    
    #ACTUAL stats (for plotting data pts): 
    OA.min = min(logsub$meanOcc[logsub$logA == min(logsub$logA)])
    OA.max = max(logsub$meanOcc[logsub$logA == max(logsub$logA)])
    OA.slope = ((OA.max - OA.min)/(max(logsub$logA[logsub$meanOcc == max(logsub$meanOcc)]) - min(logsub$logA[logsub$meanOcc == min(logsub$meanOcc)])))
    OA.xmid = logsub$meanOcc[logsub$scale == '3'] #@ scale == 3, for a given focal rte s, actual value
    OA.thresh = as.character(min(logsub$scale[logsub$meanOcc > 0.49 & logsub$meanOcc < 0.60])) 
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #then save as a character so associated levels data doesn't stay stuck on the single data point
    
    #PREDICTED stats (for fitting line): 
    OA.pmin =  min(logsub$OApreds[logsub$logA == min(logsub$logA)])
    OA.pmax = max(logsub$OApreds[logsub$logA == max(logsub$logA)])
    OA.pslope = ((OA.pmax - OA.pmin)/(max(logsub$logA[logsub$meanOcc == max(logsub$meanOcc)]) - min(logsub$logA[logsub$meanOcc == min(logsub$meanOcc)])))
    OA.pxmid = logsub$OApreds[logsub$scale == '3']
    OA.pthresh = as.character(min(logsub$scale[logsub$OApreds > 0.49 & logsub$OApreds < 0.60])) 
    
    OA.r2 = summary(OAlm.r2)$r.squared
    OA.curvy =  OA.xmid - OA.pxmid 

    data.frame(stateroute = s, OA.min, OA.max, OA.slope, 
                                          OA.xmid, OA.thresh, 
                                          OA.pmin, OA.pmax, OA.pslope, 
                                          OA.pxmid, OA.pthresh, 
                                          OA.r2, OA.curvy)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    OA.min = NA
    OA.max = NA
    OA.slope = NA 
    OA.xmid = NA
    OA.thresh = NA  
    OA.pmin = NA
    OA.pmax = NA
    OA.pslope = NA 
    OA.pxmid = NA
    OA.pthresh = NA 
    OA.r2 = NA
    OA.curvy = NA
  
    
    temp = data.frame(stateroute = s, OA.min, OA.max, OA.slope, 
                      OA.xmid, OA.thresh, 
                      OA.pmin, OA.pmax, OA.pslope, 
                      OA.pxmid, OA.pthresh, 
                      OA.r2, OA.curvy)
    return(temp)
    
  })
  OA.df = rbind(OA.df, OAmodel)
  #OA model works now on one run, trycatch works, does loop work is question? 
  
  
  
  #ON 
  ONmodel = tryCatch({
    ONlog = lm(meanOcc ~ logN, data = logsub) #lm instead of nls, reg linear model
    logsub$ONpreds = predict(ONlog)
    #ONpred_df = data.frame(preds = predict(ONlog), scale = logsub$scale, logA = logsub$logA)  #get preds -> is predicting unique per scale, all clear
    ONlm.r2 = lm(logsub$meanOcc ~ ONpred_df$preds) #get r2 from model, so far this is just predmod tho 
    
    
    #ACTUAL stats (for plotting data pts): 
    ON.min = min(logsub$meanOcc[logsub$logN == min(logsub$logN)])
    ON.max = max(logsub$meanOcc[logsub$logN == max(logsub$logN)])
    ON.slope = ((ON.max - ON.min)/(max(logsub$logN[logsub$meanOcc == max(logsub$meanOcc)]) - min(logsub$logN[logsub$meanOcc == min(logsub$meanOcc)])))
    ON.xmid = logsub$meanOcc[logsub$scale == '3'] #@ scale == 3, for a given focal rte s, actual value
    ON.thresh = as.character(min(logsub$scale[logsub$meanOcc > 0.49 & logsub$meanOcc < 0.60])) 
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #then save as a character so associated levels data doesn't stay stuck on the single data point
    
    #PREDICTED stats (for fitting line): 
    ON.pmin =  min(logsub$ONpreds[logsub$logN == min(logsub$logN)])
    ON.pmax = max(logsub$ONpreds[logsub$logN == max(logsub$logN)])
    ON.pslope = ((ON.pmax - ON.pmin)/(max(logsub$logN[logsub$meanOcc == max(logsub$meanOcc)]) - min(logsub$logN[logsub$meanOcc == min(logsub$meanOcc)])))
    ON.pxmid = logsub$ONpreds[logsub$scale == '3']
    ON.pthresh = as.character(min(logsub$scale[logsub$ONpreds > 0.49 & logsub$ONpreds < 0.60])) 
    
    ON.r2 = summary(ONlm.r2)$r.squared
    ON.curvy =  ON.xmid - ON.pxmid 
    
    data.frame(stateroute = s, ON.min, ON.max, ON.slope, 
               ON.xmid, ON.thresh, 
               ON.pmin, ON.pmax, ON.pslope, 
               ON.pxmid, ON.pthresh, 
               ON.r2, ON.curvy)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    ON.min = NA
    ON.max = NA
    ON.slope = NA 
    ON.xmid = NA
    ON.thresh = NA  
    ON.pmin = NA
    ON.pmax = NA
    ON.pslope = NA 
    ON.pxmid = NA
    ON.pthresh = NA 
    ON.r2 = NA
    ON.curvy = NA
    
    
    temp = data.frame(stateroute = s, ON.min, ON.max, ON.slope, 
                      ON.xmid, ON.thresh, 
                      ON.pmin, ON.pmax, ON.pslope, 
                      ON.pxmid, ON.pthresh, 
                      ON.r2, ON.curvy)
    return(temp)
    
  })
  ON.df = rbind(ON.df, ONmodel)
  
  
  
  #CA
  CAmodel = tryCatch({
    CAlog = lm(pctCore ~ logA, data = logsub) #lm instead of nls, reg linear model
    logsub$CApreds = predict(CAlog)
    #CApred_df = data.frame(preds = predict(CAlog), scale = logsub$scale, logA = logsub$logA)  #get preds -> is predicting unique per scale, all clear
    CAlm.r2 = lm(logsub$pctCore ~ CApred_df$preds) #get r2 from model, so far this is just predmod tho 
    
    
    #ACTUAL stats (for plotting data pts): 
    CA.min = min(logsub$pctCore[logsub$logA == min(logsub$logA)])
    CA.max = max(logsub$pctCore[logsub$logA == max(logsub$logA)])
    CA.slope = ((CA.max - CA.min)/(max(logsub$logA[logsub$pctCore == max(logsub$pctCore)]) - min(logsub$logA[logsub$pctCore == min(logsub$pctCore)])))
    CA.xmid = logsub$pctCore[logsub$scale == '3'] #@ scale == 3, for a given focal rte s, actual value
    CA.thresh = as.character(min(logsub$scale[logsub$pctCore > 0.49 & logsub$pctCore < 0.60])) 
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #then save as a character so associated levels data doesn't stay stuck on the single data point
    
    #PREDICTED stats (for fitting line): 
    CA.pmin =  min(logsub$CApreds[logsub$logA == min(logsub$logA)])
    CA.pmax = max(logsub$CApreds[logsub$logA == max(logsub$logA)])
    CA.pslope = ((CA.pmax - CA.pmin)/(max(logsub$logA[logsub$pctCore == max(logsub$pctCore)]) - min(logsub$logA[logsub$pctCore == min(logsub$pctCore)])))
    CA.pxmid = logsub$CApreds[logsub$scale == '3']
    CA.pthresh = as.character(min(logsub$scale[logsub$CApreds > 0.49 & logsub$CApreds < 0.60])) 
    
    CA.r2 = summary(CAlm.r2)$r.squared
    CA.curvy =  CA.xmid - CA.pxmid 
    
    data.frame(stateroute = s, CA.min, CA.max, CA.slope, 
               CA.xmid, CA.thresh, 
               CA.pmin, CA.pmax, CA.pslope, 
               CA.pxmid, CA.pthresh, 
               CA.r2, CA.curvy)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    CA.min = NA
    CA.max = NA
    CA.slope = NA 
    CA.xmid = NA
    CA.thresh = NA  
    CA.pmin = NA
    CA.pmax = NA
    CA.pslope = NA 
    CA.pxmid = NA
    CA.pthresh = NA 
    CA.r2 = NA
    CA.curvy = NA
    
    
    temp = data.frame(stateroute = s, CA.min, CA.max, CA.slope, 
                      CA.xmid, CA.thresh, 
                      CA.pmin, CA.pmax, CA.pslope, 
                      CA.pxmid, CA.pthresh, 
                      CA.r2, CA.curvy)
    return(temp)
    
  })
  CA.df = rbind(CA.df, CAmodel)
  
  #CN 
  CNmodel = tryCatch({
    CNlog = lm(pctCore ~ logN, data = logsub) #lm instead of nls, reg linear model
    logsub$CNpreds = predict(CNlog)
    #CNpred_df = data.frame(preds = predict(CNlog), scale = logsub$scale, logN = logsub$logN)  #get preds -> is predicting unique per scale, all clear
    CNlm.r2 = lm(logsub$pctCore ~ CNpred_df$preds) #get r2 from model, so far this is just predmod tho 
    
    
    #ACTUAL stats (for plotting data pts): 
    CN.min = min(logsub$pctCore[logsub$logN == min(logsub$logN)])
    CN.max = max(logsub$pctCore[logsub$logN == max(logsub$logN)])
    CN.slope = ((CN.max - CN.min)/(max(logsub$logN[logsub$pctCore == max(logsub$pctCore)]) - min(logsub$logN[logsub$pctCore == min(logsub$pctCore)])))
    CN.xmid = logsub$pctCore[logsub$scale == '3'] #@ scale == 3, for a given focal rte s, actual value
    CN.thresh = as.character(min(logsub$scale[logsub$pctCore > 0.49 & logsub$pctCore < 0.60])) 
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #then save as a character so associated levels data doesn't stay stuck on the single data point
    
    #PREDICTED stats (for fitting line): 
    CN.pmin =  min(logsub$CNpreds[logsub$logN == min(logsub$logN)])
    CN.pmax = max(logsub$CNpreds[logsub$logN == max(logsub$logN)])
    CN.pslope = ((CN.pmax - CN.pmin)/(max(logsub$logN[logsub$pctCore == max(logsub$pctCore)]) - min(logsub$logN[logsub$pctCore == min(logsub$pctCore)])))
    CN.pxmid = logsub$CNpreds[logsub$scale == '3']
    CN.pthresh = as.character(min(logsub$scale[logsub$CNpreds > 0.49 & logsub$CNpreds < 0.60])) 
    
    CN.r2 = summary(CNlm.r2)$r.squared
    CN.curvy =  CN.xmid - CN.pxmid 
    
    data.frame(stateroute = s, CN.min, CN.max, CN.slope, 
               CN.xmid, CN.thresh, 
               CN.pmin, CN.pmax, CN.pslope, 
               CN.pxmid, CN.pthresh, 
               CN.r2, CN.curvy)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    CN.min = NA
    CN.max = NA
    CN.slope = NA 
    CN.xmid = NA
    CN.thresh = NA  
    CN.pmin = NA
    CN.pmax = NA
    CN.pslope = NA 
    CN.pxmid = NA
    CN.pthresh = NA 
    CN.r2 = NA
    CN.curvy = NA
    
    
    temp = data.frame(stateroute = s, CN.min, CN.max, CN.slope, 
                      CN.xmid, CN.thresh, 
                      CN.pmin, CN.pmax, CN.pslope, 
                      CN.pxmid, CN.pthresh, 
                      CN.r2, CN.curvy)
    return(temp)
    
  })
  CN.df = rbind(CN.df, CNmodel)
  


  # Fitting % transient
  #TA 
  TAmodel = tryCatch({
    TAlog = lm(pctTran ~ lnA, data = logsub) #lm instead of nls, reg linear model
    logsub$TApreds = predict(TAlog)
    #TApred_df = data.frame(preds = predict(TAlog), scale = logsub$scale, lnA = logsub$lnA)  #get preds -> is predicting unique per scale, all clear
    TAlm.r2 = lm(logsub$pctTran ~ TApred_df$preds) #get r2 from model, so far this is just predmod tho 
    
    
    #ACTUAL stats (for plotting data pts): 
    TA.min = min(logsub$pctTran[logsub$lnA == min(logsub$lnA)])
    TA.max = max(logsub$pctTran[logsub$lnA == max(logsub$lnA)])
    TA.slope = ((TA.max - TA.min)/(max(logsub$lnA[logsub$pctTran == max(logsub$pctTran)]) - min(logsub$lnA[logsub$pctTran == min(logsub$pctTran)])))
    TA.xmid = logsub$pctTran[logsub$scale == '3'] #@ scale == 3, for a given focal rte s, actual value
    TA.thresh = as.character(min(logsub$scale[logsub$pctTran > 0.49 & logsub$pctTran < 0.60])) 
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #then save as a character so associated levels data doesn't stay stuck on the single data point
    
    #PREDICTED stats (for fitting line): 
    TA.pmin =  min(logsub$TApreds[logsub$lnA == min(logsub$lnA)])
    TA.pmax = max(logsub$TApreds[logsub$lnA == max(logsub$lnA)])
    TA.pslope = ((TA.pmax - TA.pmin)/(max(logsub$lnA[logsub$pctTran == max(logsub$pctTran)]) - min(logsub$lnA[logsub$pctTran == min(logsub$pctTran)])))
    TA.pxmid = logsub$TApreds[logsub$scale == '3']
    TA.pthresh = as.character(min(logsub$scale[logsub$TApreds > 0.49 & logsub$TApreds < 0.60])) 
    
    TA.r2 = summary(TAlm.r2)$r.squared
    TA.curvy =  TA.xmid - TA.pxmid 
    
    data.frame(stateroute = s, TA.min, TA.max, TA.slope, 
               TA.xmid, TA.thresh, 
               TA.pmin, TA.pmax, TA.pslope, 
               TA.pxmid, TA.pthresh, 
               TA.r2, TA.curvy)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    TA.min = NA
    TA.max = NA
    TA.slope = NA 
    TA.xmid = NA
    TA.thresh = NA  
    TA.pmin = NA
    TA.pmax = NA
    TA.pslope = NA 
    TA.pxmid = NA
    TA.pthresh = NA 
    TA.r2 = NA
    TA.curvy = NA
    
    
    temp = data.frame(stateroute = s, TA.min, TA.max, TA.slope, 
                      TA.xmid, TA.thresh, 
                      TA.pmin, TA.pmax, TA.pslope, 
                      TA.pxmid, TA.pthresh, 
                      TA.r2, TA.curvy)
    return(temp)
  })

TA.df = rbind(TA.df, TAmodel)


#TN 
TNmodel = tryCatch({
  TNlog = lm(pctTran ~ lnN, data = logsub) #lm instead of nls, reg linear model
  logsub$TNpreds = predict(TNlog)
  #TNpred_df = data.frame(preds = predict(TNlog), scale = logsub$scale, lnN = logsub$lnN)  #get preds -> is predicting unique per scale, all clear
  TNlm.r2 = lm(logsub$pctTran ~ TNpred_df$preds) #get r2 from model, so far this is just predmod tho 
  
  
  #ACTUAL stats (for plotting data pts): 
  TN.min = min(logsub$pctTran[logsub$lnN == min(logsub$lnN)])
  TN.max = max(logsub$pctTran[logsub$lnN == max(logsub$lnN)])
  TN.slope = ((TN.max - TN.min)/(max(logsub$lnN[logsub$pctTran == max(logsub$pctTran)]) - min(logsub$lnN[logsub$pctTran == min(logsub$pctTran)])))
  TN.xmid = logsub$pctTran[logsub$scale == '3'] #@ scale == 3, for a given focal rte s, actual value
  TN.thresh = as.character(min(logsub$scale[logsub$pctTran > 0.49 & logsub$pctTran < 0.60])) 
  #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
  #then save as a character so associated levels data doesn't stay stuck on the single data point
  
  #PREDICTED stats (for fitting line): 
  TN.pmin =  min(logsub$TNpreds[logsub$lnN == min(logsub$lnN)])
  TN.pmax = max(logsub$TNpreds[logsub$lnN == max(logsub$lnN)])
  TN.pslope = ((TN.pmax - TN.pmin)/(max(logsub$lnN[logsub$pctTran == max(logsub$pctTran)]) - min(logsub$lnN[logsub$pctTran == min(logsub$pctTran)])))
  TN.pxmid = logsub$TNpreds[logsub$scale == '3']
  TN.pthresh = as.character(min(logsub$scale[logsub$TNpreds > 0.49 & logsub$TNpreds < 0.60])) 
  
  TN.r2 = summary(TNlm.r2)$r.squared
  TN.curvy =  TN.xmid - TN.pxmid 
  
  data.frame(stateroute = s, TN.min, TN.max, TN.slope, 
             TN.xmid, TN.thresh, 
             TN.pmin, TN.pmax, TN.pslope, 
             TN.pxmid, TN.pthresh, 
             TN.r2, TN.curvy)
  
}, warning = function(w) {
  warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
}, error = function(e) {
  TN.min = NA
  TN.max = NA
  TN.slope = NA 
  TN.xmid = NA
  TN.thresh = NA  
  TN.pmin = NA
  TN.pmax = NA
  TN.pslope = NA 
  TN.pxmid = NA
  TN.pthresh = NA 
  TN.r2 = NA
  TN.curvy = NA
  
  
  temp = data.frame(stateroute = s, TN.min, TN.max, TN.slope, 
                    TN.xmid, TN.thresh, 
                    TN.pmin, TN.pmax, TN.pslope, 
                    TN.pxmid, TN.pthresh, 
                    TN.r2, TN.curvy)
  return(temp)
})

TN.df = rbind(TN.df, TNmodel)
  
  
} #end of loop 
  
  
#join all together using inner_join by focal rte, not cbind 
coefs = OA.df %>% 
  inner_join(ON.df, OA.df, by = "stateroute") %>% 
  inner_join(CA.df, OA.df, by = "stateroute") %>% 
  inner_join(CN.df, OA.df, by = "stateroute") %>%
  inner_join(TA.df, OA.df, by = "stateroute") %>%
  inner_join(TN.df, OA.df, by = "stateroute")

  
coefs_2 = na.omit(coefs)
  
  
write.csv(coefs_2, "scripts/R-scripts/scale_analysis/coefs.csv", row.names = FALSE) #updated 07/17
#exp mods have much better r2 vals for pctTran than power 

####Plotting occupancy-scale relationships with observed and predicted values####
#work in progress
#do I want to plot slope and line of predicted values over the top of actual? should be an easy sub 

bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
bbs_allscales$logA = log10(bbs_allscales$area)
bbs_allscales$logN = log10(bbs_allscales$aveN)
bbs_allscales$lnA = log(bbs_allscales$area) #log is the natural log 
bbs_allscales$lnN = log(bbs_allscales$aveN) #rerun plots with this?
coefs = read.csv("scripts/R-scripts/scale_analysis/coefs.csv", header = TRUE)


#will go inside plotting forloop 
coef_join = coefs %>% inner_join(bbs_allscales, by = c("stateroute"="focalrte"))
#stateroutes = unique(bbs_allscales$focalrte)
s = 2001
coef_sub = subset(coef_join, coef_join$stateroute == s)


theme_set(theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
#OA
OAlog = lm(meanOcc ~ logA, data = coef_sub) #lm instead of nls, reg linear model
OApreds = data.frame(preds = predict(OAlog), scale = coef_sub$scale, logA = coef_sub$logA) 


plot1 = ggplot(coef_sub, aes(x = logA, y = meanOcc))+geom_point(colour = "firebrick")+
  geom_abline(yintercept = , slope = OA.slope) +labs(x = "Log area", y = "Mean % Occupancy")+
  coord_cartesian(ylim = c(0, 1))

#ref from abline help: 

# Calculate slope and intercept of line of best fit
coef(lm(mpg ~ wt, data = mtcars))
p + geom_abline(intercept = 37, slope = -5)
# But this is easier to do with geom_smooth:
p + geom_smooth(method = "lm", se = FALSE)
