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
                   OA.pxmid = numeric(), OA.thresh = numeric(), 
                   OA.r2 = numeric(), OA.curvy = numeric()) 

ON.df = data.frame(stateroute = numeric(), ON.min = numeric(), ON.max = numeric(), ON.slope = numeric(), 
                   ON.xmid = numeric(), ON.thresh = numeric(), 
                   ON.pmin = numeric(), ON.pmax = numeric(), ON.pslope = numeric(), 
                   ON.pxmid = numeric(), ON.thresh = numeric(), 
                   ON.r2 = numeric(), ON.curvy = numeric())

CA.df = data.frame(stateroute = numeric(), CA.min = numeric(), CA.max = numeric(), CA.slope = numeric(), 
                   CA.xmid = numeric(), CA.thresh = numeric(), 
                   CA.pmin = numeric(), CA.pmax = numeric(), CA.pslope = numeric(), 
                   CA.pxmid = numeric(), CA.thresh = numeric(), 
                   CA.r2 = numeric(), CA.curvy = numeric())

CN.df = data.frame(stateroute = numeric(), CN.min = numeric(), CN.max = numeric(), CN.slope = numeric(), 
                   CN.xmid = numeric(), CN.thresh = numeric(), 
                   CN.pmin = numeric(), CN.pmax = numeric(), CN.pslope = numeric(), 
                   CN.pxmid = numeric(), CN.thresh = numeric(), 
                   CN.r2 = numeric(), CN.curvy = numeric())

TA.df = data.frame(stateroute = numeric(), TA.min = numeric(), TA.max = numeric(), TA.slope = numeric(), 
                   TA.xmid = numeric(), TA.thresh = numeric(), 
                   TA.pmin = numeric(), TA.pmax = numeric(), TA.pslope = numeric(), 
                   TA.pxmid = numeric(), TA.thresh = numeric(), 
                   TA.r2 = numeric(), TA.curvy = numeric())

TN.df = data.frame(stateroute = numeric(), TN.min = numeric(), TN.max = numeric(), TN.slope = numeric(), 
                   TN.xmid = numeric(), TN.thresh = numeric(), 
                   TN.pmin = numeric(), TN.pmax = numeric(), TN.pslope = numeric(), 
                   TN.pxmid = numeric(), TN.thresh = numeric(), 
                   TN.r2 = numeric(), TN.curvy = numeric())

warnings = data.frame(stateroute = numeric(), warning = character())


#read in data for processing
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
stateroutes = unique(bbs_allscales$focalrte) #this stuff is the same, looks normal ^

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
#NEED to do or it won't be in order

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

    data.frame(stateroute = s, OA.alt_xmid, OA.alt_xmid_pred, OA.alt_xmid_dev, OA.mid_occ, 
               OA.slope, OA.max, OA.min, OA.r2)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    OA.alt_xmid <- NA
    OA.alt_xmid_pred <- NA
    OA.alt_xmid_dev <- NA 
    OA.mid_occ <- NA
    OA.slope <- NA
    OA.max <- NA
    OA.min <- NA
    OA.r2 <- NA
  
    
    temp = data.frame(stateroute = s, OA.alt_xmid, OA.alt_xmid_pred, OA.alt_xmid_dev, OA.mid_occ, 
                      OA.slope, OA.max, OA.min, OA.r2)
    return(temp)
    
  })
  OA.df = rbind(OA.df, OAmodel)
  #OA model works now on one run, trycatch works, does loop work is question? 
  
  
  
  #ON 
  ONmodel = tryCatch({
    ONlog = lm(meanOcc ~ logN, data = logsub) #lm instead of nls, reg linear model
    ONpred_df = data.frame(preds = predict(ONlog), scale = logsub$scale)  #get preds -> is predicting unique per scale, all clear
    ONlm.r2 = lm(logsub$meanOcc ~ ONpred_df$preds) #get r2 from model 
    
    
    ON.alt_xmid = logsub$meanOcc[logsub$scale == '3'] #@ scale == 3, for a given focal rte s, actual value
    #logsub[21,3] achieves same thing
    ON.alt_xmid_pred = ONpred_df$preds[ONpred_df$scale == '3']
    ON.alt_xmid_dev = (ON.alt_xmid - ON.alt_xmid_pred) #squared deviance of pred from actual val #need pred AT SCALE = 3 THO
    ON.mid_occ = as.character(min(logsub$scale[logsub$meanOcc > 0.49 & logsub$meanOcc < 0.60])) 
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #then save as a character so associated levels data doesn't stay stuck on the single data point
    
    
    #eq of a line 
    #((y2-y1)/(x2-x1)) 
    #meanOcc vals are y, logN is the x 
    #x and y are dictated by the original model 
    ON.slope = ((max(logsub$meanOcc) - min(logsub$meanOcc))/(max(logsub$logN) - min(logsub$logN)))
    #max in BOTH dimensions, x and y
    ON.max = max(logsub$meanOcc[logsub$logN == max(logsub$logN)]) #what point is at the "end of the line", for a given focal rte s? 
    ON.min = min(logsub$meanOcc[logsub$logN == min(logsub$logN)]) #what point is at the beginning of the line, for a given focal rte s?
    
    
    ON.r2 <- summary(ONlm.r2)$r.squared
    data.frame(stateroute = s, ON.alt_xmid, ON.alt_xmid_pred, ON.alt_xmid_dev, ON.mid_occ, 
               ON.slope, ON.max, ON.min, ON.r2)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    ON.alt_xmid = NA
    ON.alt_xmid_pred = NA
    ON.alt_xmid_dev = NA 
    ON.mid_occ = NA
    ON.slope = NA
    ON.max = NA
    ON.min = NA
    ON.r2 = NA
    temp = data.frame(stateroute = s, ON.alt_xmid, ON.alt_xmid_pred, ON.alt_xmid_dev, ON.mid_occ, 
                      ON.slope, ON.max, ON.min, ON.r2)
    return(temp)
    
  })
  ON.df = rbind(ON.df, ONmodel)
  
  
  
  #CA
  CAmodel = tryCatch({
    CAlog = lm(pctCore ~ logA, data = logsub) #lm instead of nls, reg linear model
    CApred_df = data.frame(preds = predict(CAlog), scale = logsub$scale)  #get preds -> is predicting unique per scale, all clear
    CAlm.r2 = lm(logsub$pctCore ~ CApred_df$preds) #get r2 from model 
    
    
    CA.alt_xmid = logsub$pctCore[logsub$scale == '3'] #@ scale == 3, for a given focal rte s, actual value
    #logsub[21,3] achieves same thing
    CA.alt_xmid_pred = CApred_df$preds[CApred_df$scale == '3']
    CA.alt_xmid_dev = (CA.alt_xmid - CA.alt_xmid_pred) #squared deviance of pred from actual val #need pred AT SCALE = 3 THO
    CA.mid_occ = as.character(min(logsub$scale[logsub$meanOcc > 0.49 & logsub$meanOcc < 0.60])) 
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #then save as a character so associated levels data doesn't stay stuck on the single data point
    
    
    #eq of a line 
    #((y2-y1)/(x2-x1)) 
    #pctCore vals are y, logA is the x 
    #x and y are dictated by the original model 
    CA.slope = ((max(logsub$pctCore) - min(logsub$pctCore))/(max(logsub$logA) - min(logsub$logA)))
    #max in BOTH dimensions, x and y
    CA.max = max(logsub$pctCore[logsub$logA == max(logsub$logA)]) #what point is at the "end of the line", for a given focal rte s? 
    CA.min = min(logsub$pctCore[logsub$logA == min(logsub$logA)]) #what point is at the beginning of the line, for a given focal rte s?
    
    
    CA.r2 <- summary(CAlm.r2)$r.squared
    data.frame(stateroute = s, CA.alt_xmid, CA.alt_xmid_pred, CA.alt_xmid_dev, CA.mid_occ, 
               CA.slope, CA.max, CA.min, CA.r2)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    CA.alt_xmid = NA
    CA.alt_xmid_pred = NA
    CA.alt_xmid_dev = NA 
    CA.mid_occ = NA
    CA.slope = NA
    CA.max = NA
    CA.min = NA
    CA.r2 = NA
    temp = data.frame(stateroute = s, CA.alt_xmid, CA.alt_xmid_pred, CA.alt_xmid_dev, CA.mid_occ, 
                      CA.slope, CA.max, CA.min, CA.r2)
    return(temp)
    
  })
  CA.df = rbind(CA.df, CAmodel)
  
  #CN 
  CNmodel = tryCatch({
    CNlog = lm(pctCore ~ logN, data = logsub) #lm instead of nls, reg linear model
    CNpred_df = data.frame(preds = predict(CNlog), scale = logsub$scale)  #get preds -> is predicting unique per scale, all clear
    CNlm.r2 = lm(logsub$pctCore ~ CNpred_df$preds) #get r2 from model 
    
    
    CN.alt_xmid = logsub$pctCore[logsub$scale == '3'] #@ scale == 3, for a given focal rte s, actual value
    #logsub[21,3] achieves same thing
    CN.alt_xmid_pred = CNpred_df$preds[CNpred_df$scale == '3']
    CN.alt_xmid_dev = (CN.alt_xmid - CN.alt_xmid_pred) #squared deviance of pred from actual val #need pred AT SCALE = 3 THO
    CN.mid_occ = as.character(min(logsub$scale[logsub$meanOcc > 0.49 & logsub$meanOcc < 0.60])) 
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #then save as a character so associated levels data doesn't stay stuck on the single data point
    
    
    #eq of a line 
    #((y2-y1)/(x2-x1)) 
    #pctCore vals are y, logN is the x 
    #x and y are dictated by the original model 
    CN.slope = ((max(logsub$pctCore) - min(logsub$pctCore))/(max(logsub$logN) - min(logsub$logN)))
    #max in BOTH dimensions, x and y
    CN.max = max(logsub$pctCore[logsub$logN == max(logsub$logN)]) #what point is at the "end of the line", for a given focal rte s? 
    CN.min = min(logsub$pctCore[logsub$logN == min(logsub$logN)]) #what point is at the beginning of the line, for a given focal rte s?
    
    
    CN.r2 <- summary(CNlm.r2)$r.squared
    data.frame(stateroute = s, CN.alt_xmid, CN.alt_xmid_pred, CN.alt_xmid_dev, CN.mid_occ, 
               CN.slope, CN.max, CN.min, CN.r2)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    CN.alt_xmid = NA
    CN.alt_xmid_pred = NA
    CN.alt_xmid_dev = NA 
    CN.mid_occ = NA
    CN.slope = NA
    CN.max = NA
    CN.min = NA
    CN.r2 = NA
    temp = data.frame(stateroute = s, CN.alt_xmid, CN.alt_xmid_pred, CN.alt_xmid_dev, CN.mid_occ, 
                      CN.slope, CN.max, CN.min, CN.r2)
    return(temp)
    
  })
  CN.df = rbind(CN.df, CNmodel)
  


  # Fitting % transient
TAmodel = tryCatch({
  TAlog = lm(-log(pctTran) ~ lnA, data = logsub) #try with log10(pctTran), log(pctTran) ~ logA, and pctTran ~ logA since relationships wonky
  TA = lm(-log(pctTran) ~ area, data = logsub)
  TApred_df = data.frame(preds = predict(TAlog), scale = logsub$scale)  #get preds -> is predicting unique per scale, all clear
  TAlm.r2 = lm(logsub$pctTran ~ TApred_df$preds) #get r2 from model


  TA.alt_xmid = logsub$pctTran[logsub$scale == '3'] #@ scale == 3, for a given focal rte s, actual value
  #logsub[21,3] achieves same thing
  TA.alt_xmid_pred = TApred_df$preds[TApred_df$scale == '3']
  TA.alt_xmid_dev = (TA.alt_xmid - TA.alt_xmid_pred) #squared deviance of pred from actual val #need pred AT SCALE = 3 THO
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
  TA.max = max(logsub$pctTran[logsub$lnA == max(logsub$lnA)]) #what point is at the "end of the line", for a given focal rte s?
  TA.min = min(logsub$pctTran[logsub$lnA == min(logsub$lnA)]) #what point is at the beginning of the line, for a given focal rte s?


  TA.r2 <- summary(TAlm.r2)$r.squared
  data.frame(stateroute = s, TA.alt_xmid, TA.alt_xmid_pred, TA.alt_xmid_dev, TA.mid_occ, TA.slope, TA.max, TA.min, TA.r2)

}, warning = function(w) {
  warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
}, error = function(e) {
  TA.alt_xmid = NA
  TA.alt_xmid_pred = NA
  TA.alt_xmid_dev = NA
  TA.mid_occ = NA
  TA.slope = NA
  TA.max = NA
  TA.min = NA
  TA.r2 = NA
  temp = data.frame(stateroute = s, TA.alt_xmid, TA.alt_xmid_pred, TA.alt_xmid_dev, TA.mid_occ, TA.slope, TA.max, TA.min, TA.r2)
  return(temp)
  })

TA.df = rbind(TA.df, TAmodel)

TNmodel = tryCatch({
    TNlog = lm(-log(pctTran) ~ lnN, data = logsub) #try with log10(pctTran), log(pctTran) ~ logA, and pctTran ~ logA since relationships wonky
    TN = lm(-log(pctTran) ~ aveN, data = logsub)
    TNpred_df = data.frame(preds = predict(TNlog), scale = logsub$scale)  #get preds -> is predicting unique per scale, all clear
    TNlm.r2 = lm(logsub$pctTran ~ TNpred_df$preds) #get r2 from model


    TN.alt_xmid = logsub$pctTran[logsub$scale == '3'] #@ scale == 3, for a given focal rte s, actual value
    #logsub[21,3] achieves same thing
    TN.alt_xmid_pred = TNpred_df$preds[TNpred_df$scale == '3']
    TN.alt_xmid_dev = (TN.alt_xmid - TN.alt_xmid_pred) #squared deviance of pred from actual val #need pred AT SCALE = 3 THO
    TN.mid_occ = as.character(min(logsub$scale[logsub$meanOcc > 0.49 & logsub$meanOcc < 0.60]))
    #want the FIRST instance where it hits this range -> how? minimum scale at which it does that
    #then save as a character so associated levels data doesn't stay stuck on the single data point


    #eq of a line
    #((y2-y1)/(x2-x1))
    #pctTran vals are y, lnN is the x
    #x and y are dictated by the original model
    TN.slope = ((max(logsub$pctTran) - min(logsub$pctTran))/(max(logsub$lnN) - min(logsub$lnN)))
    #max in BOTH dimensions, x and y
    TN.max = max(logsub$pctTran[logsub$lnN == max(logsub$lnN)]) #what point is at the "end of the line", for a given focal rte s?
    TN.min = min(logsub$pctTran[logsub$lnN == min(logsub$lnN)]) #what point is at the beginning of the line, for a given focal rte s?


    TN.r2 <- summary(TNlm.r2)$r.squared
    data.frame(stateroute = s, TN.alt_xmid, TN.alt_xmid_pred, TN.alt_xmid_dev, TN.mid_occ, TN.slope, TN.max, TN.min, TN.r2)

  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    TN.alt_xmid = NA
    TN.alt_xmid_pred = NA
    TN.alt_xmid_dev = NA
    TN.mid_occ = NA
    TN.slope = NA
    TN.max = NA
    TN.min = NA
    TN.r2 = NA
    temp = data.frame(stateroute = s, TN.alt_xmid, TN.alt_xmid_pred, TN.alt_xmid_dev, TN.mid_occ, TN.slope, TN.max, TN.min, TN.r2)
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
