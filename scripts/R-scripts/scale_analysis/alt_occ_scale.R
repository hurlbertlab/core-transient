#'Variation in occupancy at multiple scales WITHIN & ABOVE BBS sites
#'REVISED ANALYSIS
#'Molly F. Jenkins 
#'04/23/2017

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


####Calculations for Occupancy above the scale of a BBS route####
good_rtes2 = read.csv(paste(BBS, "good_rtes2.csv", sep = ""), header = TRUE) 
require(fields)
#Distance calculation between all combination of routes to pair them by min dist for aggregation
distances = rdist.earth(matrix(c(good_rtes2$Longi, good_rtes2$Lati), ncol=2),
                        matrix(c(good_rtes2$Longi, good_rtes2$Lati), ncol=2),
                        miles=FALSE, R=6371)
dist.df = data.frame(rte1 = rep(good_rtes2$stateroute, each = nrow(good_rtes2)),
                     rte2 = rep(good_rtes2$stateroute, times = nrow(good_rtes2)),
                     dist = as.vector(distances))
#write.csv(dist.df, "C:/git/core-transient/scripts/R-scripts/scale_analysis/dist_df.csv", row.names = FALSE) for later calcs

#bring in NON-50 stop data for above-route scale res
bbs_allyears = read.csv(paste(BBS, "bbs_allyears.csv", sep = ""), header = TRUE)
bbs_bestAous = bbs_allyears %>% 
  filter(Aou > 2880 & !(Aou >= 3650 & Aou <= 3810) & !(Aou >= 3900 & Aou <= 3910) & 
                 !(Aou >= 4160 & Aou <= 4210) & Aou != 7010)  #excluding shorebirds and owls, data less reliable


numrtes = 1:65 # based on min common number in top 6 grid cells, see grid_sampling_justification script 
output = data.frame(r = NULL, nu = NULL, AOU = NULL, occ = NULL)
for (r in uniqrtes) {
  for (nu in numrtes) {
  tmp = filter(dist.df2, rte1 == r) %>%
    arrange(dist)
  tmprtes = tmp$rte2[1:nu]   
  #Aggregate routes together based on distance, calc occupancy, etc
  
  bbssub = filter(bbs_bestAous, stateroute %in% c(r, tmprtes)) 
  bbsuniq = unique(bbssub[, c('Aou', 'Year')])
  occs = bbsuniq %>% dplyr::count(Aou) %>% dplyr::mutate(occ = n/15)
  
  temp = data.frame(focalrte = r,
                    numrtes = nu+1,                           #total # routes being aggregated
                    meanOcc = mean(occs$occ, na.rm =T),       #mean occupancy
                    pctCore = sum(occs$occ > 2/3)/nrow(occs),
                    pctTrans = sum(occs$occ <= 1/3)/nrow(occs), #fraction of species that are transient
                    totalAbun = sum(bbssub$SpeciesTotal)/15,  #total community size (per year)
                    maxRadius = tmp$dist[nu])                 #radius including rtes aggregated
  output = rbind(output, temp)
  print(paste("Focal rte", r, "#' rtes sampled", nu))
  
  } #n loop
  
} #r loop

bbs_focal_occs = as.data.frame(output)
#Calc area for above route scale
bbs_focal_occs$area = bbs_focal_occs$numrtes*50*(pi*(0.4^2)) #number of routes * fifty stops * area in sq km of a stop 
# write.csv(bbs_focal_occs, "/scripts/R-scripts/scale_analysis/bbs_focal_occs.csv", row.names = FALSE)


####Plotting BBS occupancy at scales above a BBS route####
plot(bbs_focal_occs$numrtes, bbs_focal_occs$meanOcc, xlab = "#' routes", ylab = "mean occupancy")
par(mfrow = c(2, 1))
plot(bbs_focal_occs$numrtes, bbs_focal_occs$pctTran, xlab = "#' routes", ylab = "% Trans")
plot(bbs_focal_occs$numrtes, bbs_focal_occs$pctCore, xlab = "#' routes", ylab = "% Core")


####Below-route occupancy calculations####
fifty_allyears = read.csv(paste(BBS, "fifty_allyears.csv", sep = ""), header = TRUE)
fifty_bestAous = fifty_allyears %>% 
  filter(AOU > 2880 & !(AOU >= 3650 & AOU <= 3810) & !(AOU >= 3900 & AOU <= 3910) & 
           !(AOU >= 4160 & AOU <= 4210) & AOU != 7010) #leaving out owls, waterbirds as less reliable data

#occ_counts function for calculating occupancy at any scale
occ_counts = function(countData, countColumns, scale) {
  bbssub = countData[, c("stateroute", "year", "AOU", countColumns)]
  bbssub$groupCount = rowSums(bbssub[, countColumns])
  bbsu = unique(bbssub[bbssub[, "groupCount"]!= 0, c("stateroute", "year", "AOU")]) 
  
  abun.summ = bbssub %>% #abundance
    group_by(stateroute, year) %>%  
    summarize(totalN = sum(groupCount)) %>%
    group_by(stateroute) %>%
    summarize(aveN = mean(totalN))
    
  occ.summ = bbsu %>% #occupancy
    count(stateroute, AOU) %>%
    mutate(occ = n/15, scale = scale, subrouteID = countColumns[1]) %>%
    group_by(stateroute) %>%
    summarize(meanOcc = mean(occ), 
              pctCore = sum(occ > 2/3)/length(occ),
              pctTran = sum(occ <= 1/3)/length(occ)) %>%
              #spRichTrans33  
               # spRichTrans25 = sum(occ <= 1/4)/length(occ),
              # spRichTrans10 = sum(occ <= 0.1)/length(occ)) %>%
    mutate(scale = paste(scale, g, sep = "-")) %>%
    left_join(abun.summ, by = 'stateroute')
    return(occ.summ)
}


# Generic calculation of occupancy for a specified scale
scales = c(5, 10, 25, 50)

output = c()
for (scale in scales) {
  numGroups = floor(50/scale)
  for (g in 1:numGroups) {
    groupedCols = paste("Stop", ((g-1)*scale + 1):(g*scale), sep = "")
    temp = occ_counts(fifty_bestAous, groupedCols, scale)
    output = rbind(output, temp) 
  }
}
bbs_below<-data.frame(output)


####Binding above and below route scales, calc area####
bbs_focal_occs = read.csv(paste(BBS, "bbs_focal_occs.csv", sep = ""), header = TRUE) 
bbs_below = read.csv(paste(BBS, "bbs_below.csv", sep = ""), header = T)


#adding maxRadius column to bbs_below w/NA's + renaming and rearranging columns accordingly, creating area cols
bbs_below= bbs_below %>% 
  mutate(maxRadius = c("NA")) %>%
  dplyr::rename(focalrte = stateroute) %>%
  select(focalrte, scale, everything()) %>%
  mutate(area = (as.integer(lapply(strsplit(as.character(bbs_below$scale), 
                                           split="-"), "[", 1)))*(pi*(0.4^2))) 
#modify and split scale so that it's just the # of stops in each seg; not the seg order # preceded by a "-"


bbs_focal_occs = bbs_focal_occs %>% 
  dplyr::rename(scale = numrtes, aveN = totalAbun) %>%
  mutate(area = scale*50*(pi*(0.4^2))) #area in km by # of routes * 50 stops in each rte * area of a stop (for above-route scale later)
bbs_focal_occs$scale = as.factor(bbs_focal_occs$scale)


bbs_allscales = rbind(bbs_below, bbs_focal_occs) #rbind ok since all share column names
#write.csv(bbs_allscales, "C:/git/core-transient/scripts/R-scripts/scale_analysis/bbs_allscales.csv", row.names = FALSE)


####Cross-scale analysis and visualization####
bbs_allscales = read.csv("data/BBS/bbs_allscales.csv", header = TRUE)
bbs_allscales$logA = log10(bbs_allscales$area)
bbs_allscales$logN = log10(bbs_allscales$aveN)
bbs_allscales$lnA = log(bbs_allscales$area) #log is the natural log 
bbs_allscales$lnN = log(bbs_allscales$aveN) #rerun plots with this?

mod1 = lm(meanOcc~logA, data = bbs_allscales) #explains ~50% of the variation in occ
mod2 = lm(meanOcc~logN, data = bbs_allscales)
summary(mod1)

plot(meanOcc~logA, data = bbs_allscales, xlab = "Log Area" , ylab = "Mean Temporal Occupancy")
plot(meanOcc~logN, data = bbs_allscales, xlab = "Average Abundance" , ylab = "Mean Temporal Occupancy")
#^^same pattern roughly; abundance describes ~same amt of variance as area so serves as a good proxy 


####Extract coefficients from scale-occupancy relationships for analysis####
OA.df = data.frame(stateroute = numeric(), OA.A= numeric(), OA.i = numeric(), OA.k = numeric(), OA.r2 = numeric())
ON.df = data.frame(stateroute = numeric(), ON.A= numeric(), ON.i = numeric(), ON.k = numeric(), ON.r2 = numeric())
CA.df = data.frame(stateroute = numeric(), CA.A= numeric(), CA.i = numeric(), CA.k = numeric(), CA.r2 = numeric())
CN.df = data.frame(stateroute = numeric(), CN.A= numeric(), CN.i = numeric(), CN.k = numeric(), CN.r2 = numeric())
TA.df = data.frame(stateroute = numeric(), TAexp= numeric(), TApow = numeric(), TAexp.r2 = numeric(), TApow.r2 = numeric())
TN.df = data.frame(stateroute = numeric(), TNexp= numeric(), TNpow = numeric(), TNexp.r2 = numeric(), TNpow.r2 = numeric())

warnings = data.frame(stateroute = numeric(), warning = character())
stateroutes = unique(bbs_allscales$focalrte) #this stuff is the same, looks normal ^


#06/19 version of tryCatch
for(s in stateroutes){
  logsub = subset(bbs_allscales, bbs_allscales$focalrte == s)  
  #fitting the log curve for area (for each route)
  
  #OA 
  OAmodel = tryCatch({
    OAlog = nls(meanOcc ~ SSlogis(logA, Asym, xmid, scal), data = logsub)
    OApred = predict(OAlog)
    OAlm.r2 = lm(logsub$meanOcc ~ OApred)
    OA.i <- summary(OAlog)$coefficients["xmid","Estimate"]
    OA.A <- summary(OAlog)$coefficients["Asym","Estimate"]
    OA.k <- summary(OAlog)$coefficients["scal","Estimate"]
    OA.r2 <- summary(OAlm.r2)$r.squared
    data.frame(stateroute = s, OA.A, OA.i, OA.k, OA.r2)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    OA.i <- NA
    OA.A <- NA
    OA.k <- NA
    OA.r2 <- NA
    temp = data.frame(stateroute = s, OA.A, OA.i, OA.k, OA.r2)
    return(temp)
    
  })
  OA.df = rbind(OA.df, OAmodel)

  #ON 
  ONmodel = tryCatch({
    ONlog = nls(meanOcc ~ SSlogis(logN, Asym, xmid, scal), data = logsub)
    ONpred = predict(ONlog)
    ONlm.r2 = lm(logsub$meanOcc ~ ONpred)
    
    ON.i <- summary(ONlog)$coefficients["xmid","Estimate"]
    ON.A <- summary(OAlog)$coefficients["Asym","Estimate"]
    ON.k <- summary(ONlog)$coefficients["scal","Estimate"]
    ON.r2 <- summary(ONlm.r2)$r.squared
    data.frame(stateroute = s, ON.A, ON.i, ON.k, ON.r2)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    ON.i <- NA
    ON.A <- NA
    ON.k <- NA
    ON.r2 <- NA
    temp = data.frame(stateroute = s, ON.A, ON.i, ON.k, ON.r2)
    return(temp)
    
  })
  ON.df = rbind(ON.df, ONmodel)
  
  #CA
  CAmodel = tryCatch({
    CAlog = nls(pctCore ~ SSlogis(logA, Asym, xmid, scal), data = logsub)
    CApred = predict(CAlog)
    CAlm.r2 = lm(logsub$pctCore ~ CApred)
    
    CA.i <- summary(CAlog)$coefficients["xmid","Estimate"]
    CA.A <- summary(CAlog)$coefficients["Asym","Estimate"]
    CA.k <- summary(CAlog)$coefficients["scal","Estimate"]
    CA.r2 <- summary(CAlm.r2)$r.squared
    data.frame(stateroute = s, CA.A, CA.i, CA.k, CA.r2)
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    CA.i <- NA
    CA.A <- NA
    CA.k <- NA
    CA.r2 <- NA
    temp = data.frame(stateroute = s, CA.A, CA.i, CA.k, CA.r2)
    return(temp)
    
  })
  CA.df = rbind(CA.df, CAmodel)
  
  #CN
  CNmodel = tryCatch({
    CNlog = nls(pctCore ~ SSlogis(logN, Asym, xmid, scal), data = logsub)
    CNpred = predict(CNlog)
    CNlm.r2 = lm(logsub$pctCore ~ CNpred) #bootstraping r2 vals for CNlog since not in summary stats
    
    CN.i <- summary(CNlog)$coefficients["xmid","Estimate"]
    CN.A <- summary(CAlog)$coefficients["Asym","Estimate"]
    CN.k <- summary(CNlog)$coefficients["scal","Estimate"]
    CN.r2 <- summary(CNlm.r2)$r.squared
    data.frame(stateroute = s, CN.A, CN.i, CN.k, CN.r2)
    
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    CN.i <- NA
    CN.A <- NA
    CN.k <- NA
    CN.r2 <- NA
    temp = data.frame(stateroute = s, CN.A, CN.i, CN.k, CN.r2)
    return(temp)
    
  })
  CN.df = rbind(CN.df, CNmodel)
  
  
  # Fitting % transient
  #TA #revisit!!
  TAlog = lm(log(pctTran) ~ lnA, data = logsub) #try with log10(pctTran), log(pctTran) ~ logA, and pctTran ~ logA since relationships wonky  
  TA = lm(log(pctTran) ~ area, data = logsub)
  TA.temp = data.frame(stateroute = s, 
                       TAexp = TAlog$coefficients[2],
                       TApow = TA$coefficients[2], 
                       TAexp.r2 = summary(TAlog)$r.squared, 
                       TApow.r2 = summary(TA)$r.squared) 
  TA.df = rbind(TA.df, TA.temp)
  
  #TN  
  TNlog = lm(log(pctTran) ~ lnN, data = logsub)
  TN = lm(log(pctTran) ~ area, data = logsub)
    TN.temp = data.frame(stateroute = s, 
                       TNexp = TNlog$coefficients[2],
                       TNpow = TN$coefficients[2], 
                       TNexp.r2 = summary(TNlog)$r.squared, 
                       TNpow.r2 = summary(TN)$r.squared)
  TN.df = rbind(TN.df, TN.temp)
}

#join all together using inner_join by focal rte, not cbind 
coefs = OA.df %>% 
  inner_join(ON.df, OA.df, by = "stateroute") %>% 
  inner_join(CA.df, OA.df, by = "stateroute") %>% 
  inner_join(CN.df, OA.df, by = "stateroute") %>% 
  inner_join(TA.df, OA.df, by = "stateroute") %>% 
  inner_join(TN.df, OA.df, by = "stateroute")  

write.csv(coefs, "C:/git/core-transient/scripts/R-scripts/scale_analysis/coefs.csv", row.names = FALSE) #updated 06/19
#exp mods have much better r2 vals for pctTran than power 

####Plotting occupancy-scale relationships with observed and predicted values####
bbs_allscales = read.csv("data/bbs_allscales.csv", header = TRUE)
bbs_allscales$logA = log10(bbs_allscales$area)
bbs_allscales$logN = log10(bbs_allscales$aveN)
bbs_allscales$lnA = log(bbs_allscales$area) #log is the natural log 
bbs_allscales$lnN = log(bbs_allscales$aveN) #rerun plots with this?
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
     geom_line(aes(x = logA, y = OApreds), color = "navy") +labs(x = "Log area", y = "Mean % Occupancy")
  
  
  #ON
  ONpreds = logistic_fcn(coef_sub[,34], coef_sub[,6], coef_sub[,7], coef_sub[,8])
   plot2 = ggplot(coef_sub, aes(x = logN, y = meanOcc))+geom_point(colour = "firebrick")+
     geom_line(aes(x = logN, y = ONpreds), color = "navy") +labs(x = "Log abundance", y = "Mean % Occupancy")
   
 
  #CA
  CApreds = logistic_fcn(coef_sub[,33], coef_sub[,10], coef_sub[,11], coef_sub[,12])
   plot1_2= ggplot(coef_sub, aes(x = logA, y = pctCore))+geom_point(colour = "turquoise")+
     geom_line(aes(x = logA, y = CApreds), color = "navy")+labs(x = "Log area", y = "% Core Occupancy")
   
  #aveN
  #CN
  CNpreds = logistic_fcn(coef_sub[,34], coef_sub[,14], coef_sub[,15], coef_sub[,16])
   plot2_2= ggplot(coef_sub, aes(x = logN, y = pctCore))+geom_point(colour = "turquoise")+
     geom_line(aes(x = logN, y = CNpreds), color = "navy")+labs(x = "Log abundance", y = "% Core Occupancy")

  #using exponential function since higher explanatory power than pwr function
  #TA
  TApreds =  coef_sub[,35]*(coef_sub[,18]) #35 = optimum; replacing ^ with * bc natural log, removing -1
  plot1_3 = ggplot(coef_sub, aes(x = lnA, y = log(pctTran)))+geom_point(colour = "olivedrab")+
    geom_line(aes(x = lnA, y = log(TApreds)), color = "navy") +labs(x = "Log area", y = "% Transient Occupancy")

  #TN
  TNpreds = coef_sub[,36]*(coef_sub[,22])
  plot2_3 = ggplot(coef_sub, aes(x = lnN, y = log(pctTran)))+geom_point(colour = "olivedrab")+
    geom_line(aes(x = lnN, y = TNpreds), color = "navy")+labs(x = "Log abundance", y = "% Transient Occupancy")

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
