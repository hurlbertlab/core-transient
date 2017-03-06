#'Variation in occupancy at multiple scales WITHIN & ABOVE BBS sites
#'REVISED ANALYSIS - Alternate: 6 region nested loop with nearest rtes paired with focal rtes
#'Molly F. Jenkins 
#'11/11/2016

#'Summary: ID six regions 
#'for (region in six regions) 
#'   for (scale in 2:66) (minimum common #' of rtes in each grid)
#'random sampling for every possible number of rtes between 2:66 instead of relying on a magic number sample for each grain 
#'create a distance matrix 
#'calculate great circle distance between routes 
#'subset distance matrix to focal routes
#'for (i in 1:length(a) 
  #'for j in (i +1):length(a) <- keeps redundant pairings so can rule out later as needed 

#'when focal route in loop is "2", want to find all of the rows in a where i OR j is the focal route 
#'then, for five focal routes, rank by distance, and take just the top five 



#'Set working directory to core-transient folder on github i.e. setwd("C:/git/core-transient/")


#'#' Please download and install the following packages:
#' maps, sp, rgdal, raster, maptools, rgeos, dplyr, fields
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
#'#'#'#'#'#'#'#'#'
#'----Write for_loop to calculate distances between every BBS site combination to find focal and associated routes that correspond best----
#''store minimum value for each iteration of combos in output table
#'does any of this use any of the fifty_top6 data? Have we moved on from that?


good_rtes2 = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes2.csv", header = TRUE) 


require(fields)
#' Distance calculation between all combination of 
distances = rdist.earth(matrix(c(good_rtes2$Longi, good_rtes2$Lati), ncol=2),
                        matrix(c(good_rtes2$Longi, good_rtes2$Lati), ncol=2),
                        miles=FALSE, R=6371)

dist.df = data.frame(rte1 = rep(good_rtes2$stateroute, each = nrow(good_rtes2)),
                     rte2 = rep(good_rtes2$stateroute, times = nrow(good_rtes2)),
                     dist = as.vector(distances))

#' inside loop, e.g., filter(dist.df, rte1 == 2001, rte2 != 2001)
dist.df2 = filter(dist.df, rte1 != rte2)

uniqrtes = unique(dist.df2$rte1)
#'#'#'#'Aggregating loop for above-route scales#'#'#'#' 

#'bring in NON-50 stop data 
bbs_allyears = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_allyears.csv", header = TRUE)
#'exclude AOU species codes <=2880 [waterbirds, shorebirds, etc], (>=3650 & <=3810) [owls],
#'(>=3900 &  <=3910) [kingfishers], (>=4160 & <=4210) [nightjars], 7010 [dipper]
#'^best practices 
bbs_bestAous = bbs_allyears %>% 
  filter(Aou > 2880 & !(Aou >= 3650 & Aou <= 3810) & !(Aou >= 3900 & Aou <= 3910) & 
                 !(Aou >= 4160 & Aou <= 4210) & Aou != 7010)  

numrtes = 1:65 #' based on min common number in top 6 grid cells 
output = data.frame(r = NULL, nu = NULL, AOU = NULL, occ = NULL)
for (r in uniqrtes) {
  for (nu in numrtes) {
  tmp = filter(dist.df2, rte1 == r) %>%
    arrange(dist)
  tmprtes = tmp$rte2[1:nu]   #'selects rtes to aggregate under focal route by dist from focal route, based on nu in numrtes range
  #' Aggregate those routes together, calc occupancy, etc
  
  bbssub = filter(bbs_bestAous, stateroute %in% c(r, tmprtes)) #'resolves issue of r not being included in occ calc on top of its paired routes
  bbsuniq = unique(bbssub[, c('Aou', 'Year')])
  occs = bbsuniq %>% dplyr::count(Aou) %>% dplyr::mutate(occ = n/15)
  
  temp = data.frame(focalrte = r,
                    numrtes = nu+1,                           #'total #' routes being aggregated
                    meanOcc = mean(occs$occ, na.rm =T),       #'mean occupancy
                    pctCore = sum(occs$occ > 2/3)/nrow(occs),
                    pctTrans = sum(occs$occ <= 1/3)/nrow(occs),#'fraction of species that are transient
                    totalAbun = sum(bbssub$SpeciesTotal)/15,  #'total community size (per year)
                    maxRadius = tmp$dist[nu])                 #'radius including rtes aggregated
  output = rbind(output, temp)
  print(paste("Focal rte", r, "#' rtes sampled", nu))
  
  } #'n loop
  
} #'r loop


bbs_focal_occs = as.data.frame(output)
# write.csv(bbs_focal_occs, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_focal_occs.csv", row.names = FALSE)
head(output)  
  

#'#'#'#'#'#'#'#'#'
#'#'#'#'Calc area for above route scale#'#'#'#'

bbs_focal_occs$area = bbs_focal_occs$numrtes*50*(pi*(0.4^2)) #'in km 
#' number of routes * fifty stops * area in sq km of a stop 

#'#'#'#'Occupancy vs area/#' rtes#'#'#'#'

plot(bbs_focal_occs$numrtes, bbs_focal_occs$meanOcc, xlab = "#' routes", ylab = "mean occupancy")
par(mfrow = c(2, 1))
plot(bbs_focal_occs$numrtes, bbs_focal_occs$pctTran, xlab = "#' routes", ylab = "% Trans")
plot(bbs_focal_occs$numrtes, bbs_focal_occs$pctCore, xlab = "#' routes", ylab = "% Core")

#'still just at above route scale tho - now need to stitch above and below together again 


#'#'#'#'Find lat/lons of focal routes, add env data, color code points#'#'#'#'
routes = read.csv('scripts/R-scripts/scale_analysis/routes.csv')
routes$stateroute = 1000*routes$statenum + routes$Route




#'#'#'#'rerun sub-route occ analysis#'#'#'#'

fifty_allyears = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/fifty_allyears.csv", header = TRUE)

fifty_bestAous = fifty_allyears %>% 
  filter(AOU > 2880 & !(AOU >= 3650 & AOU <= 3810) & !(AOU >= 3900 & AOU <= 3910) & 
           !(AOU >= 4160 & AOU <= 4210) & AOU != 7010) 
#'#'#'So for the whole dataset, 10 pt count stops: #'we are only getting one out of five chunks along 
#'want to estimate occupancy across each one, as of now only estimating for count 10 column 
#'fifty pt count data and then taking pts 1-5 and collapsing them all together 
#'#'#'#'#'#'#'#'#'
occ_counts = function(countData, countColumns, scale) {
  bbssub = countData[, c("stateroute", "year", "AOU", countColumns)]
  bbssub$groupCount = rowSums(bbssub[, countColumns])
  bbsu = unique(bbssub[bbssub[, "groupCount"]!= 0, c("stateroute", "year", "AOU")]) #'because this gets rid of 0's...
  
  abun.summ = bbssub %>% 
    group_by(stateroute, year) %>%  
    summarize(totalN = sum(groupCount)) %>%
    group_by(stateroute) %>%
    summarize(aveN = mean(totalN))
    
  occ.summ = bbsu %>%
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
  
#'need to fix nested dataframe output, why gen as list?     
  return(occ.summ)
}


#' Generic calculation of occupancy for a specified scale

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


#'#'#'#'joining above and below route scales, calc area#'#'#'#'
bbs_focal_occs = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_focal_occs.csv", header = TRUE)
bbs_below = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_below.csv", header = T)



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
  mutate(area = scale*50*(pi*(0.4^2)))
bbs_focal_occs$scale = as.factor(bbs_focal_occs$scale)
#area in km by # of routes * 50 stops in each rte * area of a stop (for above-route scale later)


bbs_allscales = rbind(bbs_below, bbs_focal_occs)
#write.csv(bbs_allscales, "C:/git/core-transient/data/bbs_allscales.csv", row.names = FALSE)
#can redirect later ^
##############################################################

####Cross-scale analysis and visualization####
bbs_allscales = read.csv("data/bbs_allscales.csv", header = TRUE)
bbs_allscales$logA = log10(bbs_allscales$area)
bbs_allscales$logN = log10(bbs_allscales$aveN)
bbs_allscales$lnA = log(bbs_allscales$area) #log is the natural log 
bbs_allscales$lnN = log(bbs_allscales$aveN) #rerun plots with this?

mod1 = lm(meanOcc~logA, data = bbs_allscales) #explains ~50% of the variation in occ
mod2 = lm(meanOcc~logN, data = bbs_allscales)
summary(mod1)

plot(meanOcc~logA, data = bbs_allscales, xlab = "Log Area" , ylab = "Mean Temporal Occupancy")
plot(meanOcc~logN, data = bbs_allscales, xlab = "Average Abundance" , ylab = "Mean Temporal Occupancy")
#^^same pattern


####Logistic curve fitting; sep loop for now####
#want to fit a logistic curve (not a regression!) to each as well 
#use nls: 
library(stats)

OA.df = data.frame(stateroute = numeric(), OA.A= numeric(), OA.i = numeric(), OA.k = numeric(), OA.r2 = numeric())
ON.df = data.frame(stateroute = numeric(), ON.A= numeric(), ON.i = numeric(), ON.k = numeric(), ON.r2 = numeric())
CA.df = data.frame(stateroute = numeric(), CA.A= numeric(), CA.i = numeric(), CA.k = numeric(), CA.r2 = numeric())
CN.df = data.frame(stateroute = numeric(), CN.A= numeric(), CN.i = numeric(), CN.k = numeric(), CN.r2 = numeric())
TA.df = data.frame(stateroute = numeric(), TAexp= numeric(), TApow = numeric(), TAexp.r2 = numeric(), TApow.r2 = numeric())
TN.df = data.frame(stateroute = numeric(), TNexp= numeric(), TNpow = numeric(), TNexp.r2 = numeric(), TNpow.r2 = numeric())


#Use tryCatch to run through all routes but store routes with errors
warnings = data.frame(stateroute = numeric(), warning = character())
#subspecify to only pull bbs data at year s 
stateroutes = unique(bbs_allscales$focalrte)


for(s in stateroutes){
  logsub = subset(bbs_allscales, bbs_allscales$focalrte == s)  
  #fitting the log curve for area (for each route)
  
  #OA 
  OAmodel = tryCatch({
    OAlog = nls(meanOcc ~ SSlogis(logA, Asym, xmid, scal), data = logsub)
    OApred = predict(OAlog)
    OAlm.r2 = lm(logsub$meanOcc ~ OApred)
    return(data.frame(stateroute = s, OA.A, OA.i, OA.k, 
                      OA.r2 = summary(OAlm.r2)$r.squared))
    return(data.frame(stateroute = s, OA.pred = OApred)) #can I return multiple outputs from a trycatch?
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    OA.i <- NA
    OA.A <- NA
    OA.k <- NA
    OA.r2 <- NA
    OA.pred <- NA
  }, finally = {
    OA.i <- summary(OAlog)$coefficients["xmid","Estimate"]
    OA.A <- summary(OAlog)$coefficients["Asym","Estimate"]
    OA.k <- summary(OAlog)$coefficients["scal","Estimate"]
    OA.r2 <- summary(OAlm.r2)$r.squared
    OA.pred <- OApred
    })
  OA.temp = data.frame(stateroute = s, OA.A, OA.i, OA.k, OA.r2)
  OA.temppred = data.frame(stateroute = s, OA.pred = OA.pred)
  OA.df = rbind(OA.df, OA.temp)
  OA.pred.df = rbind(OA.pred.df, OA.temppred)
  
  #ON 
  ONmodel = tryCatch({
    ONlog = nls(meanOcc ~ SSlogis(logN, Asym, xmid, scal), data = logsub)
    ONpred = predict(ONlog)
    ONlm.r2 = lm(logsub$meanOcc ~ ONpred)
    return(data.frame(stateroute = s, ON.A, ON.i, ON.k, 
                      ON.r2 = summary(ONlm.r2)$r.squared, ON.pred = ONpred))
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    ON.i <- NA
    ON.A <- NA
    ON.k <- NA
    ON.r2 <- NA
    ON.pred <- NA
  }, finally = {
    ON.i <- summary(ONlog)$coefficients["xmid","Estimate"]
    ON.A <- summary(ONlog)$coefficients["Asym","Estimate"]
    ON.k <- summary(ONlog)$coefficients["scal","Estimate"]
    ON.r2 <- summary(ONlm.r2)$r.squared
    ON.pred <- ONpred
    })
  ON.temp = data.frame(stateroute = s, ON.A, ON.i, ON.k, ON.r2, ON.pred) #fix
  ON.df = rbind(ON.df, ON.temp)
  
  #CA
  CAmodel = tryCatch({
    CAlog = nls(pctCore ~ SSlogis(logA, Asym, xmid, scal), data = logsub)
    CApred = predict(CAlog)
    CAlm.r2 = lm(logsub$pctCore ~ CApred)
    return(data.frame(stateroute = s, CA.A, CA.i, CA.k, 
                      CA.r2 = summary(CAlm.r2)$r.squared, CA.pred = CApred))
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    CA.i <- NA
    CA.A <- NA
    CA.k <- NA
    CA.r2 <- NA
    CA.pred <- NA
  }, finally = {
    CA.i <- summary(CAlog)$coefficients["xmid","Estimate"]
    CA.A <- summary(CAlog)$coefficients["Asym","Estimate"]
    CA.k <- summary(CAlog)$coefficients["scal","Estimate"]
    CA.r2 = summary(CAlm.r2)$r.squared
    CA.pred <- CApred
  })
  CA.temp = data.frame(stateroute = s, CA.A, CA.i, CA.k, CA.r2, CA.pred) 
  CA.df = rbind(CA.df, CA.temp)
  
  #CN
  CNmodel = tryCatch({
    CNlog = nls(pctCore ~ SSlogis(logN, Asym, xmid, scal), data = logsub)
    CNpred = predict(CNlog)
    CNlm.r2 = lm(logsub$pctCore ~ CNpred) #bootstraping r2 vals for CNlog since not in summary stats
    return(data.frame(stateroute = s, CN.A, CN.i, CN.k, 
                      CN.r2 = summary(CNlm.r2)$r.squared, CN.pred = CNpred))
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    CN.i <- NA
    CN.A <- NA
    CN.k <- NA
    CN.r2 <- NA
    CN.pred <- NA
  }, finally = {
    CN.i <- summary(CNlog)$coefficients["xmid","Estimate"]
    CN.A <- summary(CNlog)$coefficients["Asym","Estimate"]
    CN.k <- summary(CNlog)$coefficients["scal","Estimate"]
    CN.r2 <- summary(CNlm.r2)$r.squared
    CN.pred = CNpred
    })
  CN.temp = data.frame(stateroute = s, CN.A, CN.i, CN.k, CN.r2, CN.pred) 
  CN.df = rbind(CN.df, CN.temp)

  # Fitting % transient
  #TA
  TAlog = lm(log(pctTran) ~ lnA, data = logsub)
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

#write.csv(coefs, "C:/git/core-transient/scripts/R-scripts/scale_analysis/coefs.csv", row.names = FALSE) #updated 02/27
#exp mods have much better r2 vals for pctTran than power 


bbs_allscales = read.csv("data/bbs_allscales.csv", header = TRUE)
bbs_allscales$logA = log10(bbs_allscales$area)
bbs_allscales$logN = log10(bbs_allscales$aveN)
bbs_allscales$lnA = log(bbs_allscales$area) #log is the natural log 
bbs_allscales$lnN = log(bbs_allscales$aveN) #rerun plots with this?
coefs = read.csv("scripts/R-scripts/scale_analysis/coefs.csv", header = TRUE)


logistic_fcn = function(x, Asym, xmid, scal) {
  out = Asym/(1 + exp((xmid - x)/scal))
  return(out)
}


preds.df = data.frame(stateroute = numeric(), OApreds= numeric(), ONpreds = numeric(), 
                      CApreds = numeric(), CNpreds = numeric(),
                      TApreds = numeric(), TNpreds = numeric())


stateroutes = unique(bbs_allscales$focalrte)
#pdf("output/plots/Molly Plots/BBS_scaleplots.pdf", onefile = TRUE)
tiff("output/plots/Molly Plots/pngs/BBS_scaleplots%04d.tif")


coef_join = coefs %>% inner_join(bbs_allscales, by = c("stateroute"="focalrte"))




stateroutes = c(2001, 2010, 2014)
for (s in stateroutes) {
  theme_set(theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
  coef_sub = subset(coef_join, coef_join$stateroute == s)
  
  #OA
  OApreds = logistic_fcn(coef_sub[,33], coef_sub[,2], coef_sub[,3], coef_sub[,4]) 
  plot1 = ggplot(coef_sub, aes(x = logA, y = meanOcc))+geom_point(colour = "firebrick")+
    geom_line(aes(x = logA, y = OApreds), color = "navy")
  
  #ON
  ONpreds = logistic_fcn(coef_sub[,33], coef_sub[,6], coef_sub[,7], coef_sub[,8])
  plot2 = ggplot(coef_sub, aes(x = logN, y = meanOcc))+geom_point(colour = "firebrick")+
    geom_line(aes(x = logN, y = ONpreds), color = "navy")
 
  #CA
  CApreds = logistic_fcn(coef_sub[,33], coef_sub[,10], coef_sub[,11], coef_sub[,12])
  plot1_2= ggplot(coef_sub, aes(x = logA, y = pctCore))+geom_point(colour = "turquoise")+
    geom_line(aes(x = logA, y = CApreds), color = "navy") 
 
  #CN
  CNpreds = logistic_fcn(coef_sub[,33], coef_sub[,14], coef_sub[,15], coef_sub[,16])
  plot2_2= ggplot(coef_sub, aes(x = logN, y = pctCore))+geom_point(colour = "turquoise")+
    geom_line(aes(x = logN, y = CNpreds), color = "navy")
  
  #not using log fcn for %Transient relationships bc relationship diff, exp had higher pred power also 
  #TA
  TApreds =  coef_sub[,35]^(-1*coef_sub[,18]) #35 = optimum
  plot1_3 = ggplot(coef_sub, aes(x = lnA, y = log(pctTran)))+geom_point(colour = "olivedrab")+
    geom_line(aes(x = lnA, y = TApreds), color = "navy")
  
  #TN
  TNpreds = coef_sub[,35]^(-1*coef_sub[,22])
  plot2_3 = ggplot(coef_sub, aes(x = lnN, y = log(pctTran)))+geom_point(colour = "olivedrab")+
    geom_line(aes(x = lnN, y = TNpreds), color = "navy")
  
  #storing plots
  predplot = grid.arrange(plot1, plot2, plot1_2, plot2_2, plot1_3, plot2_3,
                          ncol=2, top = paste("predplot_", s, sep = ""))
  #storing preds:
  temp.df = data.frame(stateroute = s, OApreds= OApreds , ONpreds = ONpreds, 
                       CApreds = CApreds, CNpreds = CNpreds,
                       TApreds = TApreds, TNpreds = TNpreds)
  preds.df = rbind(preds.df, temp.df)
  
}
dev.off()

#example plot
#plot(coef_sub$logA, coef_sub$meanOcc)
# points(coef_sub[,33], OApreds, type=  'l', col='red')
#cite output in plots in lieu of geom_smooth for updated output



####Characterizing changes at the level of a single focal rte, above and below#### 
#six panel plot for each rte, output as pdfs for 02/05
#set up as forloop that exports each plot before moving on to the next stateroute?
#just need to replace bbs_allscales with a subset that changes every loop, 
#dictated by stateroute 
#and I want R to bring them all together and export/save as pdf at end
stateroutes = unique(bbs_allscales$focalrte)
#pdf("output/plots/Molly Plots/BBS_scaleplots.pdf", onefile = TRUE)
png("output/plots/Molly Plots/pngs/BBS_scaleplots%03d.png") #stored as sep png files for creating gif for talks 
for (s in stateroutes) { 
  #log(area)
  theme_set(theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
  plotsub = subset(bbs_allscales, bbs_allscales$focalrte == s)
  plot1 = ggplot(plotsub, aes(x = logA, y = meanOcc))+labs(x = "Log area", y = "Mean % Occupancy")+geom_point(colour = "firebrick")+geom_smooth(se=FALSE)
  plot1_2= ggplot(plotsub, aes(x = logA, y = pctCore))+labs(x = "Log area", y = "% Core Occupancy")+geom_point(colour = "turquoise")+geom_smooth(se=FALSE)
  plot1_3 = ggplot(plotsub, aes(x = lnA, y = pctTran))+labs(x = "Log area", y = "% Transient Occupancy")+geom_point(colour = "olivedrab")+geom_smooth(se=FALSE)
  
  #aveN
  plot2 = ggplot(plotsub, aes(x=logN, y =meanOcc))+labs(x = "Log abundance", y = "Mean % Occupancy")+geom_point(colour = "firebrick")+geom_smooth(se=FALSE)
  plot2_2 = ggplot(plotsub, aes(x=logN, y =pctCore))+labs(x = "Log abundance", y = "% Core Occupancy")+geom_point(colour = "turquoise")+geom_smooth(se=FALSE)
  plot2_3 =ggplot(plotsub, aes(x=lnN, y =pctTran))+labs(x = "Log abundance", y = "% Transient Occupancy")+geom_point(colour = "olivedrab")+geom_smooth(se=FALSE)
  
  
  ####changed to log_10^^^^####
  
  #setting up aveN and log(area) cols side by side 
  library(gridExtra)
  scaleplot = grid.arrange(plot1, plot2, plot1_2, plot2_2, plot1_3, plot2_3, ncol=2, 
                           top = paste("scaleplot_", s, sep = ""))
  #saved to core-transient/output/plots  
}
dev.off()



####Env data add-in####
#for now just use what we have, that's fine 
#bring in lat-lons for each focal route and creating sites
bbs_allscales = read.csv("data/bbs_allscales.csv", header = TRUE)
bbs_latlon = read.csv("//bioark.ad.unc.edu/HurlbertLab/Jenkins/BBS scaled/good_rtes2.csv", header = TRUE)
bbs_allscales = rename(bbs_latlon, focalrte = stateroute) %>%
  right_join(bbs_allscales, by = "focalrte")


#temp
sites = data.frame(longitude = bbs_allscales$Longi, latitude = bbs_allscales$Lati)
 #points(sites$longitude, sites$latitude, col= "red", pch=16)
temp = paste('//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/BIOCLIM_meanTemp/tmean',1:12,'.bil', sep='')
tmean = stack(temp) 
# Find MEAN across all months
meanT = calc(tmean, mean)
meanT
# Convert to actual temp
meanT = meanT/10 #done
bbs_allscales$temp<-raster::extract(meanT, sites, buffer = 40000, fun = mean) #meters since data pure lat-lons, unprojected, mean of means for cells 
bbs_allscales$vartemp<-raster::extract(meanT, sites, buffer = 40000, fun = var)
#then take fun to get mean and var?

#precip 
prec<-paste('//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/2-25-2011/prec/prec',1:12, '.bil', sep ='')
mprecip = stack(prec)
Pcalc = calc(mprecip, mean)
bbs_allscales$meanP = raster::extract(Pcalc, sites, buffer = 40000, fun = mean)
bbs_allscales$varP = raster::extract(Pcalc, sites, buffer = 40000, fun = var)

#ndvi 
ndvim<-raster("//bioark.ad.unc.edu/HurlbertLab/GIS/MODIS NDVI/Vegetation_Indices_may-aug_2000-2010.gri")
ndvimean = ndvim/10000
bbs_allscales$ndvi<-raster::extract(ndvimean, sites, buffer = 40000, fun = mean)
bbs_allscales$varndvi<-raster::extract(ndvimean, sites, buffer = 40000, fun = var)


#skipping elev and elev rad for now because files are weird
#elev 
# #mean elevation PLUS elevational range 
# elevmean<-raster("C:/git/core-transient/wc10/alt.bil")
# bbs_allscales$elev<-extract(elevmean, sites, buffer = 40000, fun = mean)
# bbs_allscales$varelev<-extract(elevmean, sites, buffer = 40000, fun = var)
# 
# #elev radius
# #pull in radius elev data from Coyle folder 
# elevrad<-raster("elevation_var_40km_radius.gri")
# elevrad<-raster("//bioark.ad.unc.edu/HurlbertLab/Coyle/Projects/BBS Core/Data/elevation_var_40km_radius.gri")
# 
# #OR: 
# elevrad<-raster("C:git/core-transient/scripts/R-scripts/scale_analysis/elevation_var_aggregate_40_1km.gri")
# 
# 
# 
# #need to re-project data points to match projection of elevation raster data 
# #modify below code
# elev_proj = "+proj=laea +lat_0=40.68 +lon_0=-92.925 +units=km +ellps=WGS84" # A string that defines the projection
# points2 = SpatialPoints(sites)
# points2 = SpatialPoints(sites, proj4string=CRS(elev_proj))
# points2 = SpatialPoints(sites, proj4string=CRS("+proj=longlat +datum=WGS84"))
# points3 = spTransform(points2, CRS(elev_proj))
# buff = gBuffer(points3, width=40)
# #extract data just like before with raster function 
# bbs_allscales$erad = raster::extract(elevrad, points3,buffer = 40000, fun = mean)
# bbs_allscales$varerad = raster::extract(elevrad, points3,buffer = 40000, fun = var)


bbs_envs = bbs_allscales
#write.csv(bbs_envs, "scripts/R-scripts/scale_analysis/bbs_envs.csv", row.names = FALSE) wrote file 2/22 w/out elev and using old env data


####Coef vs env variation models####
# linear models explaining either logistic slope or negative exponential or power slopes as
# a function of env variables
bbs_envs = read.csv("scripts/R-scripts/scale_analysis/bbs_envs.csv", header = TRUE)
coefs = read.csv("scripts/R-scripts/scale_analysis/coefs.csv", header = TRUE)
uniq_env = unique(bbs_envs[, c('focalrte', 'temp', 'vartemp', 'meanP', 'varP', 'ndvi', 'varndvi')])
# Merge environmental data with the coef shape data
env_coefs = inner_join(coefs, uniq_env, by = c('stateroute' = 'focalrte'))

# Can also just look at the correlation matrix, e.g.
covmatrix = round(cor(coefs[, 2:ncol(coefs)]), 2)


# nested for loop for examining variation in coefs/fitted curves explained by env vars 
rsqrd_df = data.frame(dep = character(), ind = character(), r2 = numeric())

for (d in 2:25) {
  for (i in 26:ncol(env_coefs)) {
    tempmod = lm(env_coefs[,d] ~ env_coefs[,i])
    tempdf = data.frame(dep = names(env_coefs)[d], 
                        ind = names(env_coefs)[i], 
                        r2 = summary(tempmod)$r.squared)
    rsqrd_df = rbind(rsqrd_df, tempdf)
  }
}

#write.csv(rsqrd_df, "scripts/R-scripts/scale_analysis/mod_rsqrds.csv", row.names = FALSE) #updated 02/27 POST-meeting
####Visually Characterizing r2 vals####
rsqrd_df = read.csv("scripts/R-scripts/scale_analysis/mod_rsqrds.csv", header = TRUE)
ggplot(data = rsqrd_df, aes(x = dep, y = r2))+geom_boxplot()+facet_wrap(~ind)
#boxplot(r2~ind, data = rsqrd_df)




####Plot obs vs pred####
pdf("output/plots/Molly Plots/BBS_testplot.pdf", onefile = TRUE)
# 
# for (s in stateroutes) {
# s = 2001
# plotsub = subset(bbs_allscales, bbs_allscales$focalrte == s)
# OAlog = nls(meanOcc ~ SSlogis(logA, Asym, xmid, scal), data = plotsub)
# OApred = predict(OAlog)
# 
# theme_set(theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
# plot1 = ggplot(plotsub, aes(x = logA, y = meanOcc))+geom_point(colour = "olivedrab")+
#   geom_line(aes(x = logA, y = OApred), color = "navy")}


#not yet working in loop but works for s = 2001; not fault of pctTran either

stateroutes = unique(bbs_allscales$focalrte)
tiff("output/plots/Molly Plots/BBS_predplots.tif")
stateroutes = 2001 
for (s in stateroutes) {
  #log(area)
  theme_set(theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
  plotsub = subset(bbs_allscales, bbs_allscales$focalrte == s)

   OAlog = nls(meanOcc ~ SSlogis(logA, Asym, xmid, scal), data = plotsub)
   OApred = predict(OAlog)
  plot1 = ggplot(plotsub, aes(x = logA, y = meanOcc))+geom_point(colour = "firebrick")+
    geom_line(aes(x = logA, y = OApred), color = "navy")

   CAlog = nls(pctCore ~ SSlogis(logA, Asym, xmid, scal), data = plotsub)
   CApred = predict(CAlog)
  plot1_2= ggplot(plotsub, aes(x = logA, y = pctCore))+geom_point(colour = "turquoise")+
    geom_line(aes(x = logA, y = CApred), color = "navy")

   TAlog = lm(log(pctTran) ~ lnA, data = plotsub)
   TApred = predict(TAlog)
  plot1_3 = ggplot(plotsub, aes(x = lnA, y = log(pctTran)))+geom_point(colour = "olivedrab")+
   geom_line(aes(x = lnA, y = TApred), color = "navy")


  #aveN
   ONlog = nls(meanOcc ~ SSlogis(logN, Asym, xmid, scal), data = plotsub)
   ONpred = predict(ONlog)
  plot2 = ggplot(plotsub, aes(x = logN, y = meanOcc))+geom_point(colour = "firebrick")+
    geom_line(aes(x = logN, y = ONpred), color = "navy")

   CNlog = nls(pctCore ~ SSlogis(logN, Asym, xmid, scal), data = plotsub)
   CNpred = predict(CNlog)
  plot2_2= ggplot(plotsub, aes(x = logN, y = pctCore))+geom_point(colour = "turquoise")+
    geom_line(aes(x = logN, y = CNpred), color = "navy")

   TNlog = lm(log(pctTran) ~ lnN, data = plotsub)
   TNpred = predict(TNlog)
  plot2_3 = ggplot(plotsub, aes(x = lnN, y = log(pctTran)))+geom_point(colour = "olivedrab")+
    geom_line(aes(x = lnN, y = TNpred), color = "navy")

  predplot = grid.arrange(plot1, plot2, plot1_2, plot2_2, plot1_3, plot2_3,
                          ncol=2, top = paste("predplot_", s, sep = ""))
  }
dev.off()


