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


mod1 = lm(meanOcc~logA, data = bbs_allscales) #explains ~50% of the variation in occ
mod2 = lm(meanOcc~logN, data = bbs_allscales)
summary(mod1)

plot(meanOcc~logA, data = bbs_allscales, xlab = "Log Area" , ylab = "Mean Temporal Occupancy")
plot(meanOcc~logN, data = bbs_allscales, xlab = "Average Abundance" , ylab = "Mean Temporal Occupancy")
#^^same pattern



####Characterizing changes at the level of a single focal rte, above and below#### 
#six panel plot for each rte, output as pdfs for 02/05
#set up as forloop that exports each plot before moving on to the next stateroute?
#just need to replace bbs_allscales with a subset that changes every loop, 
#dictated by stateroute 
#and I want R to bring them all together and export/save as pdf at end
stateroutes = unique(bbs_allscales$focalrte)
pdf("output/plots/BBS_scaleplots.pdf", onefile = TRUE)
for (s in stateroutes) { 
#log(area)
theme_set(theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
plotsub = subset(bbs_allscales, bbs_allscales$focalrte == s)
plot1 = ggplot(plotsub, aes(x = logA, y = meanOcc))+geom_point(colour = "firebrick")+geom_smooth(se=FALSE)
plot1_2= ggplot(plotsub, aes(x = logA, y = pctCore))+geom_point(colour = "turquoise")+geom_smooth(se=FALSE)
plot1_3 = ggplot(plotsub, aes(x = logA, y = pctTran))+geom_point(colour = "olivedrab")+geom_smooth(se=FALSE)

#aveN
plot2 = ggplot(plotsub, aes(x=logN, y =meanOcc))+geom_point(colour = "firebrick")+geom_smooth(se=FALSE)
plot2_2 = ggplot(plotsub, aes(x=logN, y =pctCore))+geom_point(colour = "turquoise")+geom_smooth(se=FALSE)
plot2_3 =ggplot(plotsub, aes(x=logN, y =pctTran))+geom_point(colour = "olivedrab")+geom_smooth(se=FALSE)


####change to log_10^^^^####

#setting up aveN and log(area) cols side by side 
library(gridExtra)
scaleplot = grid.arrange(plot1, plot2, plot1_2, plot2_2, plot1_3, plot2_3, ncol=2, 
                         top = paste("scaleplot_", s, sep = ""))
#saved to core-transient/output/plots  
}
dev.off()

####Logistic curve fitting; sep loop for now####
#want to fit a logistic curve (not a regression!) to each as well 
#use nls: 
library(stats)

OA.df = data.frame(stateroute = numeric(), OA.A= numeric(), OA.i = numeric(), OA.k = numeric())
ON.df = data.frame(stateroute = numeric(), ON.A= numeric(), ON.i = numeric(), ON.k = numeric())
CA.df = data.frame(stateroute = numeric(), CA.A= numeric(), CA.i = numeric(), CA.k = numeric())
CN.df = data.frame(stateroute = numeric(), CN.A= numeric(), CN.i = numeric(), CN.k = numeric())
TA.df = data.frame(stateroute = numeric(), TA.A= numeric(), TA.i = numeric(), TA.k = numeric())
TN.df = data.frame(stateroute = numeric(), TN.A= numeric(), TN.i = numeric(), TN.k = numeric())


#Use tryCatch to run through all routes but store routes with errors
warnings = data.frame(stateroute = numeric(), warning = character())
#subspecify to only pull bbs data at year s 
stateroutes = unique(bbs_allscales$focalrte)

#OA mod 
for(s in stateroutes){
  logsub = subset(bbs_allscales, bbs_allscales$focalrte == s)  
  #fitting the log curve for area (for each route)
    OAmodel = tryCatch({
    OAlog = nls(meanOcc ~ SSlogis(logA, Asym, xmid, scal), data = logsub)
    return(data.frame(stateroute = s, OA.A, OA.i, OA.k))
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    OA.i <- NA
    OA.A <- NA
    OA.k <- NA
  }, finally = {
    OA.i <- summary(OAlog)$coefficients["xmid","Estimate"]
    OA.A <- summary(OAlog)$coefficients["Asym","Estimate"]
    OA.k <- summary(OAlog)$coefficients["scal","Estimate"]
    #OA.tmp = data.frame(stateroute = s, OA.A, OA.i, OA.k)
  })
  
  OA.temp = data.frame(stateroute = s, OA.A, OA.i, OA.k) #fix
  OA.df = rbind(OA.df, OA.temp)
}

#ON model
for(s in stateroutes){
  logsub = subset(bbs_allscales, bbs_allscales$focalrte == s)  
  #fitting the log curve for aveN (for each route)
  ONmodel = tryCatch({
    ONlog = nls(meanOcc ~ SSlogis(logN, Asym, xmid, scal), data = logsub)
    return(data.frame(stateroute = s, ON.A, ON.i, ON.k))
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    ON.i <- NA
    ON.A <- NA
    ON.k <- NA
  }, finally = {
    ON.i <- summary(ONlog)$coefficients["xmid","Estimate"]
    ON.A <- summary(ONlog)$coefficients["Asym","Estimate"]
    ON.k <- summary(ONlog)$coefficients["scal","Estimate"]
    #ON.tmp = data.frame(stateroute = s, ON.A, ON.i, ON.k)
  })
  
  ON.temp = data.frame(stateroute = s, ON.A, ON.i, ON.k) #fix
  ON.df = rbind(ON.df, ON.temp)
}
  
#CA model
for(s in stateroutes){
  logsub = subset(bbs_allscales, bbs_allscales$focalrte == s)  
  #fitting the log curve for area (for each route)
  CAmodel = tryCatch({
    CAlog = nls(pctCore ~ SSlogis(logA, Asym, xmid, scal), data = logsub)
    return(data.frame(stateroute = s, CA.A, CA.i, CA.k))
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    CA.i <- NA
    CA.A <- NA
    CA.k <- NA
  }, finally = {
    CA.i <- summary(CAlog)$coefficients["xmid","Estimate"]
    CA.A <- summary(CAlog)$coefficients["Asym","Estimate"]
    CA.k <- summary(CAlog)$coefficients["scal","Estimate"]
    #CA.tmp = data.frame(stateroute = s, CA.A, CA.i, CA.k)
  })
  
  CA.temp = data.frame(stateroute = s, CA.A, CA.i, CA.k) #fix
  CA.df = rbind(CA.df, CA.temp)
}

#CN model
for(s in stateroutes){
  logsub = subset(bbs_allscales, bbs_allscales$focalrte == s)  
  #fitting the log curve for aveN (for each route)
  CNmodel = tryCatch({
    CNlog = nls(pctCore ~ SSlogis(logN, Asym, xmid, scal), data = logsub)
    return(data.frame(stateroute = s, CN.A, CN.i, CN.k))
  }, warning = function(w) {
    warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
  }, error = function(e) {
    CN.i <- NA
    CN.A <- NA
    CN.k <- NA
  }, finally = {
    CN.i <- summary(CNlog)$coefficients["xmid","Estimate"]
    CN.A <- summary(CNlog)$coefficients["Asym","Estimate"]
    CN.k <- summary(CNlog)$coefficients["scal","Estimate"]
    #CN.tmp = data.frame(stateroute = s, CN.A, CN.i, CN.k)
  })
  
  CN.temp = data.frame(stateroute = s, CN.A, CN.i, CN.k) #fix
  CN.df = rbind(CN.df, CN.temp)
}

####Env data add-in####

#for now just use what we have, that's fine 

#bring in lat-lons for each focal route and creating sites

bbs_latlon = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/good_rtes2.csv", header = TRUE)
bbs_allscales = rename(bbs_latlon, focalrte = stateroute) %>%
  right_join(bbs_allscales, by = "focalrte")


sites = data.frame(longitude = bbs_allscales$Longi, latitude = bbs_allscales$Lati)
#points(sites$longitude, sites$latitude, col= "red", pch=16)
temp = paste('//bioark.ad.unc.edu/HurlbertLab/GIS/ClimateData/BIOCLIM_meanTemp/tmean',1:12,'.bil', sep='')
tmean = stack(temp) 
# Find MEAN across all months
meanT = calc(tmean, mean)
meanT
# Convert to actual temp
meanT = meanT/10 #done

bbs_allscales$temp<-raster::extract(meanT, sites)


####Troubleshooting pctTran functions####
#commented out pctTran models because need to use a diff formula to fit (fault neg slope)
# fcn = function(x, xmid, Asym, scal) {Asym/1 - exp((xmid-x)/scal)}
# st <- coef(nls(log(pctTran) ~ log(fcn(log(area), xmid, Asym, scal)), data = logsub, 
#                start = c(xmid = 1, Asym = 1, scal = 1)))

#^^^above code produces NaN's and infinite loop 

#good starting point: 

#try fitting a negative exponential 
#compare fit with logA vs reg A mods and test agains
#self starting 

#^trying to use graphs to eyeball start vals still not working, still get singular error method; 
#making xmid negative just repeats step fator redux error
#as x increases y decreases in sigmoidal fashion -> find function and try defining it 

#do model between predicted and observed vals -> can get r squared out of that 
#
TAlog = lm(log(pctTran) ~ logA, data = logsub)
TA = lm(log(pctTran) ~ area, data = logsub)
#log vs non-log models 
#rsquared on log is better 


#playing with nplr package in R prior to lm mods

test = nplr(x = logsub$logA, y = convertToProp(logsub$pctTran))
test 
plot(test)
testpars = getPar(test)
testpars$params$xmid

#playing with nplr mods (see code below)

pctTran_coefs = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/pctTran_coefs.csv", header = TRUE)
#same coefs for every stateroute tho, why?

# 
# #TA model
# for(s in stateroutes){
#   logsub = subset(bbs_allscales, bbs_allscales$focalrte == s)
#   #fitting the log curve for area (for each route)
#   TAmodel = tryCatch({
#     TAlog = nplr(x = logsub$logA, y = convertToProp(logsub$pctTran))
#     return(data.frame(stateroute = s, TA.A, TA.i, TA.k))
#   }, warning = function(w) {
#     warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
#   }, error = function(e) {
#     TA.i <- NA
#     TA.A <- NA
#     TA.k <- NA
#   }, finally = {
#     testpars = getPar(test)
#     TA.i <- testpars$params$xmid
#     TA.A <- testpars$params$bottom
#     TA.k <- testpars$params$scal
#     #TA.tmp = data.frame(stateroute = s, TA.A, TA.i, TA.k)
#   })
# 
#   TA.temp = data.frame(stateroute = s, TA.A, TA.i, TA.k) #fix
#   TA.df = rbind(TA.df, TA.temp)
# }
# 
# # #TN model
# for(s in stateroutes){
#   logsub = subset(bbs_allscales, bbs_allscales$focalrte == s)
#   #fitting the log curve for area (for each route)
#   TNmodel = tryCatch({
#     TNlog = nplr(x = logsub$logN, y = convertToProp(logsub$pctTran))
#     return(data.frame(stateroute = s, TN.A, TN.i, TN.k))
#   }, warning = function(w) {
#     warnings = rbind(warnings, data.frame(stateroute = s, warning = w))
#   }, error = function(e) {
#     TN.i <- NA
#     TN.A <- NA
#     TN.k <- NA
#   }, finally = {
#     testpars = getPar(test)
#     TN.i <- testpars$params$xmid
#     TN.A <- testpars$params$bottom
#     TN.k <- testpars$params$scal
#     #TN.tmp = daTN.frame(stateroute = s, TN.A, TN.i, TN.k)
#   })
#   
#   TN.temp = data.frame(stateroute = s, TN.A, TN.i, TN.k) #fix
#   TN.df = rbind(TN.df, TN.temp)
# }
# 
# pctTran_coefs = data.frame(TA.df, TN.df)
# 


logcurve_coefs = data.frame(OA.df, ON.df, CA.df, CN.df, TA.df, TN.df)
write.csv(logcurve_coefs, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/logcurve_coefs.csv", row.names = FALSE)
#saving as intermediate in case
#it appears no NA's! 



