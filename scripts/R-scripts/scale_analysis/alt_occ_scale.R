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
library(purrr)

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
                    pctCore = sum(occs$occ > 2/3)/nrow(occs), #'fraction of species that are core
                    pctTran = sum(occs$occ <= 1/3)/nrow(occs),#'fraction of species that are transient
                    totalAbun = sum(bbssub$SpeciesTotal)/15,  #'total community size (per year)
                    maxRadius = tmp$dist[nu])                 #'radius including rtes aggregated
  output = rbind(output, temp)
  print(paste("Focal rte", r, "#' rtes sampled", nu))
  
  } #'n loop
  
} #'r loop


bbs_focal_occs = as.data.frame(output)
write.csv(bbs_focal_occs, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_focal_occs.csv", row.names = FALSE)
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
  
  occ.df = bbsu %>%
    count(stateroute, AOU) %>%
    mutate(occ = n/15, scale = scale, subrouteID = countColumns[1])
    
  occ.summ = occ.df %>%
    group_by(stateroute) %>%
    summarize(meanOcc = mean(occ), 
              pctCore = sum(occ > 2/3)/length(occ),
              pctTran = sum(occ <= 1/3)/length(occ)) %>%
    mutate(scale = paste(scale, g, sep = "-"))
  
  abun.summ = bbssub %>% 
    group_by(stateroute, year) %>%  
    summarize(totalN = sum(groupCount)) %>%
    group_by(stateroute) %>%
    summarize(aveN = mean(totalN)) %>%
    mutate(scale = paste(scale, g, sep = "-"))
      
#'need to fix nested dataframe output, why gen as list?     
  return(list(occ = occ.summ, abun = abun.summ))
}


#' Generic calculation of occupancy for a specified scale

scales = c(5, 10, 25, 50)


output = c()
for (scale in scales) {
  numGroups = floor(50/scale)
  for (g in 1:numGroups) {
    groupedCols = paste("Stop", ((g-1)*scale + 1):(g*scale), sep = "")
    temp = occ_counts(fifty_bestAous, groupedCols, scale)
    output = rbind(output, temp) #'rbind error: variables don't have same length? 
  }
}

bbs_output<-data.frame(output)


####output of occ and abun values tidying####
#temps all named the same but can tidy and sep out based on scales
#have correct abun and occ values but str is strange!! 

abun  = as.matrix(bbs_output$abun)
occ = as.matrix(bbs_output$occ) #successful 

#taking out "temp" subdivisions/rows and bringing data together across rows, returning to matrix form 
occ2 = t(as.matrix(flatten(occ)))
output = c()
seq_end = c(seq(from = 5, to = 90, by = 5)) 
m = 4
#fix hardcoding to softcoding tomorrow, 
#make forloop generalizeable 


for(s in seq_end) {
  temp = as.data.frame(occ2[,(s-m):s])
  output = rbind(output, temp)
}
occ_df = output 
#occ done 


abun2 = t(as.matrix(flatten(abun)))
output = c()
seq_end = c(seq(from = 3, to = 54, by = 3))
m = 2

for(s in seq_end) {
  temp = as.data.frame(abun2[,(s-m):s])
  output = rbind(output, temp)
}

abun_df = output
#abun done, longer rows than occ_df tho?  

####merging abun and occ back together####
#combine abun and occ back together (even tho calc'd differently, still associated w/spec stateroutes)
#using unique combo of scale and stateroute 
bbs_below = full_join(abun_df, occ_df, by = c("stateroute", "scale"))

#scale = numrtes in above-scale df 
#write.csv(bbs_below, "//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_below.csv", row.names = FALSE)


#should I avg occ and abun across segments at the same scale? 








#not sure why row # gets bumped up, suspect because residual buffered NA's 
sub = which(is.na(bbs_below))
sub2 = bbs_below[sub,]


#'#'#'#'finished above route aggregation of routes#'#'#'#'
bbs_focal_occs = read.csv("//bioark.ad.unc.edu/HurlbertLab/Gartland/BBS scaled/bbs_focal_occs.csv", header = TRUE)
#'^^correct, up-to-date version of ABOVE ROUTE aggregated pairings as of 01/19/2017

