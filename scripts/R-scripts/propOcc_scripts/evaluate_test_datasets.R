
wzMaker2 = function(i){
  
  siteID = nestedDataset[[2]][i]
  nestedDatasetDf = nestedDataset[[1]]
  nestedDatasetDf$siteID = nestedDatasetDf[,siteID]
  
  # Subset to sites with a high enough species richness and year samples:
  
  siteSr_nTime = ddply(nestedDatasetDf, .(siteID), summarize,
                       sr = length(unique(species)), 
                       nTime = length(unique(year)))
  
  goodSites = subset(siteSr_nTime, sr >= 10 & siteSr_nTime$nTime >= 5)$siteID
  
  d1 = nestedDatasetDf[nestedDatasetDf$siteID %in% goodSites,]
  
  # Get data frame of the number of spatial and temporal samples by site and year:
  
  spaceTime = ddply(d1,.(siteID, year), summarize,
                    spatialSubsamples = length(unique(site)),
                    temporalSubsamples = length(unique(date)))
  
  spaceTime = na.omit(spaceTime)
  
  # Summarize, counting the length of years for a given site having a given number of spatial and temporal subsamples.
  
  spaceTimeSummary = ddply(spaceTime,.(siteID, spatialSubsamples, temporalSubsamples),
                           summarize, years = length(year))
  
  # Get the value for w threshold:
  
  w = seq(min(spaceTime$spatialSubsamples), max(spaceTime$spatialSubsamples, by  = 1))
  z = seq(min(spaceTime$temporalSubsamples), max(spaceTime$temporalSubsamples, by  = 1))
  wz = expand.grid(w = w, z = z)
  
  # Match the potential wz values with the actual values:
  
#   wzMatch = na.omit(merge(wz, spaceTimeSummary, by.x = c('w','z'), 
#                 by.y = c('spatialSubsamples','temporalSubsamples'), all = T))
#   
  outList = list(length = nrow(spaceTime))
  for(i in 1:nrow(spaceTime)){
    outList[[i]] = cbind(wz, spaceTime[i,], row.names = NULL)
  }
t1 = rbind.fill(outList)

t1$GTEwz = t1$spatialSubsamples >= t1$w & t1$temporalSubsamples >= t1$z

t2 = ddply(t1, .(siteID, w,z), summarize, 
      yearsGTEwz = sum(spatialSubsamples>=w&temporalSubsamples>=z))

t2$wzSiteCombo = ifelse(t2$yearsGTEwz>=5, 1, 0)

t3 = ddply(t2, .(w,z), summarize, sitesGTEwz = sum(wzSiteCombo))

t3$wzSumScaled = t3$w/max(t3$w)+t3$z/max(t3$z)

t3$siteProp = t3$sitesGTEwz/length(goodSites)

wzSitePropGTE.2 = subset(t3, siteProp >=.2)

wzMax = subset(wzSitePropGTE.2, wzSumScaled == max(wzSumScaled))[,1:2]

outList = list(spaceTimeSummary, t3, wzMax)
names(outList) = c('spaceTimeSummary', 'wzFrame', 'wzMax')
return(outList)
}

# Evaluating test datasets:

# Dataset 1: all site-years with equal subsamples
# Expected output: w = 4, z = 4 (at larger scale)
#                  No sites dropped

testData1 = fakeData(nSites = 4, nPlots = 4, nYears = 5, nSeasons = 4)

nestedDataset = getNestedSiteDataset(testData1)

(wzList = wzMaker2(1))


# Dataset 2: half of sites with high spatial subsampling, half with low,
#            all with equal temporal subsampling
# Expected output: w = 6, z = 4
#                  2/4 sites dropped

d2a = fakeData(nSites = 2, nPlots = 6, nYears = 5, nSeasons = 4)
d2b = fakeData(nSites = 2, nPlots = 2, nYears = 5, nSeasons = 4, 
               startingSite = 3)
d2 = rbind(d2a, d2b)

testData2 = d2

nestedDataset = getNestedSiteDataset(testData2)

(wzList = wzMaker2(1))

(wzList2 = wzMaker2(2))

# Dataset 3: half of sites with high spatial subsampling but low temporal
#            and vice versa
# Expected output: w = 6, z = 2 or
#                  w = 2, z = 6 BUT NOT
#                  w = 6, z = 6 because no sites will meet that criterion
#                  2/4 sites dropped

d3a = fakeData(nSites = 2, nPlots = 6, nYears = 5, nSeasons = 2)
d3b = fakeData(nSites = 2, nPlots = 2, nYears = 5, nSeasons = 6, 
               startingSite = 3)
d3 = rbind(d3a, d3b)

testData3 = d3

nestedDataset = getNestedSiteDataset(testData3)

(wzList = wzMaker2(1))

(wzList2 = wzMaker2(2))














