test = fakeData(2,2,10,2)

nestedDataset = getNestedSiteDataset(test)

wzMaker2 = function(i){
  
  siteGrain = nestedDataset[[2]][i]
  nestedDatasetDf = nestedDataset[[1]]
  nestedDatasetDf$siteGrain = nestedDatasetDf[,siteGrain]
  
  # Subset to sites with a high enough species richness and year samples:
  
  siteSr_nTime = ddply(nestedDatasetDf, .(siteGrain), summarize,
                       sr = length(unique(species)), 
                       nTime = length(unique(year)))
  
  goodSites = subset(siteSr_nTime, sr >= 10 & siteSr_nTime$nTime >= 5)$siteGrain
  
  d1 = nestedDatasetDf[nestedDatasetDf$siteGrain %in% goodSites,]
  
  # Get data frame of the number of spatial and temporal samples by site and year:
  
  spaceTime = ddply(d1,.(siteGrain, year), summarize,
                    spatialSubsamples = length(unique(site)),
                    temporalSubsamples = length(unique(date)))
  
  spaceTime = na.omit(spaceTime)
  
  # Get the value for w threshold:
  
  w = seq(min(spaceTime$spatialSubsamples), max(spaceTime$spatialSubsamples, by  = 1))
  z = seq(min(spaceTime$temporalSubsamples), max(spaceTime$temporalSubsamples, by  = 1))
  wz = expand.grid(w = w, z = z)
  
  sites = unique(spaceTime$siteGrain)
    
  outList = list(length = length(sites))
  
  for(i in 1:length(sites)) {
    for(j in 1:nrow(wz)){
      siteSub = subset(spaceTime, siteGrain  == sites[i])
      w = wz[j, 1]
      z = wz[j, 2]
      YearsGTEwz = sum(siteSub$spatialSubsamples >= w &
                         siteSub$temporalSubsamples >= z)
      GTEYearThreshold = ifelse(YearsGTEwz >= 5, 1, 0)
      outList[[i]] = data.frame(site = unique(siteSub$siteGrain), 
                                w , z, 
                                YearsGTEwz,
                                GTEYearThreshold)
    }}
  
  # This dataframe provides the calculation of the number of years a site was greater than or equal to w and z for each site and potential value of w and z:
  
  wzFrame = rbind.fill(outList)
  
  # Output:
  
  outList = list(spatialGrain, wz, wzFrame, spaceTime)
  names(outList) = c('spatialGrain', 'wz','wzFrame', 'spaceTimeSamples')
  return(outList)
}


wzMaker2(2)

wzMaker2(1)

getMaxWz = function(i){
  wzFrame = wzMaker2(i)[['wzFrame']]
  wzList = list(length = nrow(wzFrame))
  nSites = nrow(wzFrame)
  wzCombinationSum = ddply(wzFrame, .(w, z), summarize, 
                           wzSum = sum(GTEYearThreshold))
  
  wzCombinationSum$siteProp = wzCombinationSum$wzSum/nSites  
  wzFrameGTE20 = subset(wzCombinationSum, siteProp >= .2) 
  wzFrameGTEmax = subset(wzFrameGTE20, w + z == max(w+z))
  return(wzFrameGTEmax)
}

wzMaker2(1)[[3]]

getMaxWz(1)

wzMaker2(2)[[3]]

getMaxWz(2)












