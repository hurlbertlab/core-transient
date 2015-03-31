test = fakeData(2,2,10,2)

nestedDataset = getNestedSiteDataset(test)

wzMaker2 = function(i){
  
  siteGrain = nestedDataset[[2]][i]
  nestedDataset = nestedDataset[[1]]
  nestedDataset$siteGrain = nestedDataset[,siteGrain]
  
  # Subset to sites with a high enough species richness and year samples:
  
  siteSr_nTime = ddply(nestedDataset, .(siteGrain), summarize,
                       sr = length(unique(species)), 
                       nTime = length(unique(year)))
  
  goodSites = subset(siteSr_nTime, sr >= 10 & siteSr_nTime$nTime >= 5)$siteGrain
  
  d1 = nestedDataset[nestedDataset$siteGrain %in% goodSites,]
  
  # Get data frame of the number of spatial and temporal samples by site and year:
  
  spaceTime = ddply(d1,.(siteGrain, year), summarize,
                    spatialSubsamples = length(unique(site)),
                    temporalSubsamples = length(unique(date)))
  
  spaceTime = na.omit(spaceTime)
  
  # Get the value for w threshold:
  
  w = seq(min(spaceTime$spatialSubsamples), max(spaceTime$spatialSubsamples, by  = 1))
  siteYears = nrow(spaceTime)
  wFrame = data.frame(w)
  for(i in 1:length(w)) {
    wFrame[i,2] = nrow(subset(spaceTime, spatialSubsamples  <= w[i]))
    wFrame[i,3] = wFrame[i,2]/siteYears
  }
  names(wFrame)[2:3] = c('siteYears','propW')
  w = wFrame[which.min(abs(wFrame[,'propW'] - threshold)), 'w']
  
  # Get the value for the z threshold:
  
  z = seq(min(spaceTime$temporalSubsamples), max(spaceTime$temporalSubsamples, by  = 1))
  zFrame = data.frame(z)
  for(i in 1:length(z)){
    zFrame[i,2] = nrow(subset(spaceTime, temporalSubsamples <=z[i]))
    zFrame[i,3] = zFrame[i,2]/siteYears
  }
  names(zFrame)[2:3] = c('siteYears','propZ')
  z = zFrame[which.min(abs(zFrame[,'propZ'] - threshold)), 'z']
  
  # Output:
  
  outList = list(spatialGrain,wFrame,zFrame, w, z, spaceTime)
  names(outList) = c('spatialGrain', 'wFrame', 'zFrame','w', 'z', 'spaceTimeSamples')
  return(outList)
}


wzMaker2(1)









