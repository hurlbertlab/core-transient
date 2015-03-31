test = fakeData(2,2,10,2)

test1a = fakeData(2,2,10,2)


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
  z = seq(min(spaceTime$temporalSubsamples), max(spaceTime$temporalSubsamples, by  = 1))
  wz = expand.grid(w = w, z = z)
  
  sites = unique(spaceTime$siteGrain)
    
  outList = list(length = length(sites))
  
  for(i in 1:length(sites)) {
    for(j in 1:nrow(wz)){
      siteSub = subset(spaceTime, siteGrain  == sites[i])
      w = wz[j, 1]
      z = wz[j, 2]
      outList[[i]] = data.frame(site = unique(siteSub$siteGrain), 
                                w = w, z = z, 
                                YearsGTEwz = sum(siteSub$spatialSubsamples >= w &
                                    siteSub$temporalSubsamples >= z))
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










