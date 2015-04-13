RichnessYearSubsetFrame = function(i){
  siteID = nestedDataset[[2]][i]
  nestedDatasetDf = nestedDataset[[1]]
  nestedDatasetDf$siteID = nestedDatasetDf[,siteID]
  # Subset to sites with a high enough species richness and year samples:
  siteSr_nTime = ddply(nestedDatasetDf, .(siteID), summarize,
                       sr = length(unique(species)), 
                       nTime = length(unique(year)))
  
  goodSites = subset(siteSr_nTime, sr >= 10 & siteSr_nTime$nTime >= minNYears)$siteID
  outFrame = na.omit(nestedDatasetDf[nestedDatasetDf$siteID %in% goodSites,])
  return(select(outFrame, one_of(c('siteID','date','year', 'site', 'species','count'))))
}

zFinder = function(inData, minNYears = 10, proportionalThreshold = .5){
  data = inData # Note: This data are obtained using RichnessYearSubsetFrame(i)
  # Calculate the number of temporal samples per site and year: 
  
  spaceTime = ddply(data, .(siteID, year), summarize, temporalSubsamples = length(unique(date)))
  
  # zPossible is a potential threshold temporal subsampling:
  
  zPossible = sort(unique(spaceTime$temporalSubsamples))
    
 zList = list(length = length(zPossible))
 goodSites1 = data.frame(z = NA, siteID = NA)
 
  for(i in 1:length(zPossible)){
    # Years in which the subsamplings was greater than equal to z for a given site:
    siteYearsGTEz = ddply(filter(spaceTime, temporalSubsamples>=zPossible[i]), .(siteID),
                summarize, yearsGTEz = length(unique(year)))
    # The proportion of sites with a minimum number of 10 years of sampling >= z
    goodSites = filter(siteYearsGTEz, yearsGTEz>=minNYears)$siteID
    nSites = length(goodSites)
    propSites =  nSites/length(unique(data$siteID))
    zList[[i]] = data.frame(z = zPossible[i], nSites, propSites)
    
    goodSites1 = rbind(goodSites1, data.frame(z = rep(zPossible[i], length(goodSites)), 
                                              siteID = goodSites))
  }
  
  z = max(filter(rbind.fill(zList), propSites >=  proportionalThreshold)$z)

  goodSites1 = goodSites1[-1,]
  goodSitesForZ = goodSites1$siteID[goodSites1$z == z]
   
  output = list (z = z, goodSites = goodSitesForZ)
 
  return(output)
}

inData = RichnessYearSubsetFrame(1)

z = zFinder(inData)
  

wFinder = function(inData, zOutput, minNYears = 10, proportionalThreshold = .5){
  data = inData
  # Add a siteYear column:
  data$siteYear = paste(data$siteID, data$year, sep ='_')
  data = filter(data, siteID %in% zOutput$goodSites)
  # Summarize data by the number of temporal samples in a given year:
  spaceTime = ddply(data, .(siteYear, siteID, year), summarize, 
                           temporalSubsamples = length(unique(date)),
                           spatialSubsamples = length(unique(site)))
  # Subset timeZsummary to siteYears >= z
  spaceTimeZsub = filter(spaceTime, temporalSubsamples>=zOutput$z)
  
  
  # Get possible values for w:
  w = unique(spaceTimeZsub$spatialSubsamples)
  
#   for(i in 1:length(wPossible)){
#     # Years in which the subsamplings was greater than equal to z for a given site:
#     siteYearsGTEz = ddply(filter(spaceTimeZsub, spatialSubsamples>=wPossible[i]), .(siteYear),
#                           summarize, yearsGTEz = length(unique(year)))
#     # The proportion of sites with a minimum number of 10 years of sampling >= z
#     nSites = nrow(filter(siteYearsGTEz, yearsGTEz>=minNYears))
#     propSites =  nSites/length(unique(data$siteID))
#     siteYearsGTEzList[[i]] = data.frame(z = zPossible[i], nSites, propSites)
#   }
#   
#   z = max(filter(rbind.fill(siteYearsGTEzList), propSites >=  proportionalThreshold)$z)
#   return(z)
}
  
}
  
  
  
  
  