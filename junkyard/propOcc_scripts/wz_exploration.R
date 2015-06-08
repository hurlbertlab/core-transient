
#-------------------------------------------------------------------------------*
# ---- SUBSET DATASET TO SITES WITH ADEQUATE TIME SAMPLES AND RICHNESS ----
#-------------------------------------------------------------------------------*

# Note: Prior to running "RichnessYearSubsetFrame", you must have created the nestedDataset using the function "getNestedDataset". The output of the getNestedDataset function is a list with the first list item being the dataset, expanded across all potential spatial and temporal grains and the second being a vector of the names of the site columns. The "i" in this function refers to the the value in the vector of site names. 

RichnessYearSubsetFrame = function(spatialGrain = 'site', temporalGrain = 'year', minNYears = 10, minSpRich = 10){
  # Extract the nested dataset:
    nestedDatasetDf = nestedDataset[[1]]
  # Add a column named siteID:
    nestedDatasetDf$siteID = nestedDatasetDf[,spatialGrain]
  # Get the number of years and species richness for each site: 
    siteSr_nTime = ddply(nestedDatasetDf, .(siteID), summarize,
                         sr = length(unique(species)), 
                         nTime = length(unique(year)))
  # Subset to sites with a high enough species richness and year samples:
    goodSites = filter(siteSr_nTime, sr >= minSpRich & 
                         siteSr_nTime$nTime >= minNYears)$siteID
  # If statement to return if there's no good sites:
    if(length(goodSites) == 0) {return(print('No acceptable sites, rethink site definitions or temporal scale'))}
  else {
  # Match good sites and the dataframe:
    outFrame = na.omit(nestedDatasetDf[nestedDatasetDf$siteID %in% goodSites,])
  # Add date column as a specific temporal grain:
    outFrame$date = outFrame[,temporalGrain]
  # Return output (note: "select(one_of" returns just the specified columns)
    return(select(outFrame,
                  one_of(c('siteID','date','year', 'site', 'species','count'))))}
}

# head(RichnessYearSubsetFrame(spatialGrain = 'site1',temporalGrain = 'year_season'))

#-------------------------------------------------------------------------------*
# ---- CALCULATE the Z-threshold ----
#-------------------------------------------------------------------------------*
# The Z-threshold refers to the maximum number of temporal subsamples that provide the most sites with greater than a minimum number of years of data. The following function returns this value.

# Note: Prior to running "zFinder", you must have already run the function "RichnessYearSubsetFrame" for which "inData" is the function's output.  

zFinder = function(inData, minNYears = 10, proportionalThreshold = .5){
  data = inData
  # Calculate the number of temporal samples per site and year: 
    spaceTime = ddply(data, .(siteID, year),
                      summarize, temporalSubsamples = length(unique(date)))
  
  # zPossible is a potential threshold of temporal subsampling:
    zPossible = sort(unique(spaceTime$temporalSubsamples))

  # Create an empty matrix to store summary data for possible Z-values:
    zMatrix = matrix(ncol = 3, nrow = length(zPossible), 
                     dimnames = list(NULL, c('z','nSites','propSites')))

  # Create an empty list of sites to store site names of good sites at a given Z-value:
    zSiteList = list(length = length(zPossible))

  # For loop to populate the zMatrix and zSite Lists:
  
    for(i in 1:length(zPossible)){
      # Calculate the years in which the subsamplings was greater than equal to z for a given site:
        siteYearsGTEz = ddply(filter(spaceTime, temporalSubsamples>=zPossible[i]),
                          .(siteID), summarize, 
                          yearsGTEz = length(unique(year)))
      # Determine sites with a minimum number of 10 years of sampling >= z
        goodSites = filter(siteYearsGTEz, yearsGTEz>=minNYears)$siteID
      # Construct matrix of z values, the number and proportion of sites:
        zMatrix[i,'z'] = zPossible[i]
        zMatrix[i, 'nSites'] = length(goodSites)
        zMatrix[i, 'propSites'] = length(goodSites)/length(unique(data$siteID))
      # List the names of goodSites for a given Z-value:
        zSiteList[[i]] = goodSites
      # Name each list entry by the Z-value
        names(zSiteList)[[i]] = zPossible[i]
      }

  # Get the highest Z value with at least minNYears:
    z = max(filter(data.frame(zMatrix), propSites >= proportionalThreshold)$z)
  
  # Get the names of the sites that satisfy Z:
   zSites = factor(zSiteList[[as.character(z)]])

  # Return the z value and site names 
    return(list (z = z, zSites = zSites, zTable = data.frame(zMatrix)))
}

#-------------------------------------------------------------------------------*
# ---- CALCULATE the W-threshold, subset data ----
#-------------------------------------------------------------------------------*
# The W-threshold refers to the maximum number of spatial subsamples that provide a given proprtion of siteYears.

# Note: Prior to running "wzDataSubset", you must have already run the function "RichnessYearSubsetFrame" (inData) and the "zFinder" (zOutput).  

wzDataSubset = function(inData, zOutput, minNYears = 10, proportionalThreshold = .5){
  #-Z-#
    data = inData
    z = zOutput[[1]]
  
  # Add a siteYear column:
    data$siteYear = paste(data$siteID, data$year, sep ='_')
  
  # Subset data to just the sites that meet the z-threshold for each site:
    dataZSiteSub = filter(data, siteID %in% zOutput$zSites)
  
  # Subset data to years that meet the z-threshold:
    siteYearDates = distinct(select(dataZSiteSub, one_of(c('siteYear','date'))))
    siteYearSummary = ddply(siteYearDates, .(siteYear), summarize, 
                      temporalSubsamples = length(unique(date)))
    siteYearZYearSub = filter(siteYearSummary, temporalSubsamples>=z)
    dataZYearSub = filter(dataZSiteSub, siteYear %in% siteYearZYearSub$siteYear)
  
  # Add a column that concatenates site, year, and date:
    dataZYearSub$siteYearDate = paste(dataZYearSub$siteYear, dataZYearSub$date, sep = '_')
  
  # For each siteID and year, sample z number of sampling events:
    siteYears = siteYearZYearSub$siteYear
    events = list(length = z*length(siteYears))
  
    for(i in 1:length(siteYears)){
      # Subset to a given siteYear:
        siteYearSub = filter(dataZYearSub, siteYear == siteYears[i])
      # Get unique frame of siteYearDates
        siteYearDates = distinct(data.frame(siteYearDate = siteYearSub$siteYearDate))
      # Sample the events by the Z-value:
        events[[i]] = sample_n(siteYearDates, size = z, replace = F)$siteYearDate
    }
  
  # Subset data to sampled events:
  
  sampledEvents = unlist(events)
  
  dataZSub = filter(dataZYearSub, siteYearDate %in% sampledEvents)

  #-W-#
  # Summarize data by the number of temporal samples in a given year:
    dataZSub$siteYearDate = paste(dataZSub$siteYear, dataZSub$date, sep ='_')
    spaceTime = ddply(dataZSub, .(siteYearDate), summarize, 
                           spatialSubsamples = length(unique(site)))
  
  # Determine the number of siteYears present:
    nSiteYearDates = nrow(spaceTime)
  
  # Get possible values for w:
    wPossible = sort(unique(spaceTime$spatialSubsamples))
  
  # Create an empty matrix to store summary data for possible W-values:
    wMatrix = matrix(ncol = 3, nrow = length(wPossible), 
                   dimnames = list(NULL, c('w','nSiteYearDates','propSiteYearDates')))
  
  # Create an empty list of sites to store site names of good sites at a given W-value:
    wSiteYearDateList = list(length = length(wPossible))
  
  # For loop to populate the wMatrix and wSite Lists:
  
    for(i in 1:length(wPossible)){
      # Calculate the years in which the subsamplings was greater than equal to w for a given site:
        siteYearDateGTEw = filter(spaceTime, spatialSubsamples>=wPossible[i])$siteYearDate
      # Construct matrix of w values, the number and proportion of sites:
        wMatrix[i,'w'] = wPossible[i]
        wMatrix[i, 'nSiteYearDates'] = length(siteYearDateGTEw)
        wMatrix[i, 'propSiteYearDates'] = length(siteYearDateGTEw)/nrow(spaceTime)
      # List the names of siteYears for a given W-value:
      wSiteYearDateList[[i]] = siteYearDateGTEw
      # Name each list entry by the Z-value
      names(wSiteYearDateList)[[i]] = wPossible[i]
    }
  
  # Get the highest W value that includes >= .5 of siteYears:
  
    wFrame = data.frame(wMatrix)
  
    w = max(filter(data.frame(wMatrix), propSiteYearDates >= proportionalThreshold)$w)
  
  # Get the names of the siteYearDates that satisfy W:
    wSiteYearDates = factor(wSiteYearDateList[[as.character(w)]])
  
  # Subset data
  
    dataW = filter(dataZSub, siteYearDate %in% wSiteYearDates)
  
  # For each siteYearDate, sample w sampling events:
    siteYearDateNames = unique(dataW$siteYearDate)
    events = list(length = w*length(siteYearDateNames))
    
    for(i in 1:length(siteYearDateNames)){
      siteYearDateSub = filter(dataW, siteYearDate == siteYearDateNames[i])
#       UniqueSiteYearDate = distinct(select(siteYearDateSub, one_of('site')))
#       sampledSite = sample_n(UniqueSiteYearDate, w, replace = F)$site
      UniqueSubsites = unique(siteYearDateSub$site) 
      sampledSubsites = sample(UniqueSubsites, w, replace = F)
      events[[i]] = filter(siteYearDateSub, site %in% sampledSubsites)
    }
  
    outSampledData = rbind.fill(events)
  
  # Keep only pertinent columns:
  
  outData = select(outSampledData, one_of(c('siteID', 'year','species', 'count')))
  
  # Return the w,z values and dataframe
  
  return(list (data = outData, w = w, z = z))
}


propOccFun = function(datasetID, spatialGrain, temporalGrain, minNYears = 10, minSpRich = 10){
  inData = RichnessYearSubsetFrame(spatialGrain = spatialGrain, temporalGrain = temporalGrain)
  zOutput = zFinder(inData)
  dataset = wzDataSubset(inData, zOutput)$data
  spTime = ddply(dataset, .(siteID, species), summarize, 
                 spTime = length(unique(year)))
  siteTime = ddply(dataset, .(siteID), summarize, 
                   siteTime = length(unique(year)))
  spSiteTime = merge(spTime, siteTime)
  propOcc = data.frame(datasetID = datasetID, site = spSiteTime$siteID, 
                       species = spSiteTime$species,
                       propOcc = spSiteTime$spTime/spSiteTime$siteTime)
  return(propOcc)
}

names(nestedDataset[[1]])


head(propOccFun(223, spatialGrain = 'site_block',temporalGrain = 'year_season'))

