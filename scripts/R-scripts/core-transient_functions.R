###################################################################################*
# CORE-TRANSIENT FUNCTIONS                                                         *
###################################################################################*
# This script contains all of the functions used in the analyses that summarize
# core-transient data by site (and across sites).

#======================================================================================================*
# ---- GENERAL FUNCTIONS ----
#======================================================================================================*

# Standard error:

se = function(x) sd(x)/sqrt(length(x))

#======================================================================================================*
# ---- FUNCTIONS FOR DATA FORMATTING ----
#======================================================================================================*

# This function modifies a value in the data formatting table for a 
# specific field:

dataFormattingTableFieldUpdate = function(datasetID, Field, Value){
  rowIndex = which(dataFormattingTable$dataset_ID == datasetID)
  if (is.factor(dataFormattingTable[,Field])) {
    dataFormattingTable[,Field] = as.character(dataFormattingTable[,Field])
    dataFormattingTable[rowIndex, Field] = Value
    dataFormattingTable[,Field] = factor(dataFormattingTable[,Field])
  } else {dataFormattingTable[,Field] = factor(dataFormattingTable[,Field])}
  return(dataFormattingTable[,Field])
}

# This function modifies numeric summary values:

dataFormattingTableUpdate = function(datasetID, datasetFinal){
  rowIndex = which(dataFormattingTable$dataset_ID == datasetID)
  dataFormattingTable[,'Raw_nRecs'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Raw_nRecs',
                                   nrow(dataset))
  dataFormattingTable[,'Raw_nTime'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Raw_nTime',
                                   length(unique(dataset1$date)))
  dataFormattingTable[,'Raw_nSpecies'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Raw_nSpecies', 
                                   length(unique(dataset1$species)))
  dataFormattingTable[,'Raw_nSites'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Raw_nSites', 
                                   length(unique(dataset2$site)))
  if(dataFormattingTable[rowIndex, 'countFormat'] == 'count'){
    if(dataFormattingTable[rowIndex, 'subannualTgrain'] == 'Y'){
      datasetFinal$date = as.numeric(format(datasetFinal$date, '%Y'))
    }
    siteYearCounts = ddply(datasetFinal, .(site, date), 
                           summarize, tCount = sum(count))
    dataFormattingTable[,'Raw_Mean_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Raw_Mean_Individuals_perSiteYear', 
                                     mean(siteYearCounts$tCount))
    dataFormattingTable[,'Raw_Min_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Raw_Min_Individuals_perSiteYear', 
                                     min(siteYearCounts$tCount))
    dataFormattingTable[,'Raw_Max_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Raw_Max_Individuals_perSiteYear', 
                                     max(siteYearCounts$tCount))
    siteYearCounts = ddply(datasetFinal, .(site, date), 
                           summarize, tCount = sum(count))
    dataFormattingTable[,'Formatted_Mean_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Formatted_Mean_Individuals_perSiteYear', 
                                     mean(siteYearCounts$tCount))
    dataFormattingTable[,'Formatted_Min_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Formatted_Min_Individuals_perSiteYear', 
                                     min(siteYearCounts$tCount))
    dataFormattingTable[,'Formatted_Max_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Formatted_Max_Individuals_perSiteYear', 
                                     max(siteYearCounts$tCount))
  } else {
    dataFormattingTable[,'Raw_Mean_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Raw_Mean_Individuals_perSiteYear','NA')
    dataFormattingTable[,'Raw_Min_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Raw_Min_Individuals_perSiteYear','NA')
    dataFormattingTable[,'Raw_Max_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Raw_Max_Individuals_perSiteYear','NA')
    dataFormattingTable[,'Formatted_Mean_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Formatted_Mean_Individuals_perSiteYear','NA')
    dataFormattingTable[,'Formatted_Min_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Formatted_Min_Individuals_perSiteYear','NA')
    dataFormattingTable[,'Formatted_Max_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Formatted_Max_Individuals_perSiteYear','NA') 
  }
  dataFormattingTable[,'Formatted_nRecs'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Formatted_nRecs',
                                   nrow(datasetFinal))
  dataFormattingTable[,'Formatted_nSpecies'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Formatted_nSpecies', 
                                   length(unique(datasetFinal$species)))
  dataFormattingTable[,'Formatted_nSites'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Formatted_nSites', 
                                   length(unique(datasetFinal$site)))
  return(dataFormattingTable)
}

#======================================================================================================*
# ---- FUNCTIONS for making proportional occurrence dataframes ----
#======================================================================================================*

#------------------------------------------------------------------------------------------------------*
# The following functions are used for temporally or spatially nested data
#======================================================================================================*

#------------------------------------------------------------------------------------------------------*
# Function to round dataset to lat and long and summarize data by the new rounded values:
#------------------------------------------------------------------------------------------------------*
datasetRoundLatLong = function(dataset, accuracy){
  # Split LL column into a dataframe of lat and long:
  siteLL = data.frame(do.call(rbind, strsplit(t, '_' )))
  # Round to chosen accuracy:
  roundLL = function(LatORLong){
    LatorLongVector = as.numeric(as.character(siteLL[,LatORLong]))
    return(round_any(LatorLongVector, accuracy,  f = floor))
  }
  lat = roundLL(1)
  long = roundLL(2)
  # Paste to a new site vector and return dataset:
  dataset$analysisSite = paste(lat, long, sep = '_')
  return(dataset)  
}

#------------------------------------------------------------------------------------------------------*
# Function to summarize data to a given site level:
#------------------------------------------------------------------------------------------------------*
getNestedSiteDataset = function(dataset, siteGrain = 'site'){
  # If sites are not nested (lat-long or categorical):
  if(dataFormattingTable$spatial_scale_variable == 'N'){
    dataset$analysisSite = dataset$site
    return(dataset)
  } else {
    # If sites are defined by lat-longs:
    if(dataFormattingTable$LatLong_sites == 'Y')
    {dataset = datasetRoundLatLong(dataset, accuracy = siteGrain)
     return(dataset)} else {
       # If sites are categorical but nested ...
       # Get the definition for a site and store each level as separate columns:
       siteLevels = strsplit(siteLevel, '_')[[1]]
       # Convert site data to a table and add names based on site definition:
       siteTable = read.table(text = as.character(dataset$site), sep = '_', stringsAsFactors = F)
       siteDefinition = dataFormattingTable$Raw_siteUnit
       names(siteTable) = strsplit(as.character(siteDefinition), '_')[[1]]
       # Get pre-determined site levels and maintain site based on match: 
       siteLevels = strsplit(siteGrain, '_')[[1]]
       dataset$analysisSite = do.call('paste', c(siteTable[siteLevels], sep = '_'))
       return(dataset)  
     }}
}

#------------------------------------------------------------------------------------------------------*
# Nested time dataset (spatial nesting is categorical, not lat-long):
#------------------------------------------------------------------------------------------------------*
getNestedTimeDataset = function(dataset,  temporalGrain){
  if(dataFormattingTable$subannualTgrain == 'Y'){
    dataset$date = as.POSIXct(strptime(dataset$date, '%Y-%m-%d'))
    year = as.numeric(format(dataset$date, '%Y'))
    day = as.numeric(strftime(dataset$date, format = '%j'))
    week = trunc(day/7)+1
    biweek = trunc(week/2)+1
    month = as.numeric(format(dataset$date, '%m'))
    bimonth = trunc(month/2)+1
    season = ifelse(day < 80 |day >= 356, 1,
                    ifelse(day >= 80 & day < 172, 2,
                           ifelse(day >= 172 & day < 266, 3, 4)))
    # Combine time data into a dataframe:
    timeFrame = cbind(day, week, biweek, month, bimonth, season, year)
    #   # Add analysisDate column at a given temporal grain and year:
    dataset$analysisDate = paste(year, timeFrame[,temporalGrain], sep ='_')
    dataset$year = year
    # Summarize data to the new time scale:
  } else {
    dataset$analysisDate = dataset$date
    dataset$year = dataset$date
  }
  return(dataset)
}

#------------------------------------------------------------------------------------------------------*
# Wrapper function for nested data (if necessary):
#------------------------------------------------------------------------------------------------------*
getNestedDataset = function(dataset, siteGrain, temporalGrain){
  datasetSpace = getNestedSiteDataset(dataset, siteGrain)
  datasetTime = getNestedTimeDataset(datasetSpace, temporalGrain)
  return(datasetTime)
}

#------------------------------------------------------------------------------------------------------*
# ---- SUBSET DATASET TO SITES WITH ADEQUATE TIME SAMPLES AND RICHNESS ----
#======================================================================================================*

# Note: Prior to running "RichnessYearSubsetFrame", you must have created the nestedDataset using the function "getNestedDataset". The output of the getNestedDataset function is a list with the first list item being the dataset, expanded across all potential spatial and temporal grains and the second being a vector of the names of the site columns. The "i" in this function refers to the the value in the vector of site names. 

richnessYearSubsetFun = function(dataset, spatialGrain, temporalGrain, 
                                 minNYears = 10, minSpRich = 10){
  dataset = getNestedDataset(dataset, spatialGrain, temporalGrain)
  # Get the number of years and species richness for each site: 
  siteSr_nTime = ddply(dataset, .(analysisSite), summarize,
                       sr = length(unique(species)), 
                       nTime = length(unique(year)))
  # Subset to sites with a high enough species richness and year samples:
  goodSites = filter(siteSr_nTime, sr >= minSpRich & 
                       siteSr_nTime$nTime >= minNYears)$analysisSite
  # If statement to return if there are no good sites:
  if(length(goodSites) == 0) {
    return(print('No acceptable sites, rethink site definitions or temporal scale'))}
  else {
    # Match good sites and the dataframe:
    outFrame = na.omit(dataset[dataset$site %in% goodSites,])
    return(outFrame)
  }}

#------------------------------------------------------------------------------------------------------*
# ---- CALCULATE the Z-threshold ----
#------------------------------------------------------------------------------------------------------*
# The Z-threshold refers to the maximum number of temporal subsamples that provide the most sites with greater than a minimum number of years of data. The following function returns this value.

# Note: Prior to running "zFinder", you must have already run the function "RichnessYearSubsetFrame" for which "inData" is the function's output.  

zFinder = function(inData, minNTime = 10, proportionalThreshold = .5){
  data = inData
  # Calculate the number of temporal samples per site and year: 
    spaceTime = ddply(data, .(analysisSite, year, analysisDate),
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
      # Subset spaceTime to subsamples greater than or equal to z for a given site:
        spaceTimeGTEz = filter(spaceTime, temporalSubsamples>=zPossible[i])
      # Determine sites in which the temporal subsampling was greater than equal to z for at least the minimum time samples:
        goodSites = ddply(spaceTimeGTEz, .(analysisSite), summarize, 
                              length(unique(analysisDate) >= minNTime))$analysisSite
      # Construct matrix of z values, the number and proportion of sites:
        zMatrix[i,'z'] = zPossible[i]
        zMatrix[i, 'nSites'] = length(goodSites)
        zMatrix[i, 'propSites'] = length(goodSites)/length(unique(spaceTime$analysisSite))
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

#------------------------------------------------------------------------------------------------------*
# ---- CALCULATE the W-threshold, subset data ----
#------------------------------------------------------------------------------------------------------*
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


propOccFun = function(datasetID, spatialGrain = 'site', temporalGrain = 'year', minNYears = 10, minSpRich = 10){
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


###################################################################################

#------------------------------------------------------------------------------------------------------*
# Function to change date object to year:
#------------------------------------------------------------------------------------------------------*

getYear = function(date){
  if (class(date)[1] == 'factor') date = as.POSIXlt(date)
  return(as.numeric(format(date, '%Y')))
}

#------------------------------------------------------------------------------------------------------*
# The following function is used to create and explore and extract the species richness and number of time samples for a site.
#------------------------------------------------------------------------------------------------------*


siteSummaryFun = function(dataset){
  ddply(dataset, .(datasetID, site), summarize, 
        spRich = length(unique(species)), 
        nTime = length(unique(year)))
}



########################################################################################################
#------------------------------------------------------------------------------------------------------*
# Function to evaluate spatial and temporal sampling grain:
#------------------------------------------------------------------------------------------------------*

wzMaker = function(i, minNYears = 10, proportionalThreshold = .2){
  
  siteID = nestedDataset[[2]][i]
  nestedDatasetDf = nestedDataset[[1]]
  nestedDatasetDf$siteID = nestedDatasetDf[,siteID]
  
  # Subset to sites with a high enough species richness and year samples:
  
  siteSr_nTime = ddply(nestedDatasetDf, .(siteID), summarize,
                       sr = length(unique(species)), 
                       nTime = length(unique(year)))
  
  goodSites = subset(siteSr_nTime, sr >= 10 & siteSr_nTime$nTime >= minNYears)$siteID
  
  d1 = nestedDatasetDf[nestedDatasetDf$siteID %in% goodSites,]
  
  # Get data frame of the number of spatial and temporal samples by site and year:
  
  spaceTime = ddply(d1,.(siteID, year), summarize,
                    spatialSubsamples = length(unique(site)),
                    temporalSubsamples = length(unique(date)))
  
  # Summarize, counting the length of years for a given site having a given number of spatial and temporal subsamples.
  
  spaceTimeSummary = ddply(spaceTime,.(siteID, spatialSubsamples, temporalSubsamples),
                           summarize, years = length(year))
  
  # Get potential values for w and z:
  
  w = seq(min(spaceTime$spatialSubsamples), max(spaceTime$spatialSubsamples, by  = 1))
  z = seq(min(spaceTime$temporalSubsamples), max(spaceTime$temporalSubsamples, by  = 1))
  #wz = expand.grid(w = w, z = z)
  
  wz = distinct(expand.grid(w = spaceTime$spatialSubsamples, z = spaceTime$temporalSubsamples))
  
  # Out
  outList = list(length = nrow(wz))
  for(i in 1:nrow(wz)){
    w = wz[i,1]
    z = wz[i,2]
    # Calculate the sum of w and z values relative to the max values of each:
    wzScaledSum = w/max(wz$w) + z/max(wz$z)
    # For each site and value of w and z, count the number of years sampled with time and spatial samples greater than or equal to the values of w and z:
    wzSiteYearSum = ddply(subset(spaceTime, spatialSubsamples>=w & temporalSubsamples>=z),
                          .(siteID), summarize, years = length(year))
    # Determine the proportion of sites greater than the minimum number of years:
    wzSiteProp = ifelse(nrow(wzSiteYearSum) == 0, 0, sum(wzSiteYearSum[,2]>=minNYears)/length(goodSites))
    # Bind output
    outList[[i]] = cbind(wz[i,], wzScaledSum, wzSiteProp)
  }
  
  wzSiteSummary = rbind.fill(outList)
  
  # Subset to max w z values for site proportions greater than .2
  
  wzMax = subset(subset(wzSiteSummary, wzSiteProp >=proportionalThreshold), wzScaledSum == max(wzScaledSum))
  
  wz = subset(wzMax, wzSiteProp == max(wzSiteProp))[,1:2]
  
  wzMakerOutList = list(spaceTimeSummary, wzSiteSummary, wz)
  names(wzMakerOutList) = c('spaceTimeSummary', 'wzSiteSummary', 'wzMax')
  return(wzMakerOutList)
}

#------------------------------------------------------------------------------------------------------*
# The following function writes the proportional occurence data frame on sites.
#------------------------------------------------------------------------------------------------------*

propOccFun = function(dataset){
  spTime = ddply(dataset, .(datasetID, site, species), summarize, 
                 spTime = length(unique(year)))
  siteTime = ddply(dataset, .(datasetID, site), summarize, 
                   siteTime = length(unique(year)))
  propOcc = merge(spTime, siteTime)
  propOcc$propOcc = propOcc$spTime / propOcc$siteTime
  return(propOcc[,-c(4:5)])
}

#------------------------------------------------------------------------------------------------------*
# ---- FUNCTIONS for proportional occurrence and site summary data frames  ----
#======================================================================================================*

nestedSiteValidity = function(dataset, i){
  siteUnit = paste(as.character(siteUnitTable[1,1:i]), collapse = '_')
  dataset$year = getYear(dataset$date)
  if (siteUnit == siteUnitTable[,1]) {
    dataset$site = siteTable[,1]} else {
      dataset$site = factor(apply(siteTable[,1:i], 1, paste, collapse = '_'))
    } 
  siteSummary = ddply(dataset, .(site), summarize,
                      timeSamples = length(unique(year)), 
                      nSpecies = length(unique(species)))
  nSite = nrow(siteSummary)
  MEANnTime = mean(siteSummary$timeSamples)
  MINnTime = min(siteSummary$timeSamples)
  MAXnTime = max(siteSummary$timeSamples)
  STDEVnTime = sd(siteSummary$timeSamples) 
  nBadSiteTime = nrow(subset(siteSummary, timeSamples < 5))
  nBadSiteSpecies = nrow(subset(siteSummary, nSpecies < 10))
  nBadSites = nrow(subset(siteSummary, timeSamples < 5 | nSpecies < 10))
  propBadSiteTime = nBadSiteTime/nRecs
  propBadSiteSpecies = nBadSiteSpecies/nRecs
  propBadSites = nBadSiteSpecies/nRecs
  return(data.frame(siteUnit, nSite, 
                    MEANnTime, MINnTime, MAXnTime, STDEVnTime,
                    nBadSiteTime,nBadSiteSpecies,nBadSites,
                    propBadSiteTime, propBadSiteSpecies, propBadSites))
}

# Function to calculate site validity across scales for nested sites:

nestedSiteValiditySummary = function(dataset){
  siteUnit = dataFormattingTable$Raw_siteUnit
  siteUnitTable = read.table(text = as.character(siteUnit), sep = '_', stringsAsFactors = F)
  siteTable = read.table(text = as.character(dataset$site), sep = '_', stringsAsFactors = F)
  outList = list(length = ncol(siteTable))
  for (i in 1:ncol(siteTable)){
    outList[[i]] = nestedSiteValidity(dataset, i)
  }
  return(rbind.fill(outList))
}


#======================================================================================================*
# ---- GET DATA ----
#======================================================================================================*

# The following function reads in the data and returns a list of the proportional 
# occurence data frame, the site summary (sp richness and number of time samples
# for a given site), system, and taxa:

getDataList = function(datasetID){
  propOcc = read.csv(paste('data/propOcc_datasets/propOcc_', 
                           datasetID, '.csv', sep = ''))
  siteSummary = read.csv(paste('data/siteSummaries/siteSummary_', 
                               datasetID, '.csv',  sep = ''))
  metaData = subset(read.csv('data_source_table.csv'),
                    dataset_ID == datasetID)
  system = metaData$system
  taxa = metaData$taxa
  return(list(propOcc = propOcc, siteSummary = siteSummary, 
              system = system, taxa = taxa))
}

#======================================================================================================*
# ---- BIMODALILITY ----
#======================================================================================================*
# NOTE: For these functions to run, occProp, Ntime, and outSummary frames must
# already be loaded and the "Sampling summary" lines of code MUST be run in the 
# dashboard!
#
# Functions:
# - bimodality: Calculates the bimodality metric developed by Allen and Ethan. 
#     Inputs: Site
#     Outputs: A single numeric bimodality value
#
# - random.bimodality: The bimodality for a random sample of the dataset.
#     Inputs: Site
#     Outputs: A single numeric bimodality value
#
# - p.bimodal: Randomization test for bimodality. Runs n-reps of the random.
#     bimodality function and compares the actual bimodality with the 
#     distribution of random values.
#     Inputs: Site, number of reps
#     Outputs: A single numeric p-value.
#
# - occs.scaled: Scales occupancy from [0,1] to (0,1) -- because beta distribution
#     inputs must not contain 0's or 1's.
#     Inputs: Site
#     Outputs: A numeric vector of scaled occupancy values.
# 
# - fitbeta: Calculates the shape parameters for a fitted beta distribution.
#     Inputs: Site
#     Outputs: A vector of shape parameters (alpha and beta).
# 
#------------------------------------------------------------------------------------------------------*
# ---- Function for calculating bimodality ----
#======================================================================================================*
# Note 1: Bimodality is the fraction of species occurring at either end of 
# occupancy distribution. We use a randomization approach to test whether the 
# distribution is significantly bimodal.
# Note 2: To run this function the number of time samples for the site (nt) needs
# to be specified. This is done so in the wrapper summary table function.


bimodalityFun = function(propOcc_or_RandomPropOcc, nTime){
  occs = propOcc_or_RandomPropOcc
  maxvar = var(c(rep(1/nTime,floor(length(occs)/2)),
                 rep(1,ceiling(length(occs)/2))))
  return(var(occs)/maxvar)
}

# Random sample of occurences for a given site (to be used in randomization, below):

randomOccsFun = function(propOcc, nTime){
  # Generate a table (data frame) of occProps and frequencies:
  occPropTable = data.frame(table(propOcc))
  # Create a data frame of possible occProps:
  occPropDummyTable = data.frame(propOcc = seq(1/nTime, 1, by = 1/nTime))
  # Merge the two data frames:
  combinedTable = merge(occPropDummyTable, occPropTable, all.x = T)
  combinedTable[is.na(combinedTable[,2]),2]<-0                            # Replace NA's with zeros
  # Reassign bin values randomly and add to frame:
  newFreq = sample(combinedTable$Freq, length(combinedTable[,1]))
  randomTable = data.frame(combinedTable[,1], newFreq)
  randomOccs=unlist(apply(randomTable, 1, function(x) rep(x[1], x[2])))
  return(as.vector(randomOccs))
}

# Randomization test for bimodality:

pBimodalFun = function(propOcc,nTime, reps){
  actualBimod = bimodalityFun(propOcc, nTime)
  # For loop to get random bimodality values
  randomBimod = numeric(length = reps)
  for (i in 1:reps){
    randomBimod[i] = bimodalityFun(randomOccsFun(propOcc, nTime), nTime)
  }
  # Calculate the p-value (proportion of sites with higher bimodality than the
  # actual bimodality value):
  sum(randomBimod >= actualBimod)/(reps + 1)
}

#------------------------------------------------------------------------------------------------------*
# ---- Function for fitting the beta distribution ----
#======================================================================================================*
# Required packages = MASS

# Scale occupancy from [0,1] to (0,1) following Smithson and Verkuilen 2006
# Note: See supplemental at
# http://supp.apa.org/psycarticles/supplemental/met_11_1_54/met_11_1_54_supp.html

occsScaledFun = function(occProp){
  x = occProp# [as.character(occProp$site) == site,'occ']
  n = length(x)
  s = .5
  (x*(n-1)+s)/n
}

# Fit beta distribution:

fitBeta = function(occProp, nTime) {
  if (bimodalityFun(occProp,nTime)!= 0)
  {occs  = occsScaledFun(occProp)
   shape.params = suppressWarnings(fitdistr(occs, "beta",
                                            list(shape1 = 2, shape2 = 2)))
   return(as.vector(shape.params$estimate))
  } else c(NA, NA)
}

#======================================================================================================*
# ---- CORE-TRANSIENT MODE STATISTICS ----
#======================================================================================================*

# Proportion of samples that are core or transient:

modeProp = function(propOcc, mode, threshold) {
  if (mode == 'core') length(propOcc[propOcc >= 1-propOcc])/length(propOcc)
  else length(propOcc[propOcc <= threshold])/length(propOcc)
}

# Randomization test for a given mode (is the proportion of samples in core or
# transient greater than we would expect by random chance):

pModeFun = function(propOcc, nTime, mode, threshold, reps){
  actualProp = modeProp(propOcc, mode, threshold)
  # For loop to get random frequncies in the mode:
  randomProps = numeric(length = reps)
  for (i in 1:reps){
    randomProps[i] = modeProp(randomOccsFun(propOcc, nTime), mode, threshold)
  }
  # Calculate the p-value (proportion of sites with higher frequency than the
  # actual bimodality value):
  pVal = sum(randomProps >= actualProp)/(reps + 1)
  return(pVal)
}

#======================================================================================================*
# ---- DATASET SUMMARY FUNCTIONS ----
#======================================================================================================*
# NOTE: For these functions to run, occProp, Ntime, and outSummary frames must
# already be loaded!
#
# Functions:
# - summaryStats: Produces summary sampling data for one site. 
#     Inputs: Site and the threshold value for core and transient designation.
#     Outputs: A one-row dataframe with dataset ID, site ID, threshold used,
#       the system, taxa, # of time samples, total, core, and transient richness
#       proportion of core and transient species, and the average proportion of 
#       occurance across species.
#
# - ctSummary: A partial-wrapper function that runs and compiles bimodality test
#     statistics across sites and adds it to the sampling summary frame above.
#     Inputs: Site and the threshold value for core and transient designation.
#     Outputs: A one-row dataframe with the summary output above and bimodality
#     (Allen + Ethan formula), randomization-derived p-value, and the alpha and
#     beta shape parameters for the beta distibution.
#
#------------------------------------------------------------------------------------------------------*
# ---- Function to generate summary of sampling ----
#======================================================================================================*

# Summary stats for all sites in a dataset:

summaryStatsFun = function(datasetID, threshold, reps){
  # Get data:
  dataList = getDataList(datasetID)
  sites  = dataList$siteSummary$site
  # Get summary stats for each site:
  outList = list(length = length(sites))
  for(i in 1:length(sites)){
    propOcc = subset(dataList$propOcc, site == sites[i])$propOcc
    siteSummary = subset(dataList$siteSummary, site == sites[i])
    nTime = siteSummary$nTime
    spRichTotal = siteSummary$spRich
    spRichCore = length(propOcc[propOcc >= 1 - threshold])
    spRichTrans = length(propOcc[propOcc <= threshold])
    propCore = spRichCore/spRichTotal
    propCore_pVal = pModeFun(propOcc, nTime, 'core', threshold, reps)
    propTrans = spRichTrans/spRichTotal
    propTrans_pVal = pModeFun(propOcc, nTime, 'trans', threshold, reps)
    mu = mean(propOcc)
    bimodality = bimodalityFun(propOcc, nTime)
    pBimodal = pBimodalFun(propOcc, nTime, reps)
    betaParms = fitBeta(propOcc, nTime)
    alpha = betaParms[1]
    beta = betaParms[2]   
    outList[[i]] = data.frame(datasetID, site = sites[i],
                              system = dataList$system, taxa = dataList$taxa,
                              nTime, spRichTotal, spRichCore, spRichTrans,
                              propCore, propCore_pVal,  propTrans, propTrans_pVal,
                              mu, bimodality, pBimodal, alpha, beta)
  }
  return(rbind.fill(outList))
}

#------------------------------------------------------------------------------------------------------*
# ---- MAKE SUMMARY STATS OF ANY NEW PROPOCC FILES ----
#======================================================================================================*

addNewSummariesFun = function(threshold, reps){
  currentSummaryData = read.csv('output/tabular_data/core-transient_summary.csv')
  currentDatasetIDs = unique(currentSummaryData$datasetID)
  propOcc_datasets = list.files('data/propOcc_datasets')
  # The following gets the integer values for the datasetID's from
  # "propOcc_##.csv" or "propOcc_###.csv":
  propOccDatasetIDs = read.table(text = 
                                   as.character(read.table(text = propOcc_datasets,
                                                           sep =c('_'))[,2]),sep ='.')[,1]
  # Find dataset IDs that are not yet summarized:
  newDatasetIDs = propOccDatasetIDs[!propOccDatasetIDs %in% currentDatasetIDs]
  # For loop to extract summary stats for new datasetIDs
  outList = list(length = length(newDatasetIDs))
  for(i in 1:length(newDatasetIDs)){
    outList[[i]] = summaryStatsFun(newDatasetIDs[i], threshold, reps)
  }
  newSummaryData = rbind.fill(outList)
  updatedSummaryData = rbind(currentSummaryData, newSummaryData)
  return(updatedSummaryData[order(datasetID),])
}

###################################################################################*
# ---- UPDATE COMPLETE TO THIS POINT ----
###################################################################################*

#======================================================================================================*
# ---- PLOT FUNCTIONS ----
#======================================================================================================*
# NOTE: For these functions to run, occProp, Ntime, and outSummary frames must
# already be loaded!

#------------------------------------------------------------------------------------------------------*
# ---- Custom themes ----
#======================================================================================================*

# Theme for plot with no background grid:

theme_CT_NoGrid = function(base_size = 12) {
  theme(
    axis.text.x = element_text(size=14, color = 'black',vjust = 1, hjust = .5),
    axis.text.y = element_text(size=12, color = 'black', hjust = 1),
    axis.title.x = element_text(size = 18, vjust = -1),
    axis.title.y = element_text(size = 18, vjust = 1.5),
    title = element_text(size=16, vjust = 1),
    legend.title=element_blank(),
    axis.line = element_line(color = 'black'),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(2,.5,1.5,.5), 'lines'))
}

theme_CT_Grid = function(base_size = 12) {
  theme(axis.text = element_text(size=14, color = 'black'),
        axis.title.x = element_text(size = 18, vjust = -1),
        axis.title.y = element_text(size = 18, vjust = 1),
        title = element_text(size=18, vjust = -0.5),
        axis.line = element_line(colour = 'black'),
        panel.background = element_blank(),
        panel.grid.major = element_line(size = .5, color = 'gray90'),
        panel.grid.minor = element_line(size = .25, color = 'gray90'),
        plot.margin = unit(c(0,.5,1.5,.5), 'lines'))
}

#------------------------------------------------------------------------------------------------------*
# ---- Function to make core-transient histogram  ----
#======================================================================================================*
# This function creates a ct histogram for one site:

ct.hist = function(site) {
  # Get data, subset to a given site:
  occProp = occProp[as.character(occProp$site) == site,]
  ct = ct[as.character(ct$site) == site, ]
  # Plot labels:
  main = paste('Site ', site, paste('(',  as.character(ct$system),
                                    ', ', as.character(ct$taxa),')', sep = ''))
  sub = bquote(b ~ '=' ~ .(round(ct$bimodal, 2)) ~ '    '~
                 P['b'] ~ '=' ~ .(round(ct$bimodal.p, 3)) ~ '    '~
                 mu ~ '=' ~ .(round(ct$mu, 2)) ~ '    '~
                 t ~ '=' ~ .(ct$nTime))
  sub2 = bquote(alpha ~ '=' ~ .(round(ct$alpha, 3)) ~ '    '~
                  beta ~ '=' ~ .(round(ct$beta, 3)))
  # Set band width, breaks and possible values of x for the histogram:
  bw = 1/nTime#(max(occProp$occ)-min(occProp$occ))/10
  brks = seq(min(occProp$occ), max(occProp$occ),bw)
  x = seq(1/ct$nTime,1-1/ct$nTime, .01)
  beta.df = data.frame(x = x, y = dbeta(x, ct$alpha, ct$beta))
  # Plot data: 
  out.plot = ggplot(occProp, aes(x=occ)) +
    geom_histogram(aes(y = ..density..), binwidth = bw, breaks = brks, right = F,
                   fill = 'gray', color = 1) +
    xlim(1/nTime, 1) +
    geom_line(data = beta.df, aes(x = x, y = y), color = 'red') +
    # stat_function(fun = function(x) dbeta(x, ct$alpha, ct$beta), color = 'red') +
    # Add labels:
    xlab('Proportion of temporal samples') + ylab('Density') + 
    ggtitle(bquote(atop(.(main), atop(.(sub), atop(.(sub2)))))) +
    # Add themes:
    theme(axis.text = element_text(size=14, color = 1),
          axis.title.x = element_text(vjust = -1),
          axis.title.y = element_text(vjust = 2),
          title = element_text(size=16, vjust = -1),
          axis.line = element_line(colour = "black"),
          panel.background = element_blank(),
          plot.margin = unit(c(.5,.5,1.5,1), "lines"))
  return(out.plot)
}
