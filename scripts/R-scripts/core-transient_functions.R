#######################################################################################################*
# CORE-TRANSIENT FUNCTIONS                                                                             *
#######################################################################################################*
# This script contains all of the functions used in the analyses that summarize
# core-transient data by site (and across sites).

#======================================================================================================*
# ---- GENERAL FUNCTIONS ----
#======================================================================================================*

# Standard error:

se = function(x) sd(x)/sqrt(length(x))

# Function to change date object to year:

getYear = function(date){
  if (class(date)[1] == 'factor') date = as.POSIXlt(date)
  return(as.numeric(format(date, '%Y')))
}

#######################################################################################################*
# ---- DATA PREPARATION ----
#######################################################################################################*

#======================================================================================================*
# ---- FUNCTIONS FOR DATA FORMATTING ----
#======================================================================================================*

#------------------------------------------------------------------------------------------------------*
#  ---- Functions to modify a value in the data formatting table for a specific field ----
#------------------------------------------------------------------------------------------------------*

dataFormattingTableFieldUpdate = function(datasetID, Field, Value){
  rowIndex = which(dataFormattingTable$dataset_ID == datasetID)
  if (is.factor(dataFormattingTable[,Field])) {
    dataFormattingTable[,Field] = as.character(dataFormattingTable[,Field])
    dataFormattingTable[rowIndex, Field] = Value
    dataFormattingTable[,Field] = factor(dataFormattingTable[,Field])
  } else {
    dataFormattingTable[rowIndex, Field] = Value
  }
  return(dataFormattingTable[,Field])
}

# This function fills in numeric summary values for the cleaned raw dataset
# in the data formatting table.

dataFormattingTableUpdate = function(datasetID, datasetFinal){
  rowIndex = which(dataFormattingTable$dataset_ID == datasetID)
  year = as.numeric(substr(datasetFinal$date, 1, 4))
  dataFormattingTable[,'Raw_nRecs'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Raw_nRecs',
                                   nrow(datasetFinal))
  dataFormattingTable[,'Raw_nTime'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Raw_nTime',
                                   length(unique(datasetFinal$date)))
  dataFormattingTable[,'Raw_nSpecies'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Raw_nSpecies', 
                                   length(unique(datasetFinal$species)))
  dataFormattingTable[,'Raw_nSites'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Raw_nSites', 
                                   length(unique(datasetFinal$site)))
  dataFormattingTable[,'Raw_start_year'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Raw_start_year', 
                                   min(year))
  dataFormattingTable[,'Raw_end_year'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Raw_end_year', 
                                   max(year))
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
  } else {
    dataFormattingTable[,'Raw_Mean_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Raw_Mean_Individuals_perSiteYear','NA')
    dataFormattingTable[,'Raw_Min_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Raw_Min_Individuals_perSiteYear','NA')
    dataFormattingTable[,'Raw_Max_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Raw_Max_Individuals_perSiteYear','NA')
  }
  return(dataFormattingTable)
}


# This function fills in numeric summary values for the formatted dataset subsetted
# to standardized levels of spatial and temporal subsampling in the data formatting table.

dataFormattingTableUpdateFinished = function(datasetID, datasetFinal){
  rowIndex = which(dataFormattingTable$dataset_ID == datasetID)
  year = as.numeric(substr(datasetFinal$year, 1, 4))
  dataFormattingTable[,'Formatted_nRecs'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Formatted_nRecs',
                                   nrow(datasetFinal))
  dataFormattingTable[,'Formatted_nTime'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Formatted_nTime',
                                   length(unique(datasetFinal$year)))
  dataFormattingTable[,'Formatted_nSpecies'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Formatted_nSpecies', 
                                   length(unique(datasetFinal$species)))
  dataFormattingTable[,'Formatted_nSites'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Formatted_nSites', 
                                   length(unique(datasetFinal$site)))
  dataFormattingTable[,'Formatted_start_year'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Formatted_start_year', 
                                   min(year))
  dataFormattingTable[,'Formatted_end_year'] = 
    dataFormattingTableFieldUpdate(datasetID, 'Formatted_end_year', 
                                   max(year))
  if(dataFormattingTable[rowIndex, 'countFormat'] == 'count'){
    if(dataFormattingTable[rowIndex, 'subannualTgrain'] == 'Y'){
      datasetFinal$date = as.numeric(format(datasetFinal$date, '%Y'))
    }
    siteYearCounts = ddply(datasetFinal, .(site, year), 
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
    dataFormattingTable[,'Formatted_Mean_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Formatted_Mean_Individuals_perSiteYear','NA')
    dataFormattingTable[,'Formatted_Min_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Formatted_Min_Individuals_perSiteYear','NA')
    dataFormattingTable[,'Formatted_Max_Individuals_perSiteYear'] = 
      dataFormattingTableFieldUpdate(datasetID, 'Formatted_Max_Individuals_perSiteYear','NA')
  }
  return(dataFormattingTable)
}


#======================================================================================================*
# ---- FUNCTIONS for making proportional occurrence dataframes ----
#======================================================================================================*

#------------------------------------------------------------------------------------------------------*
# Function to round dataset to lat and long and summarize data by the new rounded values:
#------------------------------------------------------------------------------------------------------*

datasetRoundLatLong = function(dataset, accuracy){
  # Split LL column into a dataframe of lat and long:
  siteLL = data.frame(do.call(rbind, strsplit(as.character(dataset$site), '_' )))
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

getNestedSiteDataset = function(dataset, siteGrain, dataDescription){
  
  #NOTE THAT siteGrain CAN BE EITHER A CHARACTER STRING NAMING THE
  #CATEGORICAL SCALE FOR ANALYSIS, OR A NUMERIC VALUE INDICATING THE
  #SPATIAL SCALE IN DEGREES LAT-LONG.
  
  # If sites are not nested (lat-long or categorical):
  if(dataDescription$spatial_scale_variable == 'N'){
    dataset$analysisSite = dataset$site
    return(dataset)
  } else {
    # If sites are defined by lat-longs:
    if(dataDescription$LatLong_sites == 'Y')
    {dataset = datasetRoundLatLong(dataset, accuracy = siteGrain)
     return(dataset)} else {
       # If sites are categorical but nested ...
       # Get the definition for a site and store each level as separate columns:
      # siteLevels = strsplit(siteGrain, '_')[[1]]
       # Convert site data to a table and add names based on site definition:
       siteTable = read.table(text = as.character(dataset$site), sep = '_',
                              quote = '\"', stringsAsFactors = F)
       siteDefinition = dataDescription$Raw_siteUnit
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

getNestedTimeDataset = function(dataset,  temporalGrain, dataDescription){
  if(temporalGrain != 'year'){ # if analysis will be performed at fine temporal grain
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
  } else if (class(dataset$date)[1] == 'numeric') { # if analysis will be performed at annual resolution
    dataset$analysisDate = dataset$date
    dataset$year = dataset$date
  } else { 
    dataset$analysisDate = as.numeric(format(dataset$date, "%Y"))
    dataset$year = as.numeric(format(dataset$date, "%Y"))
  }
  return(dataset)
}

#------------------------------------------------------------------------------------------------------*
# Wrapper function for nested data (if necessary):
#------------------------------------------------------------------------------------------------------*

getNestedDataset = function(dataset, siteGrain, temporalGrain, dataDescription){
  datasetSpace = getNestedSiteDataset(dataset, siteGrain, dataDescription)
  datasetTime = getNestedTimeDataset(datasetSpace, temporalGrain, dataDescription)
  return(datasetTime)
}

#------------------------------------------------------------------------------------------------------*
# ---- SUBSET DATASET TO SITES WITH ADEQUATE TIME SAMPLES AND RICHNESS ----
#======================================================================================================* 

richnessYearSubsetFun = function(dataset, spatialGrain, temporalGrain, minNTime = 10, minSpRich = 10, dataDescription){
    dataset1 = getNestedDataset(dataset, spatialGrain, temporalGrain, dataDescription)
  # Get the number of years and species richness for each site: 
    siteSr_nTime = ddply(dataset1, .(analysisSite), summarize,
                         sr = length(unique(species)), 
                         nTime = length(unique(analysisDate)))
  # Subset to sites with a high enough species richness and year samples:
    goodSites = siteSr_nTime$analysisSite[siteSr_nTime$sr >= minSpRich & 
                                          siteSr_nTime$nTime >= minNTime]
  # If statement to return if there are no good sites:
    if(length(goodSites) == 0) {
      return(print('No acceptable sites, rethink site definitions or temporal scale'))}
    else {
      # Match good sites and the dataframe:
      outFrame = na.omit(dataset1[dataset1$analysisSite %in% goodSites,])
      return(outFrame)
    }}

#------------------------------------------------------------------------------------------------------*
# ---- CALCULATE the Z-threshold ----
#------------------------------------------------------------------------------------------------------*
# The Z-threshold refers to the maximum number of temporal subsamples that provide the most sites with greater than a minimum number of years of data. The following function returns this value.

# Note: Prior to running "zFinder", you must have already run the function "richnessYearSubsetFun" for which "inData" is the function's output.  

zFinder = function(inData, minNTime = 10, proportionalThreshold = .5){
  # Calculate the number of temporal samples per site and year: 
    spaceTime = ddply(inData, .(analysisSite, analysisDate),
                    summarize, temporalSubsamples = length(unique(date)))
    spaceTime$siteTime = paste(spaceTime$analysisSite, spaceTime$analysisDate, sep = '_')
  # zPossible is a potential threshold of temporal subsampling:
    zPossible = sort(unique(spaceTime$temporalSubsamples))
  # Create an empty matrix to store summary data for possible Z-values:
    zMatrix = matrix(ncol = 3, nrow = length(zPossible), 
                   dimnames = list(NULL, c('z','nSiteTimes','propSites')))
  # Create an empty list of sites to store site names of good sites at a given Z-value:
    zSiteList = list(length = length(zPossible))
  # For loop to populate the zMatrix and zSite Lists:
    for(i in 1:length(zPossible)){
      # Subset spaceTime to subsamples greater than or equal to z for a given site:
        spaceTimeGTEz = spaceTime[spaceTime$temporalSubsamples >= zPossible[i], ]
      # Determine sites and siteTimes in which the temporal subsampling was greater 
      # than equal to z for at least the minimum time samples:
        yearCountBySiteGTEz = dplyr::count(spaceTimeGTEz, analysisSite)
        goodSites = yearCountBySiteGTEz$analysisSite[yearCountBySiteGTEz$n >= minNTime]
        goodSiteTimes = spaceTimeGTEz$siteTime[spaceTimeGTEz$analysisSite %in% goodSites]
      # Construct matrix of z values, the number and proportion of siteYears with that level of subsampling:
        zMatrix[i,'z'] = zPossible[i]
        zMatrix[i, 'nSiteTimes'] = length(goodSiteTimes)
        zMatrix[i, 'propSites'] = length(goodSiteTimes)/length(unique(spaceTime$siteTime))
      # List the names of goodSites for a given Z-value:
        zSiteList[[i]] = goodSiteTimes
      # Name each list entry by the Z-value
        names(zSiteList)[[i]] = zPossible[i]
      }
  # Make a dataframe
    zTable = data.frame(zMatrix)
  # Get the highest Z value with at least minNYears:
    z = max(zTable$z[zTable$propSites >= proportionalThreshold])  
  # Get the names of the site Times that satisfy Z:
    zSiteTimes = factor(zSiteList[[as.character(z)]])
  # Return the z value and site names 
    return(list (z = z, zSiteTimes = zSiteTimes, zTable = data.frame(zMatrix)))
  }

#------------------------------------------------------------------------------------------------------*
# ---- Subset data based on z-threshold ----
#------------------------------------------------------------------------------------------------------*

dataZSubFun  = function(inData, minNTime = 10, proportionalThreshold = .5){
  # Get z-values
    zOutput = zFinder(inData, minNTime, proportionalThreshold)
    z = zOutput[[1]]
  # Add a siteTime column:
    data = inData
    data$siteTime = paste(data$analysisSite, data$analysisDate, sep ='_')
  # Subset data to just the site-timeSamples that meet the z-threshold for temporal subsampling:  
    dataZSub = subset(data, siteTime %in% zOutput$zSiteTimes)
  # Add a column that concatenates siteTime and date:
    dataZSub$siteTimeDate = paste(dataZSub$siteTime, dataZSub$date, sep = '_')
  # For each siteID and time sample, sample z number of sub-sampling events:
    siteTimes = unique(dataZSub$siteTime)
    events = list(length = z*length(siteTimes))
  
    for(i in 1:length(siteTimes)){
      # Subset to a given siteYear:
        siteDateSub = subset(dataZSub, siteTime == siteTimes[i])
      # Get unique frame of siteYearDates
        siteDates = unique(siteDateSub$siteTimeDate)
      # Sample the events by the Z-value:
        siteTimeDateSample = sample(unique(siteDateSub$siteTimeDate), size = z)
        events[[i]] = subset(siteDateSub, siteTimeDate %in% siteTimeDateSample )
      }
  # Subset data to sampled events:
    dataZSub = rbind.fill(events)
    return(dataZSub)
  }

#------------------------------------------------------------------------------------------------------*
# ---- CALCULATE the W-threshold ----
#------------------------------------------------------------------------------------------------------*
# The W-threshold refers to the maximum number of spatial subsamples that provide a given proportion of siteYears.
# This returns a w-value and a list of siteDates that satisfy this value:
# Note: Prior to running the "wFinder", you must have already run the function "richnessYearSubsetFun".

wFinder = function(inData, minNTime = 10, proportionalThreshold = .5){
  # Get data subset by Z-value:
    dataZSub = dataZSubFun(inData, minNTime, proportionalThreshold)
  # Summarize number of spatial subsamples per siteTime :
    spaceTime = ddply(dataZSub, .(siteTimeDate), summarize, 
                      spatialSubsamples = length(unique(site)))
  # Determine the number of siteTimes present:
    nSiteTimeDates = nrow(spaceTime)
  # Get possible values for w:
    wPossible = sort(unique(spaceTime$spatialSubsamples))
  # Create an empty matrix to store summary data for possible W-values:
    wMatrix = matrix(ncol = 3, nrow = length(wPossible), 
                     dimnames = list(NULL, c('w','nSiteTimeDates','propSiteTimeDates')))
  # Create an empty list of sites to store site names of good sites at a given W-value:
    wSiteTimeDateList = list(length = length(wPossible))
  # For loop to populate the wMatrix and wSite Lists:
    for(i in 1:length(wPossible)){
      # Calculate the years in which the subsamplings was greater than equal to w for a given site:
        siteTimeDateGTEw = subset(spaceTime, spatialSubsamples>=wPossible[i])$siteTimeDate
      # Construct matrix of w values, the number and proportion of sites:
        wMatrix[i,'w'] = wPossible[i]
        wMatrix[i, 'nSiteTimeDates'] = length(siteTimeDateGTEw)
        wMatrix[i, 'propSiteTimeDates'] = length(siteTimeDateGTEw)/nrow(spaceTime)
      # List the names of siteYears for a given W-value:
        wSiteTimeDateList[[i]] = siteTimeDateGTEw
      # Name each list entry by the Z-value
        names(wSiteTimeDateList)[[i]] = wPossible[i]
    }
  # Get the highest W value that includes >= proportionalThreshold of siteYears:
    wFrame = data.frame(wMatrix)
    w = max(wFrame$w[wFrame$propSiteTimeDates >= proportionalThreshold])
  # Get the names of the siteYearDates that satisfy W:
    wSiteTimeDates = factor(wSiteTimeDateList[[as.character(w)]])
  # Return list of necessary items for the subset:
    outList = list(dataZSub, wSiteTimeDates, w)
      names(outList) = c('dataZSub', 'wSiteTimeDates', 'w')
    return(outList)
}

#------------------------------------------------------------------------------------------------------*
# ---- Subset the data based on w and z values ----
#------------------------------------------------------------------------------------------------------*

wzSubsetFun = function(inData, minNTime = 10, proportionalThreshold = .5){
  wOut = wFinder(inData, minNTime, proportionalThreshold)
  # Subset data
    dataW = subset(wOut$dataZSub, siteTimeDate %in% wOut$wSiteTimeDates) 
  # For each siteYearDate, sample w sampling events:
    siteTimeDateNames = unique(dataW$siteTimeDate)
    events = list(length = wOut$w*length(siteTimeDateNames))
    for(i in 1:length(siteTimeDateNames)){
      siteTimeDateSub = subset(dataW, siteTimeDate == siteTimeDateNames[i])
      UniqueSubsites = unique(siteTimeDateSub$site) 
      sampledSubsites = sample(UniqueSubsites, wOut$w, replace = F)
      events[[i]] = subset(siteTimeDateSub, site %in% sampledSubsites)
    }
    outSampledData = rbind.fill(events)
  # Keep only pertinent columns:
    outData = dplyr::select(outSampledData, one_of(c('analysisSite', 'analysisDate','species', 'count')))
      names(outData)[1:2] = c('site', 'year') 
  # Return the subsetted data frame:
    return(outData)
}

#------------------------------------------------------------------------------------------------------*
# ---- Function for getting the subsetted dataset ----
#------------------------------------------------------------------------------------------------------*
# The subsetted dataset is limited to sites above a minimum overall species richness and number of years and each site year is subset to w and z
# Prior to running this function, make sure to run the richnessYearSubsetFun, if there are no good sites, the proportional occurrence frame cannot be made!

subsetDataFun = function(dataset, datasetID, spatialGrain, temporalGrain,
                         minNTime = 10, minSpRich = 10,
                         proportionalThreshold = .5,
                         dataDescription){
  inData = richnessYearSubsetFun(dataset, spatialGrain, temporalGrain, minNTime, minSpRich, dataDescription)
  subsettedData = wzSubsetFun(inData, minNTime, proportionalThreshold)
  outData = data.frame(datasetID = datasetID, site = subsettedData$site, year = subsettedData$year,
                       species = subsettedData$species, count = subsettedData$count)
  return(outData)
}

#------------------------------------------------------------------------------------------------------*
# ---- Make the proportional occurrence frame ----
#------------------------------------------------------------------------------------------------------*

propOccFun = function(subsettedData){
    subsettedData1 = subset(subsettedData, count > 0)
    spTime = ddply(subsettedData1, .(datasetID, site, species), summarize, 
                   spTime = length(unique(year)))
    siteTime = ddply(subsettedData1, .(site), summarize, 
                     siteTime = length(unique(year)))
    spSiteTime = merge(spTime, siteTime)
    propOcc = data.frame(datasetID = datasetID, site = spSiteTime$site, 
                         species = spSiteTime$species,
                         propOcc = spSiteTime$spTime/spSiteTime$siteTime)
    return(propOcc)
  }

#------------------------------------------------------------------------------------------------------*
# The following function is used to create and explore and extract the species richness and number of time samples for a site.
#------------------------------------------------------------------------------------------------------*
# Note: because data are subset to w and z, some sites will no longer have a species richness or number of time samples greater than the decided upon minimum

siteSummaryFun = function(subsettedData){
  subsettedData1 = subset(subsettedData, count > 0)
  ddply(subsettedData1, .(datasetID, site), summarize, 
        spRich = length(unique(species)), 
        nTime = length(unique(year)),
        meanAbundance = sum(count)/length(unique(year)))
}

#------------------------------------------------------------------------------------------------------*
# Write files
#------------------------------------------------------------------------------------------------------*
# Note: This will not work if the temporal or spatial sampling is inadequate! Make sure to run richnessYearSubsetFun prior to to test whether the spatial and temporal scales are adequate!

writePropOccSiteSummary = function(subsettedData, spatialGrainAnalysis = FALSE){
  propOcc = propOccFun(subsettedData)
  siteSummary = siteSummaryFun(subsettedData)
  datasetID = unique(siteSummary$datasetID)
  if (!spatialGrainAnalysis) {
    write.csv(propOcc, 
              paste('data/propOcc_datasets/propOcc_', datasetID, '.csv', sep = ''), row.names = F)
    write.csv(siteSummary, 
              paste('data/siteSummaries/siteSummary_', datasetID, '.csv',  sep = ''), row.names = F)
  } else if (spatialGrainAnalysis) {
    write.csv(propOcc, 
              paste('data/spatialGrainAnalysis/propOcc_datasets/propOcc_', datasetID, '.csv', sep = ''), row.names = F)
    write.csv(siteSummary, 
              paste('data/spatialGrainAnalysis/siteSummaries/siteSummary_', datasetID, '.csv',  sep = ''), row.names = F)
  }
}


#######################################################################################################*
#######################################################################################################*
# ---- END DATA PREPARATION ----
#######################################################################################################*
#######################################################################################################*
# ---- BEGIN DATA ANALYSIS ----
#######################################################################################################*

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
  metaData = subset(read.csv('data_formatting_table.csv'),
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

# In these functions, propOcc refers to a vector of occupancy values for the
# species at a single site, and nTime is the number of time samples (typically
# years) as an integer.


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
  bi = bimodalityFun(occProp,nTime)
  if (bi != 0 & !is.na(bi))
  {occs  = occsScaledFun(occProp)
   shape.params = tryCatch( ############################# TRYCATCH
 
       {
         suppressWarnings(fitdistr(occs, "beta", list(shape1 = 2, shape2 = 2), lower = c(1e-10, 1e-10))) ###
       },
       error = function(cond) {
         message(paste("Error in fitdistr; trying new starting values")) ###
         tryCatch(
           {
             suppressWarnings(fitdistr(occs, "beta", list(shape1 = 3, shape2 = 3), lower = c(1e-10, 1e-10))) ###alternative starting params
           },
           error = function(cond) {
             list(estimate = c(NA, NA)) ############ FIX THIS
           },
           warning = function(cond) {
             message()
           })
       },
       warning = function(cond) {
         message(cond) ###
       }
     )
   ################### END EDITING #########
   return(as.vector(shape.params$estimate))
  } else c(NA, NA)
}

#======================================================================================================*
# ---- CORE-TRANSIENT MODE STATISTICS ----
#======================================================================================================*

# Proportion of samples that are core or transient:

# For these functions, mode argument takes either "core" or "transient".
# The threshold argument specifies the maximum occupancy to be considered
# transient, and therefore (1 - threshold) is the minimum occupancy to be 
# considered core.

modeProp = function(propOcc, mode, threshold) {
  if (mode == 'core') sum(propOcc >= 1 - threshold)/length(propOcc)
  else if (mode == 'transient') sum(propOcc <= threshold)/length(propOcc)
  else return(print('Invalid mode'))
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
#             Threshold is the value of occupancy below which a species is
#             considered transient, and therefore (1 - threshold) is the min
#             value for a species to be considered core.
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
  sites  = as.character(dataList$siteSummary$site)
  # Get summary stats for each site:         #where is the problem coming from?!
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
    propTrans_pVal = pModeFun(propOcc, nTime, 'transient', threshold, reps)
    meanAbundance = siteSummary$meanAbundance
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
                              meanAbundance, mu, bimodality, pBimodal, alpha, beta)
  }
  return(rbind.fill(outList))
}

#------------------------------------------------------------------------------------------------------*
# ---- MAKE SUMMARY STATS OF ANY NEW PROPOCC FILES ----
#======================================================================================================*
require(MASS)
require(plyr)

addNewSummariesFun = function(threshold, reps, write = FALSE, allNew = FALSE){
  if (allNew == FALSE & 
      file.exists('output/tabular_data/core-transient_summary.csv')) {
    currentSummaryData = read.csv('output/tabular_data/core-transient_summary.csv')
    currentDatasetIDs = unique(currentSummaryData$datasetID)
  } else {
    currentSummaryData = c()
    currentDatasetIDs = c()
  }
  propOcc_datasets = list.files('data/propOcc_datasets')
  # The following gets the integer values for the datasetID's from
  # "propOcc_##.csv" or "propOcc_###.csv":
  propOccDatasetIDs = read.table(text = 
                                   as.character(read.table(text = propOcc_datasets,
                                                           sep ='_')[,2]),sep ='.')[,1]
  # Find dataset IDs that are not yet summarized:
  newDatasetIDs = propOccDatasetIDs[!propOccDatasetIDs %in% currentDatasetIDs]
  # For loop to extract summary stats for new datasetIDs
  outList = list(length = length(newDatasetIDs))
  for(i in 1:length(newDatasetIDs)){
    outList[[i]] = summaryStatsFun(newDatasetIDs[i], threshold, reps)
  }
  newSummaryData = rbind.fill(outList)
  updatedSummaryData = rbind(currentSummaryData, newSummaryData)
  updatedSummaryData = updatedSummaryData[order(updatedSummaryData$datasetID),]
  if (write) {
    write.csv(updatedSummaryData, 
              'output/tabular_data/core-transient_summary.csv', row.names = F)
  }
  return(updatedSummaryData)
}

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
