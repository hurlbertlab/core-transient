###################################################################################*
# CORE-TRANSIENT FUNCTIONS                                                         *
###################################################################################*
# This script contains all of the functions used in the analyses that summarize
# core-transient data by site (and across sites). It is divided in 3 parts:
#   1. Bimodality summary statistics
#   2. Summary output tables for a given site and across sites
#   3. Plot output

#==================================================================================*
# ---- GENERAL FUNCTIONS ----
#==================================================================================*

# Standard error:

se = function(x) sd(x)/sqrt(length(x))

#==================================================================================*
# ---- FUNCTIONS FOR DATA FORMATTING ----
#==================================================================================*

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

#==================================================================================*
# ---- FUNCTIONS for proportional occurrence and site summary data frames  ----
#==================================================================================*

#----------------------------------------------------------------------------------*
# Function to change date object to year:
#----------------------------------------------------------------------------------*

getYear = function(date){
  if (class(date)[1] == 'factor') date = as.POSIXlt(date)
  return(as.numeric(format(date, '%Y')))
}

#----------------------------------------------------------------------------------*
# The following function is used to create and explore and extract the species richness and number of time samples for a site.
#----------------------------------------------------------------------------------*


siteSummaryFun = function(dataset){
  ddply(dataset, .(datasetID, site), summarize, 
        spRich = length(unique(species)), 
        nTime = length(unique(year)))
}

#----------------------------------------------------------------------------------*
# The following functions are used to evaluate scale for temporally or spatially nested data
#----------------------------------------------------------------------------------*

# Nested site dataset (nesting is categorical, not lat-long):

getNestedSiteDataset = function(dataset, i){
  siteTable = read.table(text = as.character(dataset$site), sep = '_', stringsAsFactors = F)
  siteDefinition = dataFormattingTable$Raw_siteUnit
  siteUnitTable = read.table(text = as.character(siteDefinition), sep = '_', stringsAsFactors = F)
  outList = list(length = ncol(siteTable))
  for (i in 1:ncol(siteTable)){
    siteUnit = paste(as.character(siteUnitTable[1,1:i]), collapse = '_')
    if (siteUnit == siteUnitTable[,1]) {
      site = data.frame(siteTable[,1])} else {
        site = data.frame(factor(apply(siteTable[,1:i], 1, paste, collapse = '_')))
      } 
    names(site) = siteUnit
    if(names(site) == 'site') names(site) = 'site1'
    outList[[i]] = site
  }
  siteFrame = do.call(cbind, outList)
  nestedSiteData = cbind(dataset, siteFrame)
  return(list(nestedSiteData, names(siteFrame)))
}

# Nested time dataset (spatial nesting is categorical, not lat-long):

getNestedTimeDataset = function(dataset){
  if(dataFormattingTable$spatial_scale_variable == 'Y') {
    nestedSiteData = getNestedSiteDataset(dataset)
    nestedSiteLevels = nestedSiteData[[2]]
    nestedSiteDataset = nestedSiteData[[1]]}
  nestedSiteDataset$date = as.POSIXct(strptime(dataset$date, '%Y-%m-%d'))
  nestedSiteDataset$year = as.numeric(format(nestedSiteDataset$date, '%Y'))
  day = as.numeric(strftime(nestedSiteDataset$date, format = '%j'))
  week = trunc(day/7)+1
  biweek = trunc(week/2)+1
  month = as.numeric(format(nestedSiteDataset$date, '%m'))
  bimonth = trunc(month/2)+1
  season = ifelse(day < 80 |day >= 356, 1,
                  ifelse(day >= 80 & day < 172, 2,
                         ifelse(day >= 172 & day < 266, 3, 4)))
  subYearList = list(week, biweek, month, bimonth,season)
  names(subYearList) = c('week','biweek','month','bimonth','season')
  outList = list(length = length(subYearList))
  for(i in 1:length(subYearList)){
    outFrame = data.frame(paste(nestedSiteDataset$year, subYearList[[i]], sep = '_'))
    names(outFrame)  = paste('year', names(subYearList)[i], sep = '_')
    outList[[i]] = outFrame
  }
  subYearFrame = do.call(cbind, outList)
  return(list(cbind(nestedSiteDataset, subYearFrame), nestedSiteLevels))
}

# Wrapper function (spatial nesting is categorical, not lat-long):

getNestedDataset = function(dataset){
  if(dataFormattingTable$subannualTgrain == 'Y'){
    dataset = getNestedTimeDataset(dataset)
  } else {if(dataFormattingTable$spatial_scale_variable == T &
               dataFormattingTable$LatLong_sites != 'Y'){
    dataset = getNestedSiteDataset(dataset)
  }}
  return(dataset)
}

#----------------------------------------------------------------------------------*
# Function to evaluate spatial and temporal sampling grain:
#----------------------------------------------------------------------------------*

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

#----------------------------------------------------------------------------------*
# The following function writes the proportional occurence data frame on sites.
#----------------------------------------------------------------------------------*

propOccFun = function(dataset){
  spTime = ddply(dataset, .(datasetID, site, species), summarize, 
                 spTime = length(unique(year)))
  siteTime = ddply(dataset, .(datasetID, site), summarize, 
                   siteTime = length(unique(year)))
  propOcc = merge(spTime, siteTime)
  propOcc$propOcc = propOcc$spTime / propOcc$siteTime
  return(propOcc[,-c(4:5)])
}

#----------------------------------------------------------------------------------*
# ---- FUNCTIONS for proportional occurrence and site summary data frames  ----
#==================================================================================*

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


#==================================================================================*
# ---- GET DATA ----
#==================================================================================*

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

#==================================================================================*
# ---- BIMODALILITY ----
#==================================================================================*
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
#----------------------------------------------------------------------------------*
# ---- Function for calculating bimodality ----
#==================================================================================*
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

#----------------------------------------------------------------------------------*
# ---- Function for fitting the beta distribution ----
#==================================================================================*
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

#==================================================================================*
# ---- CORE-TRANSIENT MODE STATISTICS ----
#==================================================================================*

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

#==================================================================================*
# ---- DATASET SUMMARY FUNCTIONS ----
#==================================================================================*
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
#----------------------------------------------------------------------------------*
# ---- Function to generate summary of sampling ----
#==================================================================================*

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

#----------------------------------------------------------------------------------*
# ---- MAKE SUMMARY STATS OF ANY NEW PROPOCC FILES ----
#==================================================================================*

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

#==================================================================================*
# ---- PLOT FUNCTIONS ----
#==================================================================================*
# NOTE: For these functions to run, occProp, Ntime, and outSummary frames must
# already be loaded!

#----------------------------------------------------------------------------------*
# ---- Custom themes ----
#==================================================================================*

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

#----------------------------------------------------------------------------------*
# ---- Function to make core-transient histogram  ----
#==================================================================================*
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
