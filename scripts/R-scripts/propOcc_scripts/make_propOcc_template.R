################################################################################*
#  TEMPLATE TO MAKE THE PROPOCC AND SUMMARY TABLES
################################################################################*

# Source the core-transient functions and load required libraries and dataset:

library(stringr)
library(plyr)
library(dplyr)
library(tidyr)

source('scripts/R-scripts/core-transient_functions.R')

dataID = 236

# Get formatted dataset:

dataset = read.csv(paste("data/formatted_datasets/dataset_",
  dataID, ".csv", sep =''))

# Get the data formatting table for that dataset:

dataFormattingTable = subset(read.csv("data_formatting_table.csv"),
                             dataset_ID == dataID)

# Check table values:

dataFormattingTable

#===============================================================================*
# ---- MAKE PROPORTIONAL OCCUPANCY AND DATA SUMMARY FRAMES ----
#===============================================================================*
# We have now formatted the dataset to the finest possible spatial and temporal
# grain, removed bad species, and added the dataset ID. It's now to make some
# scale decisions and determine the proportional occupancies.

# We'll start with the function "richnessYearSubsetFun". This will subset the data to sites with an adequate number of years of sampling and species richness for specified spatial and temporal scales of analysis.

# temporalGrain must take on one of the following values:
#      day, week, biweek, month, bimonth, season, year

# spatialGrain should specify either
#  (1) the spatial resolution in decimal degrees if sites will be defined purely by lat-long boxes;
#  (2) the raw spatial grain of the data in the formatted dataset (specified by the Raw_siteUnit in the
#      dataFormattingTable); this is the default;
#  (3) a higher level grain using the dataset-specific labels concatenated by '_'. For example, if
#      the data were collected at 'sites', 'plots', and 'quads' (Raw_siteUnit = 'site_plot_quad') but
#      you wanted to analyze the data at the plot grain, you would specify 'site_plot'.

# If there are no adequate years, the function will return a custom error message.

# First take a look at the raw spatial grain of the formatted dataset
Raw_siteUnit = as.character(dataFormattingTable$Raw_siteUnit)
Raw_siteUnit

# Modify Raw_siteUnit as necessary if you want to use a larger/coarser spatial grain
#  - e.g.,  Raw_siteUnit = 'site_plot'
#    instead of 'site_plot_quad'

richnessYearsTest = richnessYearSubsetFun(dataset, 
                                          spatialGrain = Raw_siteUnit, 
                                          temporalGrain = 'year', 
                                          minNTime = 10, minSpRich = 10)


#-------------------------------------------------------------------------------*
# ---- SITE DATA ----
#===============================================================================*
# What is the appropriate sampling grain for sites? We'll explore the data formatting table to see if there are any clues

# How many sites are there?

length(unique(dataset$site))

# Could this dataset involve spatial sampling at a grain below that of the site?

dataFormattingTable$spatial_scale_variable

# If no, you can move to the temporal section. If yes, you need to determine whether site designations are lat-long or nested sampling groups.

dataFormattingTable$LatLong_sites

#-------------------------------------------------------------------------------*
# ---- SITE SCALE: NESTED SAMPLING CATEGORICAL GROUPS ----
#-------------------------------------------------------------------------------*
# Use this section IF sites are spacially nested but sites are not defined by lat-longs.
# If the spatial sampling grain is nested and site designations are not defined by lat-longs, it may be necessary to remove some observations and possibly extract samples from the data if the sampling is unevenly distributed. 

nestedDataset = getNestedDataset(dataset)

spatialGrains = nestedDataset[[2]]

# SUBSET DATASET TO SITES WITH ADEQUATE TIME SAMPLES AND RICHNESS:

DataSRTimeSub = RichnessYearSubsetFrame(temporalGrain = 'year', spatialGrain =  'site')

# CALCULATE the Z-threshold:

zThresh = zFinder(DataSRTimeSub, minNYears = 10, proportionalThreshold = .5)

# Calculate W based on Z and sample data:

dataListWZ = wzDataSubset(DataSRTimeSub, zThresh, minNYears = 10, proportionalThreshold = .5)

datasetSubWZ = dataListWZ[[1]]

# Make Prop_Occ:

propOcc = propOccFun(dataID, spatialGrain = 'site',temporalGrain = 'year')

write.csv(propOcc, 'data/propOcc_datasets/propOcc_d70.csv', row.names = F)

#######################################################################################################
#######################################################################################################
#######################################################################################################

# In this example, we will use the 3rd scale (siteID = site_block_treatment)

wzList[[1]]$wzMax

w = 36
z = 2


nestedDataset2 = select(nestedDataset[[1]], one_of('site_block_treatment','date','year','site','species','count'))

nestedDataset2$siteYear = paste(nestedDataset2$site_block_treatment, nestedDataset2$year, sep ='_')

# For each siteYear, calculate the number of spatial and temporal subsamples:

siteYearSubsampling = ddply(nestedDataset2, .(siteYear), summarize, 
                            subTime = length(unique(date)), 
                            subSpace = length(unique(site)))

# Remove siteYears that don't have at least w and z number of spaceTime subsampling:

siteYearSubsampling2 = filter(siteYearSubsampling, subTime >=z & subSpace>=w)

# For the nestedDataset, Keep siteYears in which subSampling was >= w and z:

nestedDataset3 = nestedDataset2[nestedDataset2$siteYear %in% siteYearSubsampling2$siteYear,]

# create a dataset of unique temporal and spatial samples for a given year:

subTimeSpaceData = distinct(select(nestedDataset3, one_of('siteYear','date','site')))

# Sample from each siteYear, a given date and site:

uniqueSiteYears = unique(subTimeSpaceData$siteYear)

outList = list(length = length(uniqueSiteYears))

for(i in 1:length(uniqueSiteYears)){
  sYsub = filter(subTimeSpaceData, siteYear == uniqueSiteYears[i])
  uniqueSites = unique(sYsub$site)
  uniqueDates = unique(sYsub$date)
  sites = sample(uniqueSites, size = w, replace = F)
  dates = sample(uniqueDates, size = z, replace = F)
  for(j in 1:length(sites)){
    sample(filter(sYsub, sYsub$site == sites[j]), size = z, replace = F)
  }
}

sYsubSiteDate  = paste(sites, dates, sep = '_') # Potential site date combinations of sampled values

sYsubTest = sYsub[paste(sYsub$site, sYsub$date, sep = '_') %in% sYsubSiteDate,] # sY sub that matches the potential site date combinations

dim(sYsubTest)

dim(sYsub)

length(unique(sYsub$site))

length(unique(sYsub$subSite))

length(unique(sYsub$date))




######################################################################

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



# A good first pass is to look at the number of years and species per site:

nestedSiteValiditySummary(dataset)

nestedSiteHist = function(dataset, i){
  siteUnit = paste(as.character(siteUnitTable[1,1:i]), collapse = '_')
  dataset$year = getYear(dataset$date)
  if (siteUnit == siteUnitTable[,1]) {
    dataset$site = siteTable[,1]} else {
      dataset$site = factor(apply(siteTable[,1:i], 1, paste, collapse = '_'))
    } 
  siteSummary = ddply(dataset, .(site), summarize,
                      timeSamples = length(unique(year)), 
                      nSpecies = length(unique(species)))
  hist(siteSummary$timeSamples, col = 'gray')
}

# Function to make the spatial grain more course for a dataset. 

dSafe = dataset

dataset = dSafe

rescaleNestedDataset = function(dataset, scale){
  siteUnit = paste(as.character(siteUnitTable[1,1:scale]), collapse = '_')
  if (siteUnit == siteUnitTable[,1]){
    dataset$site = factor(siteTable[,1]) } else {
      dataset$site = factor(apply(siteTable[,1:scale], 1, paste, collapse = '_'))
    }
  dataset = ddply(dataset, .(datasetID, site, date, species), summarize,
                  count = sum(count))
  return(dataset)
}

# Make sure ot have a sense of the site levels first!

head(dataset$site)

dataset1 = rescaleNestedDataset(dataset, 4)

# Now let's remove the sites with inadequate sample sites:

badSites = subset(ddply(dataset1, .(site), summarize,
                  timeSamples = length(unique(getYear(date))), 
                  nSpecies = length(unique(species))),
                  nSpecies < 10 | timeSamples < 5)$site

dataset2 = dataset1[!dataset1$site %in% badSites,]

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE ANY SPATIAL GRAIN DECISIONS!

# Note: In many instances, site definition will be spatially explicit (e.g., 
# lats and longs). When this is the case, we may need to summarize the data to
# a courser precision (few decimal places). We can do so by using the 
# "round_any" function in Hadley Wickham's plyr package, specifying "floor" 
# as the rounding function.

#-------------------------------------------------------------------------------*
# ---- TIME DATA ----
#===============================================================================*
# We start by extracting year from the dataset. Year will now be our DEFAULT
# temporal grain. Decisions for finer temporal grains may be decided at a 
# later date.

# Change date column to year:

dataset$date = getYear(dataset$date)

# Change column name:

names(dataset)[3] = 'year'


#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# And make our proportional occurence data frame:

write.csv(propOccFun(dataset), "data/propOcc_datasets/propOcc_223.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH propOcc!

# And make and write site summary dataset:

write.csv(siteSummaryFun(dataset), 'data/siteSummaries/siteSummary_223.csv', row.names = F)

# Note: Both the submodule and core-transient folder need to be pushed to Git, 
# in git bash:

# cd data
# git add formatted_datasets/dataset_208.csv
# git commit -m "added formatted dataset"
# git push
# cd ..
# git add data
# git commit -m "updated submodule with formatted dataset 208"
# git push

#-------------------------------------------------------------------------------*
# ---- EXPLORE YOUR DATASET SUMMARY INFO AND UPDATE THE DATA SOURCE TABLE  ----
#===============================================================================*

# !!!At this point, go to the data source table and provide:
#   -central lat and lon (if available, if so, LatLonFLAG = 0, if you couldn't do
#    it, add a flag of 1)
#   -spatial_grain columns (T through W)
#   -nRecs, nSites, nTime, nSpecies
#   -temporal_grain columns (AH to AK)
#   -Start and end year
#   -Any necessary notes
#   -flag any issues and put issue on github
#   -git-add-commit-push data_source_table.csv

dim(dataset)

length(unique(dataset$site))

length(unique(dataset$year))

length(unique(dataset$species))
