################################################################################*
#  TEMPLATE TO MAKE THE PROPOCC AND SUMMARY TABLES
################################################################################*

# Source the core-transient functions and load required libraries and dataset:

library(stringr)
library(plyr)

source('scripts/R-scripts/core-transient_functions.R')

dataset = read.csv("data/formatted_datasets/dataset_223.csv")

dataFormattingTable = read.csv("Reference/data_formatting_table.csv")

dataFormattingTable = subset(dataFormattingTable, dataset_ID == 223)

#===============================================================================*
# ---- MAKE PROPORTIONAL OCCUPANCY AND DATA SUMMARY FRAMES ----
#===============================================================================*
# We have now formatted the dataset to the finest possible spatial and temporal
# grain, removed bad species, and added the dataset ID. It's now to make some
# scale decisions and determine the proportional occupancies.

#-------------------------------------------------------------------------------*
# ---- SITE DATA ----
#===============================================================================*
# What is the appropriate sampling grain for sites? Return to the metadata to
# see if there's any clues.

##################################
# PUT THIS AT THE END OF THE DATA FORMATTING TEMPLATE:
# We want to address Sites before Time, but to address Sites we still need
# this 'year' field. Or we can just include it here.

# Add year column:

dataset$year = getYear(dataset$date)

####################################


# How many sites are there?

length(unique(dataset$site))

# How many species are recorded at each site? If too few (<10),
# then we may want to use a coarser spatial grain if possible.

siteTable = ddply(dataset, .(site), summarize,
                  nSpecies = length(unique(species)))
hist(siteTable$nSpecies)

# Could this dataset involve spatial sampling at a grain below that of the site?

dataFormattingTable$spatial_scale_variable

# If no, you can move to the next section. If yes, you need to determine whether site designations are lat-long or nested sampling groups.

dataFormattingTable$LatLong_sites

#-------------------------------------------------------------------------------*
# ---- SITE SCALE: NESTED SAMPLING GROUPS
#-------------------------------------------------------------------------------*

# A good first pass is to look at the number of years and species per site:

nestedSiteValidity = function(dataset, i){
  siteUnit = paste(as.character(siteUnitTable[1,1:i]), collapse = '_')
  if (siteUnit == factor(siteUnitTable[,1]){
    dataset$site = siteTable[,1] } else {
      dataset$site = factor(apply(siteTable[,1:i], 1, paste, collapse = '_'))
  } 
  siteSummary = ddply(dataset, .(site), summarize,
      timeSamples = length(unique(year)), 
      nSpecies = length(unique(species)))
      nSite = nrow(siteSummary)
  MEANnTime = mean(siteSummary$timeSamples)
  MINnTime = min(siteSummary$timeSamples)
  MAXnTime = max(siteSummary$timeSamples)
  nBadSiteTime = nrow(subset(siteSummary, timeSamples < 5))
  nBadSiteSpecies = nrow(subset(siteSummary, nSpecies < 10))
  nBadSites = nrow(subset(siteSummary, timeSamples < 5 | nSpecies < 10))
  propBadSiteTime = nBadSiteTime/nRecs
  propBadSiteSpecies = nBadSiteSpecies/nRecs
  propBadSites = nBadSiteSpecies/nRecs
  return(data.frame(siteUnit, nSite, 
                    MEANnTime, MINnTime, MAXnTime, nBadSiteTime,propBadSiteTime, 
                    nBadSiteSpecies,propBadSiteSpecies, nBadSites, propBadSites))
}
         
# For loop to calculate site validity across scales for nested sites:

siteUnit = dataFormattingTable$Raw_siteUnit

siteUnitTable = read.table(text = as.character(siteUnit), sep = '_', stringsAsFactors = F)

siteTable = read.table(text = as.character(dataset$site), sep = '_', stringsAsFactors = F)

outList = list(length = ncol(siteUnitTable))

for (i in 1:ncol(siteUnitTable)){
  outList[[i]] = nestedSiteValidity(dataset, i)
}

nestedSiteValiditySummary = rbind.fill(outList)

nestedSiteValiditySummary

# Function to make the spatial grain more course for a dataset. 

rescaleNestedDataset = function(dataset, scale){
  siteUnit = paste(as.character(siteUnitTable[1,1:scale]), collapse = '_')
  if (siteUnit == siteUnitTable[,1]){
    dataset$site = factor(siteTable[,1]) } else {
      dataset$site = factor(apply(siteTable[,1:i], 1, paste, collapse = '_'))
    }
  dataset = ddply(dataset, .(datasetID, site, date, species), summarize,
                  count = sum(count))
  return(dataset)
}

nestedSiteValidity = function(dataset, i){
  siteUnit = paste(as.character(siteUnitTable[1,1:i]), collapse = '_')
  if (siteUnit == siteUnitTable[,1]){
    dataset$site = siteTable[,1] } else {
      dataset$site = factor(apply(siteTable[,1:i], 1, paste, collapse = '_'))
    } 
  siteSummary = ddply(dataset, .(site), summarize,
                      timeSamples = length(unique(year)), 
                      nSpecies = length(unique(species)))
  nSite = nrow(siteSummary)### Stopped HERE ####


# Now let's remove the sites with inadequate sample sites:

badSites = subset(siteSummaryFun(dataset), spRich < 10 | nTime < 5)$site

dataset2 = dataset[!dataset$site %in% badSites,]



# All looks good, rename dataset:

dataset = dataset2

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

# Note: Both the submodule and core-transient folder need to be pushed to, 
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
