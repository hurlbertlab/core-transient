# Eastern Wood bird community data

# Load libraries:

  library(reshape)

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

# Get data: 

d226 = read.csv(file.path(in_dir,'dataset_226.csv'))

# Melt from wide to long format:

dClean = melt(d226, id.vars = 'Species')

# Set column names:

names(dClean) = c('species','year','count')
  
# Remove X's from the year column and convert year to numeric:
  
dClean$year = as.numeric(gsub('X', '',dClean$year))
  
# Add a site column:

dClean$site = factor(rep('d226_ew', length(dClean$year)))

# Add a dataset ID column for matching with metadata
  
dClean$datasetID = rep(226, length(dClean[,1]))
    
# Rearrange the columns"
  
dClean = dClean[,c(5,4,1:3)]
  
# Remove NA's:

dClean = na.omit(dClean)

# Remove 0's:

d226 = dClean[dClean$count>0,]

# Not all species were samples in 1949, so these records need to be excluded:

d226 = d226[d226$year!=1949,]

# Write to the formatted data folder:

write.csv(d226, file.path(out_dir,'dataset_226.csv'), row.names = F)

###################################################################################*
# ---- END DATA FORMATTING. START PROPOCC AND DATA SUMMARY ----
###################################################################################*
# We have now formatted the dataset to the finest possible spatial and temporal grain, removed bad species, and added the dataset ID. It's now to make some scale decisions and determine the proportional occupancies.

# Load additional required libraries and dataset:

library(dplyr)
library(tidyr)

datasetID = ds

# Get formatted dataset:

dataset = read.csv(paste("data/formatted_datasets/dataset_",
                         datasetID, ".csv", sep =''))

# Have a look at the dimensions of the dataset and number of sites:

dim(dataset)
length(unique(dataset$site))
length(unique(dataset$date))
head(dataset)

# Get the data formatting table for that dataset:

dataFormattingTable = subset(read.csv("data_formatting_table.csv"),
                             dataset_ID == datasetID)

# Check relevant table values:

dataFormattingTable$LatLong_sites

dataFormattingTable$spatial_scale_variable

dataFormattingTable$Raw_siteUnit

dataFormattingTable$subannualTgrain

# We'll start with the function "richnessYearSubsetFun". This will subset the data to sites with an adequate number of years of sampling and species richness. If there are no adequate years, the function will return a custom error message.

richnessYearsTest = richnessYearSubsetFun(dataset, spatialGrain = 'site_block_plot', 
                                          temporalGrain = 'year', 
                                          minNTime = 10, minSpRich = 10)

head(richnessYearsTest)
dim(richnessYearsTest) ; dim(dataset)
length(unique(richnessYearsTest$analysisSite))

# All looks okay, so we'll now get the subsetted data (w and z and sites with adequate richness and time samples):

subsettedData = subsetDataFun(dataset, datasetID, spatialGrain = 'site_block_plot', temporalGrain = 'year',
                              minNTime = 10, minSpRich = 10,
                              proportionalThreshold = .5)

# Take a look at the propOcc:

head(propOccFun(subsettedData))

hist(propOccFun(subsettedData)$propOcc)

# Take a look at the site summary frame:

siteSummaryFun(subsettedData)

# If everything looks good, write the files:

writePropOccSiteSummary(subsettedData)

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))
