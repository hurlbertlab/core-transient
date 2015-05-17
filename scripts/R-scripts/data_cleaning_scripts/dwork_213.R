# Dataset 232: Sevilleta small mammals

# Load libraries:

library(plyr)

# Set read and write directories:

in_dir = 'raw_datasets'
out_dir = 'formatted_datasets'

# Get data:

d213 = read.csv(file.path(in_dir,'dataset_213.csv'))

# Year and plot data are in one string. Extract year as the last two characters:
# Note: The function below is necessary because nchar is not equivalent across
# plot years.

substrRight <- function(x, n){
  x = as.character(x)
  substr(x, nchar(x[1])-n+1, nchar(x[1]))
}

d213$year = as.numeric(paste('19',substrRight(d213$plotyear, 2), sep =''))

# Extract plot data (code removes the last two (year) characters):

d213$plot = factor(substr(as.character(d213$plotyear), 1, nchar(as.character(d213$plotyear))-2))

# Can't count "Bare ground" as a species ... remove it:

d213 = d213[d213$sp!='Bare ground',]

# Remove "Short grass" as as species:

d213 = d213[d213$species!='Short grass',]

# Create a data frame of the count of individuals for a given 
# location and sampling event (note, count column is basal area):

d213.1 = ddply(d213, .(plot,species,year), 
               summarise, count = max(area))

# Add a dataset ID column for matching with metadata

d213.1$datasetID = rep(213,1, length(d213.1[,1]))

# Rearrange the columns"

d213.1 = d213.1[,c(5,1:4)]

# Change site naming convention:

d213.1$plot = paste('d',d213.1$datasetID, '_',d213.1$plot, sep = '')

# Make names equivalent to other datasets:

names(d213.1) = c('datasetID','site','species','year','count')


# Write to file:

write.csv(d213.1, file.path(out_dir,'dataset_213.csv'), row.names = F)

# Remove objects from the global environment

rm(list = ls())

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

# Check table values:

dataFormattingTable

dataFormattingTable$LatLong_sites

dataFormattingTable$spatial_scale_variable

dataFormattingTable$Raw_siteUnit

dataFormattingTable$subannualTgrain

# We'll start with the function "richnessYearSubsetFun". This will subset the data to sites with an adequate number of years of sampling and species richness. If there are no adequate years, the function will return a custom error message.

richnessYearsTest = richnessYearSubsetFun(dataset, spatialGrain = 'Station_Replicate', temporalGrain = 'year', 
                                          minNTime = 10, minSpRich = 10)

head(richnessYearsTest)
dim(richnessYearsTest) ; dim(dataset)
length(unique(richnessYearsTest$analysisSite))

# All looks okay, so we'll now get the subsetted data (w and z and sites with adequate richness and time samples):

subsettedData = subsetDataFun(dataset, datasetID, spatialGrain = 'Station_Replicate', temporalGrain = 'year',
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

