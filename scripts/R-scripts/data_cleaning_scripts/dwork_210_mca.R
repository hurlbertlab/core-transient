# Formatting dataset 210: Cedar Creek LTER Plants

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# Load libraries:

library(stringr)
library(plyr)

# Source the functions file:
source('scripts/R-scripts/core-transient_functions.R')

# Get data:

getwd()

list.files('data/raw_datasets')

dataset = read.csv('data/raw_datasets/dataset_210.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

head(dataset)
dim(dataset)
str(dataset)

# Remove columns not needed
dataset1 = dataset[,-c(1,2,6,7,8,9)]
head(dataset1)
summary(dataset1)

# Revert back to dataset
dataset = dataset1

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# 2 different fields associated with site data: "field" and "plot"
# Explore
class(dataset$field)
class(dataset$plot)
unique(dataset$field)
unique(dataset$plot)

# After checking for data to remove, there is none, so no removals
# Change plot to factor
dataset1 = dataset
dataset1$plot = factor(dataset1$plot)
levels(dataset$plot)

# Concatenate the two site columns to create new 'site' column
dataset1$site = paste(dataset1$field, dataset1$plot, sep = '_')
head(dataset1)
unique(dataset1$site)

# All looks goodataset, so remove old site columns and change back to dataset
dataset = dataset1[,-c(2,3)]
head(dataset)

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# explore
class(dataset$species)
length(unique(dataset$species))
#  259 unique species names

# Capitalize all to check for any case errors
sp = toupper(dataset$species)
length(unique(sp))

  # No case errors, so use original species column

# Look through unique species to find unwanted species names
levels(dataset$species)

# Several species names to be removed
badsp = c("Miscellaneous forb", "Miscellaneous grasses", "Miscellaneous grasses 2", "Miscellaneous herb", 
          "Miscellaneous herbs", "Miscellaneous legumes", "Miscellaneous litter", "Miscellaneous rushes", 
          "Miscellaneous sedges", "Miscellaneous sp.", "Miscellaneous woody plants","Forb seedlings","Mosses & lichens",
          "Pine needles")

# Remove species
dataset1 = dataset[!dataset$species %in% badsp,]
length(unique(dataset1$species))
unique(dataset1$species)

# Check nrows
nrow(dataset)
nrow(dataset1)

dataset = dataset1

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Explore
head(dataset)
  # Time listed by year
class(dataset$year)

  # Change from integer to factor
dataset$year = factor(dataset$year)
class(dataset$year)
head(dataset)
unique(dataset$year)

# Change name to 'date'
names(dataset)[1] = 'date'


#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Explore
length(unique(dataset$biomass))
str(dataset)
summary(dataset$biomass)

# Subset to remove zeros 
dataset1 = subset(dataset, biomass > 0)
dim(dataset1)
dim(dataset)

# Remove na's
dataset1 = na.omit(dataset1)
dim(dataset1)

summary(dataset1$biomass)

# Change name of column from biomass to count
names(dataset1)
names(dataset1)[3] = 'count'
head(dataset1, 20)

# Revert back to dataset
dataset = dataset1

summary(dataset)

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# Add dataset datasetID column
dataset$datasetID = rep(210, nrow(dataset))
head(dataset)

# check class of each row
str(dataset)

# Make the compiled dataframe
dataset1 = ddply(dataset, .(datasetID, site, date, species), summarize, count = max(count))
summary(dataset1)
head(dataset1, 50)

dataset = dataset1
#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*
# All looks good so write csv to data submodule

write.csv(dataset, 'data/formatted_datasets/dataset_210.csv', row.names = F)


################################################################################*
# ---- END CREATION OF FORMATTED DATA FRAME ----
################################################################################*

library(stringr)
library(plyr)

source('scripts/R-scripts/core-transient_functions.R')

dataset = read.csv("data/formatted_datasets/dataset_210.csv")

#===============================================================================*
# ---- MAKE PROPORTIONAL OCCUPANCY AND DATA SUMMARY FRAMES ----
#===============================================================================*

#-------------------------------------------------------------------------------*
# ---- TIME DATA ----
#===============================================================================*
# Explore again
class(dataset$date)
summary(dataset$date)
unique(dataset$date)

# Temporal scale is yearly, so no changes need to be made

# Change to 'year'
names(dataset)[3] = 'year'

#-------------------------------------------------------------------------------*
# ---- SITE DATA ----
#===============================================================================*
# Explore
length(unique(dataset$site))
summary(dataset$site)
head(dataset, 30)

# See how many time and species records per site
siteTable = ddply(dataset, .(site), summarize,
                  nYear = length(unique(year)),
                  nSp = length(unique(species)))

# Explore siteTable to find bad sites
head(siteTable)

head(siteTable[order(siteTable$nSp),],20)

# sufficient richness per site (lowest is 13)

# now check for number of time records
head(siteTable[order(siteTable$nYear),], 10)

  # Sufficient time samples per site

# datasetouble check for bad sites
badSites = subset(siteSummaryFun(dataset), spRich < 10 | nTime < 5)$site
length(badSites)
  # Length of badsites is 0, so no bad sites to remove

# Nothing changed, but rewrite the cleanedataset dataframe
dataset1 = ddply(dataset, .(datasetID, site, year, species), summarize, count = max(count))

head(dataset1)
summary(dataset1)

dataset = dataset1

#-------------------------------------------------------------------------------*
# ---- WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Make proportional occurence data frame:

write.csv(propOccFun(dataset), "data/propOcc_datasets/propOcc_210.csv", row.names = F)

# write site summary dataset:

write.csv(siteSummaryFun(dataset), 'data/siteSummaries/siteSummary_210.csv', row.names = F)

# Committed and pushed to both data submodule and core-trans git folder

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