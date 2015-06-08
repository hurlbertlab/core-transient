# Cleaning dataset 173 OBIS Marine Inverts

#-------------------------------------------------------------------------------*
# ---- SET-UP ----
#===============================================================================*

# Load libraries:

library(stringr)
library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(MASS)

# Source the functions file:

getwd()
source('scripts/R-scripts/core-transient_functions.R')

# Get data

ds = 173

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*
# Explore

dim(dataset)
names(dataset)
str(dataset)
head(dataset)
tail(dataset)
summary(dataset)

# Remove unwanted columns
  # List removed fields

unusedFields = c(1)
dataset1 = dataset[,-unusedFields]

# Check

dim(dataset1)
head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites', 'N') 

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Make site column

dataset2 = dataset1

# check indiv sites
length(unique(dataset$SampleID))
levels(dataset$SampleID)

# just renamed only site column from "sampleID" to "site"

names(dataset2)[3] = "site"
head(dataset2)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', 'site_sitenumber')

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable','N')

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', 'no changes were made to raw data. sites are listed by name, and some have different number IDs.')


#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Look at each individual species to find ones to remove

levels(dataset2$Species)

# No questionable or bad species

# Change name of field

head(dataset2)
names(dataset2)[4] = "species"

# Check
names(dataset2)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat.

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', 'no changes made to species column.  Wide variety of taxonomic resolution, but there are no unidentified or bad species in data.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Explore

dataset3 = dataset2

# Make count column

countfield = 'Abundance'

# Renaming it

names(dataset3)[which(names(dataset3) == countfield)] = 'count'

# Remove zeros and NAs

summary(dataset3)

# Subset to records > 0:

dataset4 = subset(dataset3, count > 0) 
summary(dataset4)

# Remove NA's:

dataset5 = na.omit(dataset4)

# Check over 

head(dataset5)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat', 'count')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', 'Data represents count, originally labeled as abundance. There were no NAs nor 0s that required removal')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT TIME DATA ----
#===============================================================================*
# Extract the sampling dates. 

# name of the field
datefield = 'Year'

# What's the format

dateformat = '%Y'

# What is the format in which date data is recorded?
class(dataset5$Year)

# If the date is just a year, then make sure it is of class numeric
# and not a factor. Otherwise change to a true date object.

if (dateformat == '%Y' | dateformat == '%y') {
  date = as.numeric(as.character(dataset5[, datefield]))
} else {
  date = as.POSIXct(strptime(dataset5[, datefield], dateformat))
}

# Check
class(date)

head(dataset5[, datefield])

head(date)

dataset6 = dataset5

# Delete the old date field

dataset6 = dataset6[, -which(names(dataset6) == datefield)]
head(dataset6)

# Add new date field

dataset6$date = date

# Check the results:

head(dataset6)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat. Provide a thorough description of any modifications that were made to the time field.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat',  'temporal data provided as year of sample. The only modification to this field involved converting to a date object.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain', 'N')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# DatasetID already in dataset

names(dataset6)[1] = 'datasetID'

# compiled dataframe:

dataset7 = ddply(dataset6,.(datasetID, site, date, species),
                 summarize, count = max(count))

# Explore the data frame:

dim(dataset7)
head(dataset7)
summary(dataset7)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- UPDATE THE DATA FORMATTING TABLE AND WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Update the data formatting table:

dataFormattingTable = dataFormattingTableUpdate(ds, dataset7)

# Final look at the dataset:

head(dataset7, 20)
summary (dataset7)

# Write formatted data frame:

write.csv(dataset7, "data/formatted_datasets/dataset_173.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-COMMIT-PUSH THE UPDATED DATA FOLDER!

# Update the format priority and format flag fields. 

dataFormattingTable[,'format_priority'] = 
  dataFormattingTableFieldUpdate(ds, 'format_priority', 'NA')

dataFormattingTable[,'format_flag'] = 
  dataFormattingTableFieldUpdate(ds, 'format_flag', 1)

# update the data formatting table:

write.csv(dataFormattingTable, 'Reference/data_formatting_table.csv', row.names = F)

# !GIT-ADD-COMMIT-PUSH THE DATA FORMATTING TABLE!

###################################################################################*
# ---- END DATA FORMATTING. START PROPOCC AND DATA SUMMARY ----
###################################################################################*
# We have now formatted the dataset to the finest possible spatial and temporal grain, removed bad species, and added the dataset ID. It's now to make some scale decisions and determine the proportional occupancies.

# Load additional required libraries and dataset:

library(dplyr)
library(tidyr)

datasetID = 173

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

richnessYearsTest = richnessYearSubsetFun(dataset, spatialGrain = 'site', 
                                          temporalGrain = 'year', 
                                          minNTime = 10, minSpRich = 10)

# Need to reset the site definitions. start by removing the numbers

dataset1 = dataset
site = as.character(dataset$site)
site1 = gsub('[0-9]', '',site)

length(unique(site))
length(unique(site1))

dataset1$site = site1

# And try again:

richnessYearsTest = richnessYearSubsetFun(dataset1, spatialGrain = 'site', 
                                          temporalGrain = 'year', 
                                          minNTime = 10, minSpRich = 10)


head(richnessYearsTest)
dim(richnessYearsTest) ; dim(dataset)
length(unique(richnessYearsTest$analysisSite))

# That worked, so running the analysis:

subsettedData = subsetDataFun(dataset1, datasetID, spatialGrain = 2, temporalGrain = 'year',
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

