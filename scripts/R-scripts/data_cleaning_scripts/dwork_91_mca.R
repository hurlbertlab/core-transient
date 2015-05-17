# Formatting Dataset 91: EMODnet Birds

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

# Get data. First specify the dataset number ('ds') you are working with.

ds = 91

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

names(dataset)
head(dataset)
tail(dataset)
summary(dataset)

# Remove unused column 'X'

dataset1 = dataset[,-1]
head(dataset1)

# Rename SampleID as site, and ID as datasetID

names(dataset1)[c(1,3,4)] = c('datasetID','site','species')

# Check

head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites', 'Y')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# Explore

length(unique(dataset1$site))
summary(dataset1)

# Sites are all "BalticSea_RigaGulf_lat_long"

# Since lat_longs are only relevant data, substring the field to just the lat_longs

site = str_sub(dataset1$site, start = 20)

# Check

head(site)

# Add new site field to data

dataset2 = dataset1
dataset2$site = site

# Double check

head(dataset2)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. How a site is coded 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', 'Region_lat_long') 

# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable', 'N') 

# Notes_siteFormat.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', 'sites were listed as region_lat_longs.  Lat_longs were extracted from the strings using substringbecause it was the only useful data.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Explore

length(unique(dataset2$species))
head(dataset2,20)

# Uppercase and look through for bad species

dataset2$species = factor(toupper(dataset2$species))

levels(dataset2$species)

# No bad species found

head(dataset2)
dataset3 = dataset2

# all looks good

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat. 

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', 'No bad species found.  No changes made to field.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*

# Explore

names(dataset3)

# Fill in the original field name here

countfield = 'Abundance'

# Renaming it

names(dataset3)[which(names(dataset3) == countfield)] = 'count'
head(dataset3)

# Check for zeros and NAs

summary(dataset3)

# No zeros, remove NAs

dataset5 = na.omit(dataset3)

# No zeros or NAs in data

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat', 'count')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', "Data represents abundance counts. no changes or removals necessary")

#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*

# Date field names

names(dataset5)
datefield = 'Year'

# What format?

dateformat = '%Y'

# Date is in just year

if (dateformat == '%Y' | dateformat == '%y') {
  date = as.numeric(as.character(dataset5[, datefield]))
} else {
  date = as.POSIXct(strptime(dataset5[, datefield], dateformat))
}

# check

class(date)

# Check dataset

head(dataset5[, datefield])

head(date)

dataset6 = dataset5

# Delete the old date field

dataset6 = dataset6[, -which(names(dataset6) == datefield)]

# Add new date field

dataset6$date = date

# Check the results

head(dataset6)
str(dataset6)

# All good

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat. Provide a thorough description of any modifications that were made to the time field.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat','data provided as years. only modification to this field was converting to numeric object.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain','N')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*

# DatasetID already included

# Make compiled dataframe

summary(dataset6)

dataset7 = ddply(dataset6,.(datasetID, site, date, species),
                 summarize, count = max(count))

# Explore the data frame:

head(dataset7)

summary(dataset7)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#-------------------------------------------------------------------------------*
# ---- UPDATE THE DATA FORMATTING TABLE AND WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Update the data formatting table

dataFormattingTable = dataFormattingTableUpdate(ds, dataset7)

# Take a final look at the dataset:

head(dataset7)

summary(dataset7)

# Everything looks good, write dataset to file

write.csv(dataset7, "data/formatted_datasets/dataset_91.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-COMMIT-PUSH THE UPDATED DATA FOLDER!

# update the format priority and format flag fields. 

dataFormattingTable[,'format_priority'] = 
  dataFormattingTableFieldUpdate(ds, 'format_priority', 'NA')

dataFormattingTable[,'format_flag'] = 
  dataFormattingTableFieldUpdate(ds, 'format_flag', 1)

# And update the data formatting table:

write.csv(dataFormattingTable, 'Reference/data_formatting_table.csv', row.names = F)

# !GIT-ADD-COMMIT-PUSH THE DATA FORMATTING TABLE!

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
head(dataset)

# Get the data formatting table for that dataset:

dataFormattingTable = subset(read.csv("data_formatting_table.csv"),
                             dataset_ID == datasetID)

# Check table values:

dataFormattingTable

# We'll start with the function "richnessYearSubsetFun". This will subset the data to sites with an adequate number of years of sampling and species richness. If there are no adequate years, the function will return a custom error message.

richnessYearsTest = richnessYearSubsetFun(dataset, spatialGrain = 1, temporalGrain = 'year', 
                                          minNYears = 10, minSpRich = 10)

head(richnessYearsTest)
dim(richnessYearsTest) ; dim(dataset)
length(unique(richnessYearsTest$analysisSite))

# All looks okay, so we'll now get the subsetted data (w and z and sites with adequate richness and time samples):

subsettedData = subsetDataFun(dataset, datasetID, spatialGrain = .01, temporalGrain = 'year',
                              minNYears = 10,  minNTime = 10, minSpRich = 10,
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

