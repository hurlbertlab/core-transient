# Formatting Dataset 120: OBIS Benthos

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

ds = 120 

list.files('data/raw_datasets')

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

head(dataset)
summary(dataset)

# Remove field labeled 'x'

dataset1 = dataset[,-1]

# Check 

head(dataset1)

# change ID to datasetID

names(dataset1)[1] = 'datasetID'

# change SampleID to site

names(dataset1)[3] = 'site'

head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites', 'N')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

length(unique(dataset1$site))
unique(dataset1$site)

# Sites give the region of site, number, and lat_longs

# Can remove the word 'REGION' from the site data

dataset2 = dataset1
dataset2$site = str_sub(dataset1$site, start = 8)

# Check over

head(dataset2)
tail(dataset2)

# All good

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', 'region_site_sitenumber_lat_long') 


# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable', 'N')

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', 'No sites removed, but used subtring to remove text REGION which was present on all data points.  Had hard time searching for metadata, and what I call site number may not be an actual site number since there are some as egative values')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Explore
head(dataset2)
length(unique(dataset2$Species))

# Change name
names(dataset2)[4] = 'species'

# Check all levels over for bad species

levels(dataset2$species)

# Set to dataset3

dataset3 = dataset2

head(dataset3)

# No bad species to remove

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat. Provide a THOROUGH description of any changes made
# to the species field, including why any species were removed.

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', 'No species removed, no changes')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*

# Make countfield and rename field

names(dataset3)
countfield = 'Abundance'

# Renaming it

names(dataset3)[which(names(dataset3) == countfield)] = 'count'
head(dataset3)

# Check for NAs or zeros

summary(dataset3)
unique(dataset3$count)

# No  zeros or NAs found in count field

dataset4 = na.omit(dataset3)

# none in whole dataset
# Set to dataset5 to keep with template

dataset5 = dataset4

head(dataset5)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat', 'count')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat','Data represents abundance count. There were no NAs nor 0s that required removal')


#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*

# field representing sampling date?
datefield = 'Year'

# Date is just given in year
# Make sure it is numeric

class(dataset5$Year)

dataset6 = dataset5

dataset6$Year = as.numeric(dataset6$Year)

# Rename field

names(dataset6)[2] = 'date'

# Check

head(dataset6)
str(dataset6)
summary(dataset6)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat. Provide a thorough description of any modifications that were made to the time field.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat', 'temporal data given yearly. Only change made was to numeric')

# subannualTgrain.

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain', 'N')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*

# Dataset ID already in dataset

# Now make the compiled dataframe:

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

# Update the data formatting table

dataFormattingTable = dataFormattingTableUpdate(ds, dataset7)

# Take a final look at the dataset:

head(dataset7)

summary (dataset7)

# If everything is looks okay we're ready to write formatted data frame:

write.csv(dataset7, "data/formatted_datasets/dataset_120.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-COMMIT-PUSH THE UPDATED DATA FOLDER!

# update the format priority and format flag fields

dataFormattingTable[,'format_priority'] = 
  dataFormattingTableFieldUpdate(ds, 'format_priority', 'NA')

dataFormattingTable[,'format_flag'] = 
  dataFormattingTableFieldUpdate(ds, 'format_flag', 1)

# update the data formatting table

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

# Are the number of time samples <10 across sites?

length(unique(dataset$date))

#### STOP. LESS THAN 10 YEARS ####

# Though sites are lat long, the number is embedded within a character string. This needs to be extracted:

site = dataset$site

siteFix = vector(length = length (site))

for(i in 1:length(site)){
  siteLL = unlist(strsplit(as.character(site[i]),split ='_'))
  siteFix[i] = paste(siteLL[length(siteLL)-1], siteLL[length(siteLL)], sep ='_')
}


# Re-assign the site column:

dataset$site = siteFix

# We'll start with the function "richnessYearSubsetFun". This will subset the data to sites with an adequate number of years of sampling and species richness. If there are no adequate years, the function will return a custom error message.

richnessYearsTest = richnessYearSubsetFun(dataset, spatialGrain = 50, 
                                          temporalGrain = 'year', 
                                          minNTime = 10, minSpRich = 10)


head(richnessYearsTest)
dim(richnessYearsTest) ; dim(dataset)
length(unique(richnessYearsTest$analysisSite))

# All looks okay, so we'll now get the subsetted data (w and z and sites with adequate richness and time samples):

subsettedData = subsetDataFun(dataset, datasetID, spatialGrain = 100, temporalGrain = 'year',
                              minNTime = 10, minSpRich = 10,
                              proportionalThreshold = .5)

# 

# Take a look at the propOcc:

head(propOccFun(subsettedData))

hist(propOccFun(subsettedData)$propOcc)

# Take a look at the site summary frame:

siteSummaryFun(subsettedData)

# If everything looks good, write the files:

writePropOccSiteSummary(subsettedData)

# Remove all objects except for functions from the environment:

rm(list = setdiff(ls(), lsf.str()))
