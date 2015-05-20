# Formatting dataset 129: OBIS Inverts

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

ds = 129 

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('Reference/data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

head(dataset)
summary(dataset)
str(dataset)

# Remove column 'X'

dataset1 = dataset[,-1]

# change names of ID, species, and site field

names(dataset1)[c(1,3,4)] = c('datasetID','site','species')
head(dataset1)

# All looks good

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites','N')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*

# Explore
length(unique(dataset1$site))
tail(dataset1)

# Sites are listed as USA_GulfOfMexico_sitenumber_lat_longs

levels(dataset1$site)

# 'USA_GulfOfMexico_sitenumber' is consistent for all sites. Remove the lat_longs to get just site numbers as site identifiers.

# Use read.table to separate the string by the underscores

site = as.character(dataset1$site)
siteTable = read.table(text = site, sep = "_")

# check the table

head(siteTable, 30)

# All good, now paste first 3 columns together

site = paste(siteTable$V1,siteTable$V2,siteTable$V3, sep = "_")

# Check new site field

unique(site)

# Add new site field to the dataset

dataset2 = dataset1
dataset2$site = site

# Double check

summary(dataset2)
head(dataset2,20)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit', 
                                 'USA_GulfOfMexico_sitenumber_lat_long') 

# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable', 'N')

# Notes_siteFormat. Use this field to THOROUGHLY describe any changes made to the site field during formatting.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat',  'all sites are listed as USA_GulfOfMexico_sitenumber_lat_longs. Using read.table and then paste, extracted just the USA_GulfOfMexico_sitenumber out and used that info for site determintation.  Lat_longs no longer included.')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*
# Look at the individual species present

length(unique(dataset2$species))
levels(dataset2$species) 

# Capitalize species names

dataset2$species = factor(toupper(dataset2$species))

# Look for bad spp

levels(dataset2$species)

# No bad species to remove

dataset3 = dataset2

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat. Provide a THOROUGH description of any changes made
# to the species field, including why any species were removed.

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', 'only change made to species was to upper case.  No bad species removed')

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*

# Explore 

names(dataset3)

# Name of field

countfield = "Abundance"

# Renaming it

names(dataset3)[which(names(dataset3) == countfield)] = 'count'

# Check for zeros and NAs

summary(dataset3)
length(unique(dataset3$count))

# No NAs or zeros to remove

dataset5 = dataset3

head(dataset5)
str(dataset5)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat',  'count')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', 'Data represents abundance counts. There were no NAs nor 0s that required removal')

#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*

# Name of the date field

datefield = "Year"

# Format of field

dateformat = '%Y'

# Date is in years, so make numeric

if (dateformat == '%Y' | dateformat == '%y') {
  date = as.numeric(as.character(dataset5[, datefield]))
} else {
  date = as.POSIXct(strptime(dataset5[, datefield], dateformat))
}

# Check and add to dataset

class(date)

head(dataset5[, datefield])

head(date)

dataset6 = dataset5

# Delete the old date field

dataset6 = dataset6[, -which(names(dataset6) == datefield)]

# Assign the new date values in a field called 'date'

dataset6$date = date

# Check the results

head(dataset6)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat. Provide a thorough description of any modifications that were made to the time field.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat','temporal data provided in years. The only modification to this field involved converting to a numeric object.')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain', 'N')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*

# DatasetID already included

# Make the compiled dataframe

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

write.csv(dataset7, "data/formatted_datasets/dataset_129.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-COMMIT-PUSH THE UPDATED DATA FOLDER!

# As we've now successfully created the formatted dataset, we will now update the format priority and format flag fields. 

dataFormattingTable[,'format_priority'] = 
  dataFormattingTableFieldUpdate(ds, 'format_priority','NA')

dataFormattingTable[,'format_flag'] = 
  dataFormattingTableFieldUpdate(ds, 'format_flag',  1)

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
length(unique(dataset$date))
head(dataset)

#### STOP! <10 YEARS OF DATA !!!! ####

# Get the data formatting table for that dataset:

dataFormattingTable = subset(read.csv("data_formatting_table.csv"),
                             dataset_ID == datasetID)

# Check relevant table values:

dataFormattingTable$LatLong_sites

dataFormattingTable$spatial_scale_variable

dataFormattingTable$Raw_siteUnit

dataFormattingTable$subannualTgrain

# Though sites are lat long, the number is embedded within a character string. This needs to be extracted:

site = dataset$site

siteFix = vector(length = length (site))

for(i in 1:length(site)){
  siteLL = unlist(strsplit(as.character(site[i]),split ='_'))
  siteFix[i] = paste(siteLL[length(siteLL)-1], siteLL[length(siteLL)], sep ='_')
}


# Put the underscore back between the sites:

dataset$site = siteFix

# We'll start with the function "richnessYearSubsetFun". This will subset the data to sites with an adequate number of years of sampling and species richness. If there are no adequate years, the function will return a custom error message.

richnessYearsTest = richnessYearSubsetFun(dataset, spatialGrain = .1, 
                                          temporalGrain = 'year', 
                                          minNTime = 10, minSpRich = 10)

head(richnessYearsTest)
dim(richnessYearsTest) ; dim(dataset)
length(unique(richnessYearsTest$analysisSite))

# All looks okay, so we'll now get the subsetted data (w and z and sites with adequate richness and time samples):

subsettedData = subsetDataFun(dataset, datasetID, spatialGrain = 2, temporalGrain = 'year',
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

                 