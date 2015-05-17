# Formatting Dataset 124: OBIS Marine Inverts

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

ds = 124 

dataset = read.csv(paste('data/raw_datasets/dataset_', ds, '.csv', sep = ''))

dataFormattingTable = read.csv('data_formatting_table.csv')

#-------------------------------------------------------------------------------*
# ---- EXPLORE THE DATASET ----
#===============================================================================*

head(dataset)
str(dataset)
summary(dataset)
tail(dataset)

# Remove column called 'X'

dataset1 = dataset[,-1]
head(dataset1)

# Change ID column to 'datasetID'

names(dataset1)[1] = 'datasetID'

# Change name of site column

names(dataset1)[3] = 'site'

# change name of species column

names(dataset1)[4] = 'species'

head(dataset1)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE! 
# Are the ONLY site identifiers the latitude and longitude of the observation or 
# sample? (I.e., there are no site names or site IDs or other designations) Y/N

dataFormattingTable[,'LatLong_sites'] = 
  dataFormattingTableFieldUpdate(ds, 'LatLong_sites','Y') 

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SITE DATA ----
#===============================================================================*
# Explore

head(dataset1)
length(unique(dataset1$site))
summary(dataset1)

# Check all sites

levels(dataset1$site)

# Sites are listed with a name (USA_Massachusetts or USA_Massachusetts_North_America_Atlantic) followed by a lat_long. So just lat_long is the only relevant data in the site field

# BSE: Lat long fix

site = as.character(dataset1$site)

for(i in 1:length(site)){
  siteNoAlpha = gsub("[[:alpha:]]", " ", site[i])
  siteNoUnderscore = gsub('_',' ', siteNoAlpha)
  siteNoLeadingOrTrailingBlank = str_trim(siteNoUnderscore)
  siteUnderscore = gsub(' ', '_', siteNoLeadingOrTrailingBlank)
  siteTable = read.table(text = siteUnderscore, sep = '_')
  lon = as.character(siteTable[length(siteTable)])
  lat = as.character(siteTable[length(siteTable)-1])
  site[i] = paste(lat,lon, sep ='_')
}

dataset1$site = factor(site)

dataset2 = dataset1

# Check

head(dataset2, 30)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SITE DATA WERE MODIFIED!

# !DATA FORMATTING TABLE UPDATE! 

# Raw_siteUnit. 

dataFormattingTable[,'Raw_siteUnit'] = 
  dataFormattingTableFieldUpdate(ds, 'Raw_siteUnit','lat_long or site number and lat_long') 

# spatial_scale_variable. Is a site potentially nested (e.g., plot within a quad or decimal lat longs that could be scaled up)? Y/N

dataFormattingTable[,'spatial_scale_variable'] = 
  dataFormattingTableFieldUpdate(ds, 'spatial_scale_variable', 'N') 

# Notes_siteFormat.

dataFormattingTable[,'Notes_siteFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_siteFormat', "site fields were cut down using substring to delete the unused information which was present in every site. Remaining are the sites as just lat_long.")

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT SPECIES DATA ----
#===============================================================================*

# Explore

length(unique(dataset2$species))

# 654 unique species accounted for

head(dataset2)

# Upper and lower case could present an issue, so make new species column with all uppercase

dataset2$species = toupper(dataset2$species)
head(dataset2)
length(unique(dataset2$species))

# no change in number of uniques, so case wasn't an issue

# Check all species to look for removable data

class(dataset2$species)
dataset2$species = as.factor(dataset2$species)

levels(dataset2$species)

# Several species have repeats but followed by a typo of a symbol character (?) and a space. Can be treated as typo and removed from the dataset.  

# Trying functions seemed to work to remove these characters, but probably not the most efficient method. 
           
spTest = levels(dataset2$species)
spTest1 = str_trim(spTest)
head(spTest1)
spTest2 = gsub("?", "", spTest1)
head(spTest2)
length(unique(spTest))
length(unique(spTest2))

# Apply it to the dataset

dataset3 = dataset2
dataset3$species = str_trim(dataset3$species)
unique(dataset3$species)
  
  # This worked to remove the trailing space on these typo species

# Now to remove the symbol character (?)

dataset3$species = gsub("?", "", dataset3$species)

# Check to see if it worked

length(unique(dataset2$species))
length(unique(dataset3$species))

# Removed 35 typo species from the dataset
# Reset vectors

dataset3$species = as.factor(dataset3$species)

# Check all species again

levels(dataset3$species)
head(dataset3)

# BSE: There were some problems with punctuation

species = gsub('[[:punct:]]',' ', dataset3$species)
speciesTrim = str_trim(species)

dataset3$species = speciesTrim

# All good after check

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE SPECIES DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Column M. Notes_spFormat. Provide a THOROUGH description of any changes made
# to the species field, including why any species were removed.

dataFormattingTable[,'Notes_spFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_spFormat', "several species were removed because they were repeated in the dataset due to a symbol and extra space; treated as a typo. typos were removed from the dataset individually, first by the space then by the symbol. started with 654 uniques, now have 619.")

#-------------------------------------------------------------------------------*
# ---- EXPLORE AND FORMAT COUNT DATA ----
#===============================================================================*
# Assign countfield

countfield = "Abundance"

# Renaming it
names(dataset3)[which(names(dataset3) == countfield)] = 'count'

# remove zero counts and NA's:

summary(dataset3)
str(dataset3)
unique(dataset3$count)

# No zeros or NAs to remove

head(dataset3)

# set straight to dataset 5 to mirror template
dataset5 = dataset3

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE COUNT DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Possible values for countFormat field are density, cover, and count.
dataFormattingTable[,'countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'countFormat', 'count')

dataFormattingTable[,'Notes_countFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_countFormat', 'Data represents abundance. There were no NAs nor 0s that required removal')

#-------------------------------------------------------------------------------*
# ---- FORMAT TIME DATA ----
#===============================================================================*

# Set datefield and date format
head(dataset5)

datefield = 'Year'
dateformat = '%Y'

# Date is just a year, change to a true date object.

str(dataset5)

# Change to numeric format

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

# Assign the new date values in a field called 'date'
dataset6$date = date

head(dataset6)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATE DATA WERE MODIFIED!

#!DATA FORMATTING TABLE UPDATE!

# Notes_timeFormat. Provide a thorough description of any modifications that were made to the time field.

dataFormattingTable[,'Notes_timeFormat'] = 
  dataFormattingTableFieldUpdate(ds, 'Notes_timeFormat', 'temporal data provided in yearly grain. no changes made other than to date object')

# subannualTgrain. After exploring the time data, was this dataset sampled at a sub-annual temporal grain? Y/N

dataFormattingTable[,'subannualTgrain'] = 
  dataFormattingTableFieldUpdate(ds, 'subannualTgrain', 'N')

#-------------------------------------------------------------------------------*
# ---- MAKE DATA FRAME OF COUNT BY SITES, SPECIES, AND YEAR ----
#===============================================================================*
# datasetID already there

# make the compiled dataframe:

dataset7 = ddply(dataset6,.(datasetID, site, date, species),
                 summarize, count = max(count))

# Explore the data frame

dim(dataset7)

head(dataset7)

summary(dataset7)

# !GIT-ADD-COMMIT-PUSH AND DESCRIBE HOW THE DATA WERE MODIFIED!
#-------------------------------------------------------------------------------*
# ---- UPDATE THE DATA FORMATTING TABLE AND WRITE OUTPUT DATA FRAMES  ----
#===============================================================================*

# Update the data formatting table

dataFormattingTable = dataFormattingTableUpdate(ds)

# Take a final look at the dataset:

head(dataset7)

summary(dataset7)

# write formatted data frame

write.csv(dataset7, "data/formatted_datasets/dataset_124.csv", row.names = F)

# !GIT-ADD-COMMIT-PUSH THE FORMATTED DATASET IN THE DATA FILE, THEN GIT-ADD-COMMIT-PUSH THE UPDATED DATA FOLDER!

# Updating formatting table priorities and flag

dataFormattingTable[,'format_priority'] = 
  dataFormattingTableFieldUpdate(ds, 'format_priority','NA')


dataFormattingTable[,'format_flag'] = 
  dataFormattingTableFieldUpdate(ds, 'format_flag',1)

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
                                          minNYears = 5, minSpRich = 10)

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



